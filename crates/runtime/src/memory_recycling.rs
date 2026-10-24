/// Also known as "garbage collection" but cuter name
/// Memory is reused/recycled, not thrown away
use std::alloc::{alloc, dealloc, Layout};
use std::cmp;
use std::mem::{align_of, size_of};
use std::ptr::NonNull;

use crate::errors::AllocError;

/// How much the collection threshold increases each growth
const GC_THRESHOLD_FACTOR: usize = 2;

const INITIAL_THRESHOLD: usize = 1024 * 1024;

pub struct Heap {
    head: Option<NonNull<GcHeader>>,
    allocated_bytes: usize,
    next_gc_threshold: usize,
    automatic_collection_enabled: bool,
}

impl Default for Heap {
    fn default() -> Self {
        Self {
            head: None,
            allocated_bytes: 0,
            next_gc_threshold: INITIAL_THRESHOLD,
            automatic_collection_enabled: true,
        }
    }
}

// SAFETY: runtime behavior is intentionally stop-the-world and operated through
// a global `Mutex`. Collection is stop-the-world
// The heap’s intrusive pointers are only mutated under external synchronization.
unsafe impl Send for Heap {}

impl Heap {
    pub(crate) fn disable_automatic_collection(&mut self) {
        self.automatic_collection_enabled = false;
    }

    pub(crate) fn collection_due_for(&self, size: usize) -> bool {
        self.allocated_bytes
            .checked_add(size)
            .is_none_or(|total| total > self.next_gc_threshold)
    }

    #[cfg(any(test, feature = "gc-stress"))]
    pub(crate) fn set_collection_threshold_for_test(&mut self, threshold: usize) {
        self.next_gc_threshold = threshold;
    }

    #[cfg(any(test, debug_assertions, feature = "gc-stress"))]
    pub fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    #[cfg(any(test, debug_assertions, feature = "gc-stress"))]
    pub fn contains_address(&self, address: usize) -> bool {
        let mut current = self.head;
        while let Some(header) = current {
            if header.as_ptr() as usize == address {
                return true;
            }
            // SAFETY: every entry in the heap's linked list points to an owned header.
            current = unsafe { (*header.as_ptr()).next };
        }
        false
    }

    /// Allocates an object header and initializes every traced field to null.
    /// Callers must initialize all other fields before the object can become
    /// reachable or another collection can run. In particular, a zeroed root
    /// word must not be read as a Rust `GcRef` (which is non-null).
    pub fn alloc_raw(
        &mut self,
        size: usize,
        descriptor: &'static TypeDescriptor,
        roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        if size > u32::MAX as usize {
            return Err(AllocError::InvalidSize);
        }
        if size < size_of::<GcHeader>() {
            return Err(AllocError::InvalidSize);
        }
        // Reject invalid trace layouts before collection or allocation.
        match descriptor.trace {
            TraceDescriptor::None => {}
            TraceDescriptor::Fields(offsets) => {
                if !valid_trace_offsets(offsets, size) {
                    return Err(AllocError::InvalidSize);
                }
            }
            TraceDescriptor::TaggedFields {
                tag_offset,
                variants,
            } => {
                if !valid_trace_offsets(std::slice::from_ref(&tag_offset), size)
                    || variants
                        .iter()
                        .any(|(_, offsets)| !valid_trace_offsets(offsets, size))
                {
                    return Err(AllocError::InvalidSize);
                }
            }
        }

        if self.automatic_collection_enabled && self.collection_due_for(size) {
            self.collect(roots);
        }

        let new_total = self
            .allocated_bytes
            .checked_add(size)
            .ok_or(AllocError::OutOfMemory)?;
        let layout = layout_for_size(size);
        // SAFETY: layout is valid and the allocator returns either a suitably
        // aligned allocation or null. No field is read before initialization.
        let ptr = NonNull::new(unsafe { alloc(layout) }.cast::<GcHeader>())
            .ok_or(AllocError::OutOfMemory)?;
        let object = GcRef(ptr);

        unsafe {
            ptr.as_ptr().write(GcHeader {
                marked: false,
                size_bytes: size as u32,
                next: self.head,
                descriptor,
            });
        }

        match descriptor.trace {
            TraceDescriptor::None => {}
            TraceDescriptor::Fields(offsets) => {
                for &offset in offsets {
                    // SAFETY: offsets were validated above; these words are
                    // within the owned allocation and have GC-pointer alignment.
                    unsafe {
                        ptr.as_ptr()
                            .cast::<u8>()
                            .add(offset as usize)
                            .cast::<*mut GcHeader>()
                            .write(std::ptr::null_mut());
                    }
                }
            }
            TraceDescriptor::TaggedFields { variants, .. } => {
                for (_, offsets) in variants {
                    for &offset in *offsets {
                        // SAFETY: offsets were validated above.
                        unsafe {
                            ptr.as_ptr()
                                .cast::<u8>()
                                .add(offset as usize)
                                .cast::<*mut GcHeader>()
                                .write(std::ptr::null_mut());
                        }
                    }
                }
            }
        }

        self.head = Some(ptr);
        self.allocated_bytes = new_total;

        if self.allocated_bytes > self.next_gc_threshold {
            self.next_gc_threshold = cmp::max(
                self.next_gc_threshold.saturating_mul(GC_THRESHOLD_FACTOR),
                self.allocated_bytes,
            );
        }

        Ok(object)
    }

    /// Collects objects not reachable from `roots`.
    ///
    /// The caller must provide all live GC roots and ensure that
    /// no Hopper thread is running unsuspended.
    /// Holding the global runtime mutex alone does not suspend JIT execution;
    /// the execution gate does not coordinate arbitrary Rust calls to `collect`.
    /// The global heap is private to `Runtime`: `collect_idle` coordinates
    /// collection with the execution gate and includes registered native roots.
    /// The global runtime's threshold policy collects through `Runtime`, not
    /// through this standalone-heap method.
    pub fn collect(&mut self, roots: &mut dyn RootSet) {
        let mut marker = Marker {
            pending: Vec::new(),
        };
        roots.visit_roots(&mut marker);
        marker.drain();
        self.sweep();

        if self.allocated_bytes >= self.next_gc_threshold / GC_THRESHOLD_FACTOR {
            self.next_gc_threshold = cmp::max(
                self.allocated_bytes.saturating_mul(GC_THRESHOLD_FACTOR),
                INITIAL_THRESHOLD,
            );
        } else {
            self.next_gc_threshold = INITIAL_THRESHOLD;
        }
    }

    fn sweep(&mut self) {
        let mut current = &mut self.head;

        while let Some(mut node) = *current {
            let header = unsafe { node.as_mut() };
            if header.marked {
                header.marked = false;
                current = &mut header.next;
                continue;
            }

            let next = header.next;
            let size = header.size_bytes as usize;
            self.allocated_bytes -= size;

            unsafe {
                (header.descriptor.destroy)(node.as_ptr());
                dealloc(node.as_ptr().cast::<u8>(), layout_for_size(size));
            }
            *current = next;
        }
    }
}

struct Marker {
    pending: Vec<NonNull<GcHeader>>,
}

impl Marker {
    fn mark_slot(&mut self, root: *mut GcRef) {
        if root.is_null() {
            return;
        }
        // Traced fields and not-yet-initialized stack slots may contain null.
        // Read the pointer representation rather than a non-null Rust GcRef.
        let address = unsafe { root.cast::<*mut GcHeader>().read() };
        if let Some(header) = NonNull::new(address) {
            // SAFETY: root slots refer to live GC allocations while collection
            // is stopped; the marker alone mutates the header's mark bit.
            unsafe {
                if !(*header.as_ptr()).marked {
                    (*header.as_ptr()).marked = true;
                    self.pending.push(header);
                }
            }
        }
    }

    fn drain(&mut self) {
        while let Some(header) = self.pending.pop() {
            // SAFETY: every queued header was marked while still allocated.
            // Field offsets were checked when the object was allocated.
            unsafe {
                match (*header.as_ptr()).descriptor.trace {
                    TraceDescriptor::None => {}
                    TraceDescriptor::Fields(offsets) => {
                        for &offset in offsets {
                            let field = header.as_ptr().cast::<u8>().add(offset as usize).cast();
                            self.mark_slot(field);
                        }
                    }
                    TraceDescriptor::TaggedFields {
                        tag_offset,
                        variants,
                    } => {
                        let tag = header
                            .as_ptr()
                            .cast::<u8>()
                            .add(tag_offset as usize)
                            .cast::<usize>()
                            .read();
                        if let Some((_, offsets)) =
                            variants.iter().find(|(variant, _)| *variant as usize == tag)
                        {
                            for &offset in *offsets {
                                let field = header.as_ptr().cast::<u8>().add(offset as usize).cast();
                                self.mark_slot(field);
                            }
                        }
                    }
                }
            }
        }
    }
}

impl RootVisitor for Marker {
    fn visit_gc_ref(&mut self, root: *mut GcRef) {
        self.mark_slot(root);
    }
}

pub trait RootVisitor {
    fn visit_gc_ref(&mut self, root: *mut GcRef);
}

pub trait RootSet {
    fn visit_roots(&mut self, visitor: &mut dyn RootVisitor);
}

#[allow(dead_code)]
#[derive(Default)]
pub struct EmptyRootSet;

impl RootSet for EmptyRootSet {
    fn visit_roots(&mut self, _visitor: &mut dyn RootVisitor) {}
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct GcRef(NonNull<GcHeader>);

impl GcRef {
    pub fn new(ptr: NonNull<GcHeader>) -> Self {
        Self(ptr)
    }

    pub fn as_ptr(self) -> *mut GcHeader {
        self.0.as_ptr()
    }

    pub unsafe fn cast<T>(self) -> *mut T {
        self.as_ptr().cast::<T>()
    }
}

#[repr(C)]
pub struct GcHeader {
    marked: bool,
    size_bytes: u32,
    next: Option<NonNull<GcHeader>>,
    descriptor: &'static TypeDescriptor,
}

/// Metadata for debugging/diagnostics
///
/// This shouldn't be used for collection/destruction,
/// but we'll keep it for now for potential future use
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjectKind {
    String,
    Array,
    Record,
    Closure,
    Other,
}

pub struct TypeDescriptor {
    pub name: &'static str,
    pub kind: ObjectKind,
    pub trace: TraceDescriptor,
    pub destroy: unsafe fn(*mut GcHeader),
}

pub enum TraceDescriptor {
    None,
    /// offsets are byte offsets from the start of the object
    /// to inline `GcRef` fields
    Fields(&'static [u32]),
    /// A tag-selected set of inline `GcRef` fields. The tag and field offsets
    /// are byte offsets from the start of the object.
    TaggedFields {
        tag_offset: u32,
        variants: &'static [(u32, &'static [u32])],
    },
}

fn valid_trace_offsets(offsets: &[u32], size: usize) -> bool {
    offsets.iter().all(|&offset| {
        let offset = offset as usize;
        offset >= size_of::<GcHeader>()
            && offset % align_of::<GcRef>() == 0
            && offset
                .checked_add(size_of::<GcRef>())
                .is_some_and(|end| end <= size)
    })
}

/// utility/helper to get a Layout with the assumed alignment of GcHeader
fn layout_for_size(size: usize) -> Layout {
    Layout::from_size_align(size, align_of::<GcHeader>()).expect("valid GC allocation layout")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::offset_of;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[repr(C)]
    struct TestLeaf {
        header: GcHeader,
        value: usize,
    }

    #[repr(C)]
    struct TestParent {
        header: GcHeader,
        child: GcRef,
    }

    #[repr(C)]
    struct TestTagged {
        header: GcHeader,
        tag: usize,
        child: GcRef,
    }

    static DESTROY_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe fn destroy_test_object(_header: *mut GcHeader) {
        DESTROY_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    static TEST_LEAF_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
        name: "TestLeaf",
        kind: ObjectKind::Other,
        trace: TraceDescriptor::None,
        destroy: destroy_test_object,
    };

    static TEST_PARENT_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
        name: "TestParent",
        kind: ObjectKind::Other,
        trace: TraceDescriptor::Fields(&[offset_of!(TestParent, child) as u32]),
        destroy: destroy_test_object,
    };

    static TEST_TAGGED_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
        name: "TestTagged",
        kind: ObjectKind::Other,
        trace: TraceDescriptor::TaggedFields {
            tag_offset: offset_of!(TestTagged, tag) as u32,
            variants: &[(0, &[offset_of!(TestTagged, child) as u32])],
        },
        destroy: destroy_test_object,
    };

    struct SlotRootSet<'a> {
        slots: Vec<*mut GcRef>,
        _marker: std::marker::PhantomData<&'a mut GcRef>,
    }

    impl<'a> SlotRootSet<'a> {
        fn new(slots: Vec<*mut GcRef>) -> Self {
            Self {
                slots,
                _marker: std::marker::PhantomData,
            }
        }
    }

    impl RootSet for SlotRootSet<'_> {
        fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
            for &slot in &self.slots {
                visitor.visit_gc_ref(slot);
            }
        }
    }

    #[test]
    fn alloc_raw_rejects_invalid_sizes() {
        let mut heap = Heap::default();
        let mut roots = EmptyRootSet;

        let too_small =
            heap.alloc_raw(size_of::<GcHeader>() - 1, &TEST_LEAF_DESCRIPTOR, &mut roots);
        assert!(matches!(too_small, Err(AllocError::InvalidSize)));

        let too_large = heap.alloc_raw((u32::MAX as usize) + 1, &TEST_LEAF_DESCRIPTOR, &mut roots);
        assert!(matches!(too_large, Err(AllocError::InvalidSize)));
    }

    #[test]
    fn global_runtime_heap_cannot_collect_without_combined_roots() {
        let mut heap = crate::Runtime::default().heap;
        heap.next_gc_threshold = 0;
        let mut roots = EmptyRootSet;

        let first = heap.alloc_string(b"first", &mut roots).unwrap();
        heap.alloc_string(b"second", &mut roots).unwrap();

        assert_eq!(
            heap.allocated_bytes,
            2 * size_of::<crate::string::HopperString>()
        );
        // Runtime, not this private Heap policy, owns global collections.
        let first = first.as_ptr().cast::<crate::string::HopperString>();
        unsafe {
            assert_eq!((*first).len, 5);
            assert_eq!(std::slice::from_raw_parts((*first).bytes, 5), b"first");
        }
        heap.collect(&mut roots);
    }

    #[test]
    fn null_stack_record_field_is_not_a_gc_ref() {
        let mut heap = Heap::default();
        let mut no_roots = EmptyRootSet;
        heap.alloc_string(b"unreachable", &mut no_roots).unwrap();

        let mut empty_field = 0usize;
        let slot = (&mut empty_field as *mut usize).cast::<GcRef>();
        let mut roots = SlotRootSet::new(vec![slot]);
        heap.collect(&mut roots);
        assert_eq!(heap.allocated_bytes, 0);
    }

    #[test]
    fn traced_fields_start_null_and_can_be_collected_before_initialization() {
        let mut heap = Heap::default();
        let parent = heap
            .alloc_raw(
                size_of::<TestParent>(),
                &TEST_PARENT_DESCRIPTOR,
                &mut EmptyRootSet,
            )
            .unwrap();
        let child_word = unsafe {
            std::ptr::addr_of!((*parent.as_ptr().cast::<TestParent>()).child)
                .cast::<*mut GcHeader>()
                .read()
        };
        assert!(child_word.is_null());

        let mut parent_root = parent;
        heap.collect(&mut SlotRootSet::new(vec![&mut parent_root]));
        assert_eq!(heap.allocated_bytes, size_of::<TestParent>());
        heap.collect(&mut EmptyRootSet);
        assert_eq!(heap.allocated_bytes, 0);
    }

    #[test]
    fn marks_deep_heap_graph_without_recursing() {
        let mut heap = Heap::default();
        heap.disable_automatic_collection();
        let mut root = heap
            .alloc_raw(
                size_of::<TestParent>(),
                &TEST_PARENT_DESCRIPTOR,
                &mut EmptyRootSet,
            )
            .unwrap();
        let mut current = root;
        for _ in 0..16_384 {
            let child = heap
                .alloc_raw(
                    size_of::<TestParent>(),
                    &TEST_PARENT_DESCRIPTOR,
                    &mut EmptyRootSet,
                )
                .unwrap();
            // SAFETY: the parent is allocated, its child field is aligned, and
            // this test holds the heap exclusively during graph construction.
            unsafe { (*current.as_ptr().cast::<TestParent>()).child = child };
            current = child;
        }
        let total = heap.allocated_bytes();
        heap.collect(&mut SlotRootSet::new(vec![&mut root]));
        assert_eq!(heap.allocated_bytes(), total);
        heap.collect(&mut EmptyRootSet);
        assert_eq!(heap.allocated_bytes(), 0);
    }

    #[test]
    fn rejects_traced_fields_outside_aligned_object_payload() {
        static BAD_HEADER: TypeDescriptor = TypeDescriptor {
            name: "BadHeader",
            kind: ObjectKind::Other,
            trace: TraceDescriptor::Fields(&[0]),
            destroy: destroy_test_object,
        };
        static BAD_ALIGNMENT: TypeDescriptor = TypeDescriptor {
            name: "BadAlignment",
            kind: ObjectKind::Other,
            trace: TraceDescriptor::Fields(&[(size_of::<GcHeader>() + 1) as u32]),
            destroy: destroy_test_object,
        };
        static BAD_BOUND: TypeDescriptor = TypeDescriptor {
            name: "BadBound",
            kind: ObjectKind::Other,
            trace: TraceDescriptor::Fields(&[size_of::<TestParent>() as u32]),
            destroy: destroy_test_object,
        };
        let mut heap = Heap::default();
        for descriptor in [&BAD_HEADER, &BAD_ALIGNMENT, &BAD_BOUND] {
            assert!(matches!(
                heap.alloc_raw(size_of::<TestParent>(), descriptor, &mut EmptyRootSet),
                Err(AllocError::InvalidSize)
            ));
        }
        assert!(heap.head.is_none());
    }

    #[test]
    fn allocation_accounting_fails_instead_of_wrapping() {
        let mut heap = Heap::default();
        // Synthetic counters avoid requiring impossible physical allocations.
        heap.allocated_bytes = usize::MAX - 1;
        heap.next_gc_threshold = usize::MAX;
        assert!(heap.collection_due_for(size_of::<TestLeaf>()));
        heap.disable_automatic_collection();
        assert!(matches!(
            heap.alloc_raw(
                size_of::<TestLeaf>(),
                &TEST_LEAF_DESCRIPTOR,
                &mut EmptyRootSet
            ),
            Err(AllocError::OutOfMemory)
        ));
        assert!(heap.head.is_none());
    }

    #[test]
    fn threshold_growth_saturates_without_wrapping() {
        let mut heap = Heap::default();
        // Exercise arithmetic without allocating an enormous heap.
        heap.next_gc_threshold = usize::MAX / GC_THRESHOLD_FACTOR + 1;
        heap.allocated_bytes = heap.next_gc_threshold;
        heap.collect(&mut EmptyRootSet);
        assert_eq!(heap.next_gc_threshold, usize::MAX);
    }

    #[test]
    fn collect_reclaims_unreachable_objects() {
        DESTROY_COUNT.store(0, Ordering::SeqCst);

        let mut heap = Heap::default();
        let mut roots = EmptyRootSet;

        let size = size_of::<TestLeaf>();
        let _leaf = heap
            .alloc_raw(size, &TEST_LEAF_DESCRIPTOR, &mut roots)
            .unwrap();

        assert_eq!(heap.allocated_bytes, size);
        heap.collect(&mut roots);

        assert_eq!(heap.allocated_bytes, 0);
        assert_eq!(DESTROY_COUNT.load(Ordering::SeqCst), 1);
        assert!(heap.head.is_none());
    }

    #[test]
    fn traced_child_is_kept_alive_when_parent_is_rooted() {
        DESTROY_COUNT.store(0, Ordering::SeqCst);

        let mut heap = Heap::default();
        let mut no_roots = EmptyRootSet;

        let child = heap
            .alloc_raw(size_of::<TestLeaf>(), &TEST_LEAF_DESCRIPTOR, &mut no_roots)
            .unwrap();

        let parent = heap
            .alloc_raw(
                size_of::<TestParent>(),
                &TEST_PARENT_DESCRIPTOR,
                &mut no_roots,
            )
            .unwrap();

        unsafe {
            let parent_ptr = parent.as_ptr().cast::<TestParent>();
            (*parent_ptr).child = child;
            assert_eq!((*parent_ptr).child.as_ptr(), child.as_ptr());
        }

        let mut parent_root = parent;
        let mut roots = SlotRootSet::new(vec![&mut parent_root as *mut GcRef]);

        heap.collect(&mut roots);

        assert_eq!(DESTROY_COUNT.load(Ordering::SeqCst), 0);
        assert_eq!(
            heap.allocated_bytes,
            size_of::<TestLeaf>() + size_of::<TestParent>()
        );
    }

    #[test]
    fn tagged_trace_descriptor_only_marks_active_variant_fields() {
        let mut heap = Heap::default();
        let child = heap
            .alloc_raw(size_of::<TestLeaf>(), &TEST_LEAF_DESCRIPTOR, &mut EmptyRootSet)
            .unwrap();
        let tagged = heap
            .alloc_raw(
                size_of::<TestTagged>(),
                &TEST_TAGGED_DESCRIPTOR,
                &mut EmptyRootSet,
            )
            .unwrap();

        unsafe {
            let tagged_ptr = tagged.as_ptr().cast::<TestTagged>();
            (*tagged_ptr).tag = 1;
            (*tagged_ptr).child = child;
        }

        let mut tagged_root = tagged;
        heap.collect(&mut SlotRootSet::new(vec![&mut tagged_root]));
        assert_eq!(heap.allocated_bytes(), size_of::<TestTagged>());

        heap.collect(&mut EmptyRootSet);
        assert_eq!(heap.allocated_bytes(), 0);
    }
}
