/// Also known as "garbage collection" but cuter name
/// Memory is reused/recycled, not thrown away
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
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
}

impl Default for Heap {
    fn default() -> Self {
        Self {
            head: None,
            allocated_bytes: 0,
            next_gc_threshold: INITIAL_THRESHOLD,
        }
    }
}

// SAFETY: runtime behavior is intentionally stop-the-world and operated through
// a global `Mutex`. Collection is stop-the-world
// The heap’s intrusive pointers are only mutated under external synchronization.
unsafe impl Send for Heap {}

impl Heap {
    // Right now this just allocates the GcHeader, other callers have
    // to fill in the rest (like the bytes for a string), but this could
    // be bad for a record with ref fields itself
    // Possible ideas:
    // - initializing all GC fields to a known null/empty representation before publication
    // - making a helper allocator that constructs the full object before it can become reachable
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
        // todo - track a max_allocated_bytes and Err OOM here
        // so people who embed this runtime can control the heap size

        if self.allocated_bytes + size > self.next_gc_threshold {
            self.collect(roots);
        }

        let layout = layout_for_size(size);
        let ptr = unsafe { alloc(layout) }.cast::<GcHeader>();
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        // safety: ptr was just checked for null
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        let object = GcRef(ptr);

        unsafe {
            ptr.as_ptr().write(GcHeader {
                marked: false,
                size_bytes: size as u32,
                next: self.head,
                descriptor,
            });
        }

        self.head = Some(ptr);
        self.allocated_bytes += size;

        if self.allocated_bytes > self.next_gc_threshold {
            self.next_gc_threshold = cmp::max(
                self.next_gc_threshold * GC_THRESHOLD_FACTOR,
                self.allocated_bytes,
            );
        }

        Ok(object)
    }

    pub fn collect(&mut self, roots: &mut dyn RootSet) {
        let mut marker = Marker;
        roots.visit_roots(&mut marker);
        self.sweep();

        if self.allocated_bytes >= self.next_gc_threshold / GC_THRESHOLD_FACTOR {
            self.next_gc_threshold = cmp::max(
                self.allocated_bytes * GC_THRESHOLD_FACTOR,
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

struct Marker;

impl RootVisitor for Marker {
    fn visit_gc_ref(&mut self, root: *mut GcRef) {
        if root.is_null() {
            return;
        }

        let gc_ref = unsafe { *root };
        mark_object(gc_ref);
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
}

/// utility/helper to get a Layout with the assumed alignment of GcHeader
fn layout_for_size(size: usize) -> Layout {
    Layout::from_size_align(size, align_of::<GcHeader>()).expect("valid GC allocation layout")
}

fn mark_object(gc_ref: GcRef) {
    let header = gc_ref.as_ptr();
    unsafe {
        if (*header).marked {
            return;
        }
        (*header).marked = true;

        match (*header).descriptor.trace {
            TraceDescriptor::None => {}
            TraceDescriptor::Fields(offsets) => {
                for &offset in offsets {
                    let field = header.cast::<u8>().add(offset as usize).cast::<GcRef>();
                    mark_root_slot(field);
                }
            }
        }
    }
}

fn mark_root_slot(root: *mut GcRef) {
    if root.is_null() {
        return;
    }

    let gc_ref = unsafe { *root };
    mark_object(gc_ref);
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

        mark_object(parent);

        unsafe {
            assert!((*parent.as_ptr()).marked);
            assert!((*child.as_ptr()).marked);
            (*parent.as_ptr()).marked = false;
            (*child.as_ptr()).marked = false;
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
}
