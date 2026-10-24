use std::mem::size_of;
use std::sync::{Mutex, OnceLock};

use crate::errors::AllocError;
use crate::string::HopperString;

pub use crate::memory_recycling::{
    GcHeader, GcRef, Heap, ObjectKind, TraceDescriptor, TypeDescriptor,
};
pub use crate::native_roots::NativeRoot;
pub use crate::stackmaps::{Safepoint, StackMapRegistry};
use memory_recycling::{EmptyRootSet, RootSet, RootVisitor};

mod errors;
mod execution;
pub mod ffi;
mod frame_walk;
mod memory_recycling;
mod native_roots;
mod stackmaps;
mod string;

static RUNTIME: OnceLock<Mutex<Runtime>> = OnceLock::new();

pub struct Runtime {
    heap: Heap,
    stackmap_registry: StackMapRegistry,
    native_roots: native_roots::NativeRoots,
    automatic_collection_enabled: bool,
}

impl Default for Runtime {
    fn default() -> Self {
        let mut heap = Heap::default();
        heap.disable_automatic_collection();
        Self {
            heap,
            stackmap_registry: StackMapRegistry::default(),
            native_roots: native_roots::NativeRoots::default(),
            automatic_collection_enabled: true,
        }
    }
}

struct CombinedRoots<'a> {
    stack: &'a mut dyn RootSet,
    native: &'a mut native_roots::NativeRoots,
}

impl RootSet for CombinedRoots<'_> {
    fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
        self.stack.visit_roots(visitor);
        self.native.visit_roots(visitor);
    }
}

impl Runtime {
    /// Allocate a descriptor-backed managed object through the global runtime.
    ///
    /// This is the common allocation path for managed objects other than the
    /// String convenience type. The collection check happens here, rather than
    /// in `Heap`, so collections include both the supplied Hopper roots and
    /// registered native roots.
    pub(crate) fn alloc_object(
        &mut self,
        size: usize,
        descriptor: &'static TypeDescriptor,
        stack_roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        if self.automatic_collection_enabled && self.heap.collection_due_for(size) {
            self.collect(stack_roots);
        }

        // The global heap's threshold is disabled: Runtime owns the policy so
        // that every collection sees the complete combined root set.
        self.heap.alloc_raw(
            size,
            descriptor,
            &mut CombinedRoots {
                stack: stack_roots,
                native: &mut self.native_roots,
            },
        )
    }

    /// Allocate a string through the global runtime's collection policy. New
    /// managed builtin allocators must also pass through a Runtime method so
    /// threshold collections see both Hopper and native roots.
    pub(crate) fn alloc_string(
        &mut self,
        bytes: &[u8],
        stack_roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        let len = bytes.len();
        let mut copied = Vec::new();
        copied
            .try_reserve_exact(len)
            .map_err(|_| AllocError::OutOfMemory)?;
        copied.extend_from_slice(bytes);
        let boxed = copied.into_boxed_slice();

        let string_ref = self.alloc_object(
            size_of::<HopperString>(),
            &crate::string::HOPPER_STRING_DESCRIPTOR,
            stack_roots,
        )?;
        let ptr = Box::into_raw(boxed) as *mut u8;

        // No collection can occur between alloc_object and initialization, so
        // the newly allocated object is safe to initialize before publishing it
        // to the caller.
        unsafe {
            let string = string_ref.as_ptr().cast::<HopperString>();
            (*string).bytes = ptr;
            (*string).len = len;
        }
        Ok(string_ref)
    }

    /// Collect using both the supplied Hopper roots and registered native roots.
    /// The caller must still ensure that no other Hopper execution is unsuspended
    /// and that all live values are rooted; the runtime mutex alone is not a
    /// stop-the-world mechanism. Do not call `heap.collect` on the global heap.
    pub(crate) fn collect(&mut self, stack_roots: &mut dyn RootSet) {
        self.heap.collect(&mut CombinedRoots {
            stack: stack_roots,
            native: &mut self.native_roots,
        });
    }
}

fn init_runtime() -> &'static Mutex<Runtime> {
    RUNTIME.get_or_init(|| Mutex::new(Runtime::default()))
}

pub(crate) fn runtime() -> &'static Mutex<Runtime> {
    init_runtime()
}

/// Collect when no Hopper frames are executing. Native-held GC values must
/// have registered handles. Unrooted raw return values may be reclaimed.
/// Reserve the execution gate before taking the runtime mutex so an
/// allocator cannot wait on that mutex while we wait for its frame to finish.
pub fn collect_idle() {
    let _reservation = execution::gate()
        .reserve_idle_collection()
        .expect("cannot collect while executing Hopper on this thread");
    let mut runtime = runtime().lock().expect("runtime lock poisoned");
    runtime.collect(&mut EmptyRootSet);
}

/// Set a low automatic-collection threshold for an isolated GC stress test.
/// Normal runtimes already collect at their default threshold.
///
/// # Safety
/// All native-held GC values must have registered handles, and raw JIT entry
/// calls returning GC values must not be used while collection is enabled.
/// Call only when no Hopper frames are active; the reservation enforces this.
#[cfg(feature = "gc-stress")]
pub unsafe fn configure_automatic_collection_for_test(threshold: usize) {
    let _reservation = execution::gate()
        .reserve_idle_collection()
        .expect("cannot configure GC during Hopper execution");
    let mut runtime = runtime().lock().expect("runtime lock poisoned");
    runtime.heap.set_collection_threshold_for_test(threshold);
}

#[cfg(feature = "gc-stress")]
pub fn allocated_bytes_for_test() -> usize {
    runtime()
        .lock()
        .expect("runtime lock poisoned")
        .heap
        .allocated_bytes()
}

#[cfg(any(debug_assertions, feature = "gc-stress"))]
pub fn heap_contains_address(address: usize) -> bool {
    runtime()
        .lock()
        .expect("runtime lock poisoned")
        .heap
        .contains_address(address)
}

#[cfg(debug_assertions)]
pub fn stack_map_info(return_address: usize) -> Option<(u32, usize)> {
    let runtime = runtime().lock().expect("runtime lock poisoned");
    let range = runtime
        .stackmap_registry
        .function_containing(return_address)?;
    let map = runtime.stackmap_registry.get(return_address)?;
    Some((range.frame_to_fp_offset, map.root_offsets.len()))
}

pub fn register_function_stack_maps(
    function_start: usize,
    function_size: u32,
    frame_to_fp_offset: u32,
    safepoints: Vec<Safepoint>,
) {
    let mut runtime = runtime()
        .lock()
        .expect("runtime to not be poisoned while registering stack maps");

    runtime.stackmap_registry.register_function(
        function_start,
        function_size,
        frame_to_fp_offset,
        safepoints,
    );
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;
    use crate::memory_recycling::GcHeader;

    static DESTROY_COUNT: AtomicUsize = AtomicUsize::new(0);

    unsafe fn destroy_test_object(_header: *mut GcHeader) {
        DESTROY_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    static TEST_OBJECT_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
        name: "TestObject",
        kind: ObjectKind::Other,
        trace: TraceDescriptor::None,
        destroy: destroy_test_object,
    };

    #[test]
    fn generic_runtime_allocation_uses_global_collection_policy() {
        DESTROY_COUNT.store(0, Ordering::SeqCst);
        let mut runtime = Runtime::default();
        runtime.heap.set_collection_threshold_for_test(size_of::<GcHeader>());

        let first = runtime
            .alloc_object(
                size_of::<GcHeader>(),
                &TEST_OBJECT_DESCRIPTOR,
                &mut EmptyRootSet,
            )
            .unwrap();
        let _first = first;

        runtime
            .alloc_object(
                size_of::<GcHeader>(),
                &TEST_OBJECT_DESCRIPTOR,
                &mut EmptyRootSet,
            )
            .unwrap();

        // The allocator may reuse the reclaimed address for the second object,
        // so reclamation is checked through destruction and accounting rather
        // than address uniqueness.
        assert_eq!(DESTROY_COUNT.load(Ordering::SeqCst), 1);
        assert_eq!(runtime.heap.allocated_bytes(), size_of::<GcHeader>());
    }
}
