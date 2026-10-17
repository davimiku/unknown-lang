use std::mem::size_of;
use std::sync::{Mutex, OnceLock};

use crate::errors::AllocError;
use crate::memory_recycling::GcRef;
use crate::string::HopperString;

pub use crate::memory_recycling::Heap;
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
    /// Allocate a string through the global runtime's collection policy. New
    /// managed builtin allocators must also pass through a Runtime method so
    /// threshold collections see both Hopper and native roots.
    pub(crate) fn alloc_string(
        &mut self,
        bytes: &[u8],
        stack_roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        if self.automatic_collection_enabled
            && self.heap.collection_due_for(size_of::<HopperString>())
        {
            self.collect(stack_roots);
        }
        // The global heap's own threshold is disabled: only Runtime decides
        // when to collect, with both stack and native roots available.
        self.heap.alloc_string(
            bytes,
            &mut CombinedRoots {
                stack: stack_roots,
                native: &mut self.native_roots,
            },
        )
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
