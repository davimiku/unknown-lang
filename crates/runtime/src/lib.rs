use std::sync::{Mutex, OnceLock};

pub use crate::memory_recycling::Heap;
pub use crate::stackmaps::{Safepoint, StackMapRegistry};

mod errors;
pub mod ffi;
mod memory_recycling;
mod stackmaps;
mod string;

pub static RUNTIME: OnceLock<Mutex<Runtime>> = OnceLock::new();

pub struct Runtime {
    pub heap: Heap,
    pub stackmap_registry: StackMapRegistry,
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            heap: Heap::default(),
            stackmap_registry: StackMapRegistry::default(),
        }
    }
}

pub fn init_runtime() -> &'static Mutex<Runtime> {
    RUNTIME.get_or_init(|| Mutex::new(Runtime::default()))
}

pub fn runtime() -> &'static Mutex<Runtime> {
    init_runtime()
}

pub fn register_function_stack_maps(
    function_start: usize,
    function_size: u32,
    safepoints: Vec<Safepoint>,
) {
    let mut runtime = runtime()
        .lock()
        .expect("runtime to not be poisoned while registering stack maps");

    runtime
        .stackmap_registry
        .register_function(function_start, function_size, safepoints);
}
