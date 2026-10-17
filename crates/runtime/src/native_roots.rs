//! Explicit roots for GC values retained by native callers between Hopper calls.

use std::collections::HashMap;

use crate::ffi::RawGcRef;
use crate::memory_recycling::{GcRef, RootSet, RootVisitor};

#[derive(Default)]
pub(crate) struct NativeRoots {
    next_id: u64,
    values: HashMap<u64, usize>,
}

impl NativeRoots {
    fn insert(&mut self, address: usize) -> u64 {
        let id = self.next_id;
        self.next_id = id.checked_add(1).expect("native root ID overflow");
        self.values.insert(id, address);
        id
    }

    fn remove(&mut self, id: u64) {
        assert!(self.values.remove(&id).is_some(), "missing native root");
    }
}

impl RootSet for NativeRoots {
    fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
        for address in self.values.values_mut() {
            // GcRef is a transparent pointer-sized wrapper; Marker reads this
            // word as a raw pointer, without creating a NonNull from a null.
            visitor.visit_gc_ref((address as *mut usize).cast::<GcRef>());
        }
    }
}

/// Keeps a GC object returned to native code alive until this handle is dropped.
///
/// Handles register in the global runtime, not in an independent `Heap`. Do
/// not construct, inspect, or drop one while holding the global runtime mutex.
/// The raw address returned by `as_raw` is only valid while the handle remains alive.
/// `from_raw` is not an atomic JIT-return-to-handle boundary; use
/// `call_no_arg_string` for supported no-argument String-returning entries.
#[must_use = "dropping the handle releases its GC root"]
pub struct NativeRoot {
    id: u64,
}

impl NativeRoot {
    /// Invoke a no-argument Hopper entry returning a `String` and root the
    /// result before another Hopper thread can run a collection.
    ///
    /// This only supports one entry signature, it's not yet a
    /// general-purpose native-to-Hopper trampoline. Do not invoke while
    /// holding the global runtime mutex or from within a Hopper callback.
    ///
    /// # Safety
    /// `entry` must be a registered Hopper JIT entry point with ABI
    /// `extern "C" fn() -> String`, where `String` is one GC pointer word.
    /// The JIT code and its stack-map registration must outlive the call.
    /// Only collections through `collect_idle` or a Hopper allocation
    /// safepoint are coordinated with this reservation.
    pub unsafe fn call_no_arg_string(entry: *const u8) -> Self {
        let reservation = crate::execution::gate()
            .reserve_native_entry()
            .expect("cannot reserve Hopper native entry");
        // SAFETY: the caller guarantees the JIT entry has this ABI and is live.
        let function: extern "C" fn() -> RawGcRef = unsafe { std::mem::transmute(entry) };
        let raw = function();
        // SAFETY: the reservation excludes other Hopper execution until the
        // return value is registered; the entry returned a live GC String.
        let handle = unsafe { Self::from_raw(raw) };
        drop(reservation);
        handle
    }

    /// Registers a live object in the global runtime heap as a native root.
    ///
    /// # Safety
    /// `raw` must be non-null and refer to a live GC object owned by the
    /// global runtime. No collection may run between producing `raw` and this
    /// call; in particular, a concurrent collector must not be active.
    pub unsafe fn from_raw(raw: RawGcRef) -> Self {
        assert!(!raw.is_null(), "cannot root a null GC reference");
        let mut runtime = crate::runtime().lock().expect("runtime lock poisoned");
        Self {
            id: runtime.native_roots.insert(raw as usize),
        }
    }

    pub fn as_raw(&self) -> RawGcRef {
        let runtime = crate::runtime().lock().expect("runtime lock poisoned");
        runtime.native_roots.values[&self.id] as RawGcRef
    }
}

impl Drop for NativeRoot {
    fn drop(&mut self) {
        let mut runtime = crate::runtime().lock().unwrap_or_else(|_| {
            eprintln!("Hopper native root: runtime lock poisoned");
            std::process::abort();
        });
        runtime.native_roots.remove(self.id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_recycling::{EmptyRootSet, Heap};

    struct OneRoot<'a>(&'a mut GcRef);

    impl RootSet for OneRoot<'_> {
        fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
            visitor.visit_gc_ref(self.0 as *mut GcRef);
        }
    }

    #[test]
    fn runtime_threshold_collection_combines_stack_and_native_roots() {
        let mut runtime = crate::Runtime::default();
        let object_size = std::mem::size_of::<crate::string::HopperString>();
        runtime
            .heap
            .set_collection_threshold_for_test(3 * object_size);
        // Exercise the default policy on a local runtime with explicit roots.
        assert!(runtime.automatic_collection_enabled);

        runtime.alloc_string(b"dead", &mut EmptyRootSet).unwrap();
        let native = runtime.alloc_string(b"native", &mut EmptyRootSet).unwrap();
        let mut stack = runtime.alloc_string(b"stack", &mut EmptyRootSet).unwrap();
        let id = runtime.native_roots.insert(native.as_ptr() as usize);

        runtime
            .alloc_string(b"trigger", &mut OneRoot(&mut stack))
            .unwrap();
        assert_eq!(runtime.heap.allocated_bytes(), 3 * object_size);
        assert!(runtime.heap.contains_address(native.as_ptr() as usize));
        assert!(runtime.heap.contains_address(stack.as_ptr() as usize));

        runtime.native_roots.remove(id);
        runtime.collect(&mut EmptyRootSet);
        assert!(!runtime.heap.contains_address(native.as_ptr() as usize));
    }

    #[test]
    fn runtime_collects_at_threshold_by_default() {
        let mut runtime = crate::Runtime::default();
        assert!(runtime.automatic_collection_enabled);
        let object_size = std::mem::size_of::<crate::string::HopperString>();
        runtime.alloc_string(b"first", &mut EmptyRootSet).unwrap();
        runtime.heap.set_collection_threshold_for_test(object_size);
        runtime.alloc_string(b"second", &mut EmptyRootSet).unwrap();
        assert_eq!(runtime.heap.allocated_bytes(), object_size);
    }

    #[test]
    fn native_roots_keep_objects_alive_until_removed() {
        let mut heap = Heap::default();
        let mut native = NativeRoots::default();
        let value = heap.alloc_string(b"held", &mut EmptyRootSet).unwrap();
        let address = value.as_ptr() as usize;
        let id = native.insert(address);

        heap.collect(&mut native);
        assert!(heap.contains_address(address));

        native.remove(id);
        heap.collect(&mut native);
        assert!(!heap.contains_address(address));
    }
}
