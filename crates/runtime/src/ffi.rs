#[cfg(debug_assertions)]
use std::cell::{Cell, RefCell};
use std::slice;

use crate::frame_walk::StackRootSet;
use crate::memory_recycling::{GcHeader, TypeDescriptor};
use crate::string::HopperString;

pub type RawGcRef = *mut GcHeader;

mod alloc_entry;
mod frame_entry;

/// JIT function check coming in, capturing the parent frame's return PC
pub fn enter_frame_entry() -> *const u8 {
    frame_entry::address()
}

/// JIT function check going out, before returning to the caller.
pub fn leave_frame_entry() -> *const u8 {
    leave_frame_impl as *const u8
}

pub(super) extern "C" fn enter_frame_impl(parent_return_address: usize) {
    let gate = crate::execution::gate();
    let owns_gate = gate.owned_by_current_thread().unwrap_or_else(|error| {
        eprintln!("Hopper execution boundary: {error}");
        std::process::abort();
    });
    let parent_is_hopper = {
        let runtime_mutex = crate::runtime();
        // A native callback could hold this mutex on the same thread.
        // We can't block on it until the gate can reject unsupported re-entry.
        let runtime = if owns_gate {
            runtime_mutex.try_lock().unwrap_or_else(|_| {
                eprintln!(
                    "Hopper execution boundary: runtime lock unavailable during Hopper execution"
                );
                std::process::abort();
            })
        } else {
            runtime_mutex.lock().unwrap_or_else(|_| {
                eprintln!("Hopper execution boundary: runtime lock poisoned");
                std::process::abort();
            })
        };
        runtime
            .stackmap_registry
            .function_containing(parent_return_address)
            .is_some()
    };
    if let Err(error) = crate::execution::gate().enter(parent_is_hopper) {
        eprintln!("Hopper execution boundary: {error}");
        std::process::abort();
    }
}

extern "C" fn leave_frame_impl() {
    if let Err(error) = crate::execution::gate().leave() {
        eprintln!("Hopper execution boundary: {error}");
        std::process::abort();
    }
}

/// An entry point that captures its Hopper caller before entering Rust.
/// The pointer can be registered as a JIT symbol, but unlike a plain Rust
/// function address it satisfies the runtime's allocation-call-site contract.
#[derive(Clone, Copy)]
pub struct GcEntryAddress(*const u8);

impl GcEntryAddress {
    pub fn as_ptr(self) -> *const u8 {
        self.0
    }
}

/// The two-argument string allocator's root-capturing JIT entry point.
pub fn alloc_string_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::address())
}

/// A descriptor-aware managed-object allocator entry point.
///
/// The Hopper-visible ABI is `(size: Int, descriptor: pointer) -> GcRef`.
/// `descriptor` must point to a live `TypeDescriptor` whose trace offsets and
/// destructor remain valid for the lifetime of the compiled code.
pub fn alloc_object_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::object_address())
}

/// Captured from the Hopper call site, before entering any Rust stack frames
#[derive(Clone, Copy, Debug)]
pub struct AllocationCallSite {
    pub caller_sp: usize,
    pub return_address: usize,
    pub caller_fp: usize,
}

#[cfg(debug_assertions)]
thread_local! {
    static LAST_ALLOCATION_CALL_SITE: Cell<Option<AllocationCallSite>> = const { Cell::new(None) };
    static PROBE_ROOTS: Cell<bool> = const { Cell::new(false) };
    static LAST_PROBED_ROOTS: RefCell<Option<ProbedRoots>> = const { RefCell::new(None) };
    static FORCE_NEXT_COLLECTION: Cell<bool> = const { Cell::new(false) };
    static FORCE_NEXT_COLLECTION_WITH_LIVE_ROOT: Cell<bool> = const { Cell::new(false) };
    static LAST_FORCED_COLLECTION: Cell<Option<ForcedCollection>> = const { Cell::new(None) };
}

#[cfg(debug_assertions)]
#[derive(Debug)]
pub struct ProbedRoots {
    pub return_address: usize,
    /// Copies of the root words while Hopper was suspended, with their frame PCs.
    pub roots: Vec<ProbedRoot>,
}

#[cfg(debug_assertions)]
#[derive(Debug)]
pub struct ProbedRoot {
    pub return_address: usize,
    pub word: usize,
}

/// Size change during a one-shot collection immediately before a JIT allocation.
#[cfg(debug_assertions)]
#[derive(Debug, Clone, Copy)]
pub struct ForcedCollection {
    pub bytes_before: usize,
    pub bytes_after: usize,
}

/// Force one collection at the next string allocation with a non-null older-frame root.
/// For isolated JIT tests only: native callers in other tests may retain raw
/// GC values that have not been registered as NativeRoots.
#[cfg(debug_assertions)]
pub fn force_collection_at_next_allocation_with_older_root() {
    LAST_FORCED_COLLECTION.with(|last| last.set(None));
    FORCE_NEXT_COLLECTION.with(|armed| armed.set(true));
}

/// Force a collection when the allocating Hopper frame has a live root.
/// For isolated JIT tests only; raw native-held values still need handles.
#[cfg(debug_assertions)]
pub fn force_collection_at_next_allocation_with_live_root() {
    LAST_FORCED_COLLECTION.with(|last| last.set(None));
    FORCE_NEXT_COLLECTION_WITH_LIVE_ROOT.with(|armed| armed.set(true));
}

#[cfg(debug_assertions)]
pub fn take_forced_collection() -> Option<ForcedCollection> {
    LAST_FORCED_COLLECTION.with(|last| last.take())
}

/// Arm a one-shot probe for the next allocation with live stack-map roots.
#[cfg(debug_assertions)]
pub fn probe_next_live_allocation_roots() {
    PROBE_ROOTS.with(|armed| armed.set(true));
    LAST_PROBED_ROOTS.with(|last| last.borrow_mut().take());
}

#[cfg(debug_assertions)]
pub fn take_probed_roots() -> Option<ProbedRoots> {
    LAST_PROBED_ROOTS.with(|last| last.borrow_mut().take())
}

/// Debug probe for verifying the shim against Cranelift's frame metadata.
#[cfg(debug_assertions)]
pub fn take_last_allocation_call_site() -> Option<AllocationCallSite> {
    LAST_ALLOCATION_CALL_SITE.with(|last| last.take())
}

// Called only by the assembly shim; its extra arguments are not part of the
// Hopper-visible allocator ABI.
pub(super) extern "C" fn alloc_string_impl(
    bytes: *const u8,
    len: i64,
    caller_sp: usize,
    return_address: usize,
    caller_fp: usize,
) -> RawGcRef {
    #[cfg(debug_assertions)]
    LAST_ALLOCATION_CALL_SITE.with(|last| {
        last.set(Some(AllocationCallSite {
            caller_sp,
            return_address,
            caller_fp,
        }));
    });

    let len = usize::try_from(len).unwrap_or_else(|_| {
        eprintln!("Hopper string allocation: negative length");
        std::process::abort();
    });
    if bytes.is_null() {
        eprintln!("Hopper string allocation: null bytes pointer");
        std::process::abort();
    }

    let runtime = crate::runtime();
    let mut runtime = runtime.lock().unwrap_or_else(|_| {
        eprintln!("Hopper string allocation: runtime lock poisoned");
        std::process::abort();
    });

    let mut roots = StackRootSet::new(
        AllocationCallSite {
            caller_sp,
            return_address,
            caller_fp,
        },
        &runtime.stackmap_registry,
    );

    #[cfg(debug_assertions)]
    if PROBE_ROOTS.with(|armed| armed.get())
        && roots
            .slots()
            .iter()
            .any(|root| unsafe { root.slot.cast::<usize>().read() } != 0)
    {
        let observed = roots
            .slots()
            .iter()
            .map(|root| ProbedRoot {
                return_address: root.return_address,
                // SAFETY: the walker validated this slot in a suspended Hopper frame.
                word: unsafe { root.slot.cast::<usize>().read() },
            })
            .collect();
        LAST_PROBED_ROOTS.with(|last| {
            *last.borrow_mut() = Some(ProbedRoots {
                return_address,
                roots: observed,
            });
        });
        PROBE_ROOTS.with(|armed| armed.set(false));
    }

    // Forced collection is for isolated tests. Normal automatic collection
    // happens in Runtime::alloc_string below, using the same captured roots.
    #[cfg(debug_assertions)]
    let has_older_live_root = roots.slots().iter().any(|root| {
        root.return_address != return_address && unsafe { root.slot.cast::<usize>().read() } != 0
    });
    #[cfg(debug_assertions)]
    let has_current_live_root = roots.slots().iter().any(|root| {
        root.return_address == return_address && unsafe { root.slot.cast::<usize>().read() } != 0
    });
    #[cfg(debug_assertions)]
    if (has_older_live_root && FORCE_NEXT_COLLECTION.with(|armed| armed.replace(false)))
        || (has_current_live_root
            && FORCE_NEXT_COLLECTION_WITH_LIVE_ROOT.with(|armed| armed.replace(false)))
    {
        // Snapshot the exact roots used by this forced collection, not an
        // earlier call whose mapped record fields may still have been null.
        LAST_PROBED_ROOTS.with(|last| {
            *last.borrow_mut() = Some(ProbedRoots {
                return_address,
                roots: roots
                    .slots()
                    .iter()
                    .map(|root| ProbedRoot {
                        return_address: root.return_address,
                        // SAFETY: these are validated suspended-frame root slots.
                        word: unsafe { root.slot.cast::<usize>().read() },
                    })
                    .collect(),
            });
        });
        let bytes_before = runtime.heap.allocated_bytes();
        runtime.collect(&mut roots);
        let bytes_after = runtime.heap.allocated_bytes();
        LAST_FORCED_COLLECTION.with(|last| {
            last.set(Some(ForcedCollection {
                bytes_before,
                bytes_after,
            }))
        });
    }

    let gc_ref = runtime
        .alloc_string(
            // SAFETY: the JIT passes a pointer to its registered string-literal
            // data and its length; the shim checked null and negative lengths.
            unsafe { std::slice::from_raw_parts(bytes, len) },
            &mut roots,
        )
        .unwrap_or_else(|error| {
            eprintln!("Hopper string allocation failed: {error:?}");
            std::process::abort();
        });

    gc_ref.as_ptr()
}

// Called only by the object allocation assembly shim; its extra arguments are
// not part of the Hopper-visible allocator ABI.
pub(super) extern "C" fn alloc_object_impl(
    size: i64,
    descriptor: *const TypeDescriptor,
    caller_sp: usize,
    return_address: usize,
    caller_fp: usize,
) -> RawGcRef {
    let size = usize::try_from(size).unwrap_or_else(|_| {
        eprintln!("Hopper managed-object allocation: negative size");
        std::process::abort();
    });
    if descriptor.is_null() {
        eprintln!("Hopper managed-object allocation: null descriptor");
        std::process::abort();
    }

    let runtime = crate::runtime();
    let mut runtime = runtime.lock().unwrap_or_else(|_| {
        eprintln!("Hopper managed-object allocation: runtime lock poisoned");
        std::process::abort();
    });
    let mut roots = StackRootSet::new(
        AllocationCallSite {
            caller_sp,
            return_address,
            caller_fp,
        },
        &runtime.stackmap_registry,
    );

    // SAFETY: the generated caller supplies a valid descriptor pointer whose
    // allocation metadata outlives the generated code using it.
    let descriptor = unsafe { &*descriptor };
    runtime
        .alloc_object(size, descriptor, &mut roots)
        .unwrap_or_else(|error| {
            eprintln!("Hopper managed-object allocation failed: {error:?}");
            std::process::abort();
        })
        .as_ptr()
}

pub extern "C" fn print_string(gc_ref: RawGcRef) {
    let str_ref = gc_ref.cast::<HopperString>();

    let s: &str = unsafe {
        let len = (*str_ref).len;
        let src = (*str_ref).bytes;
        let bytes = slice::from_raw_parts(src, len);
        str::from_utf8_unchecked(bytes)
    };
    println!("{s}");
}
