#[cfg(debug_assertions)]
use std::cell::{Cell, RefCell};
use std::slice;

use crate::frame_walk::StackRootSet;
use crate::memory_recycling::{GcHeader, GcRef, RootSet, RootVisitor, TypeDescriptor};
use crate::string::HopperString;
use crate::{array, execution, runtime};

pub type RawGcRef = *mut GcHeader;

mod alloc_entry;
pub mod bool;
pub mod float;
mod frame_entry;
pub mod int;

/// JIT function check coming in, capturing the parent frame's return PC
pub fn enter_frame_entry() -> *const u8 {
    frame_entry::address()
}

/// JIT function check going out, before returning to the caller.
pub fn leave_frame_entry() -> *const u8 {
    leave_frame_impl as *const u8
}

pub(super) extern "C" fn enter_frame_impl(parent_return_address: usize) {
    let gate = execution::gate();
    let owns_gate = gate.owned_by_current_thread().unwrap_or_else(|error| {
        eprintln!("Hopper execution boundary: {error}");
        std::process::abort();
    });
    let parent_is_hopper = {
        let runtime_mutex = runtime();
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
    if let Err(error) = execution::gate().enter(parent_is_hopper) {
        eprintln!("Hopper execution boundary: {error}");
        std::process::abort();
    }
}

extern "C" fn leave_frame_impl() {
    if let Err(error) = execution::gate().leave() {
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

/// Entry point for String allocation that captures the root
pub fn alloc_string_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::address())
}

/// Entry point for String concatenation that captures the root
pub fn string_concat_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::string_concat_address())
}

/// Returns a String's UTF-8 byte length. This operation does not allocate.
pub extern "C" fn string_byte_len(gc_ref: RawGcRef) -> i64 {
    if gc_ref.is_null() {
        eprintln!("Hopper String byte length: null String reference");
        std::process::abort();
    }
    // SAFETY: Hopper String values are non-null references to runtime-owned
    // HopperString objects.
    unsafe { (*gc_ref.cast::<HopperString>()).len }
}

unsafe fn string_bytes<'a>(string: &'a HopperString) -> &'a [u8] {
    let len = usize::try_from(string.len).unwrap_or_else(|_| {
        eprintln!("Hopper String: invalid negative byte length");
        std::process::abort();
    });
    if string.bytes.is_null() {
        eprintln!("Hopper String: null byte buffer");
        std::process::abort();
    }
    // SAFETY: all String allocations own a non-null byte slice of this length.
    unsafe { std::slice::from_raw_parts(string.bytes, len) }
}

/// Compares Strings by their UTF-8 bytes. This operation does not allocate.
pub extern "C" fn string_eq(left: RawGcRef, right: RawGcRef) -> i64 {
    if left.is_null() || right.is_null() {
        eprintln!("Hopper String equality: null String reference");
        std::process::abort();
    }
    // SAFETY: Hopper String values are non-null references to runtime-owned
    // HopperString objects.
    unsafe {
        let left = &*left.cast::<HopperString>();
        let right = &*right.cast::<HopperString>();
        (left.len == right.len && string_bytes(left) == string_bytes(right)) as i64
    }
}

// Called only by the assembly shim, which captures the Hopper caller before
// entering Rust so any collection sees the caller's roots.
pub(super) extern "C" fn string_concat_impl(
    left: RawGcRef,
    right: RawGcRef,
    caller_sp: usize,
    return_address: usize,
    caller_fp: usize,
) -> RawGcRef {
    if left.is_null() || right.is_null() {
        eprintln!("Hopper String concatenation: null String reference");
        std::process::abort();
    }

    let runtime_mutex = runtime();
    let mut runtime = runtime_mutex.lock().unwrap_or_else(|_| {
        eprintln!("Hopper String concatenation: runtime lock poisoned");
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

    // Copy the bytes before allocating the resulting GC object: allocation
    // may collect, but both input handles remain rooted in the Hopper frame.
    let joined = unsafe {
        let left_string = &*left.cast::<HopperString>();
        let right_string = &*right.cast::<HopperString>();
        let left_bytes = string_bytes(left_string);
        let right_bytes = string_bytes(right_string);
        let capacity = left_bytes
            .len()
            .checked_add(right_bytes.len())
            .unwrap_or_else(|| {
                eprintln!("Hopper String concatenation: length overflow");
                std::process::abort();
            });
        let mut joined = Vec::new();
        joined.try_reserve_exact(capacity).unwrap_or_else(|_| {
            eprintln!("Hopper String concatenation: out of memory");
            std::process::abort();
        });
        joined.extend_from_slice(left_bytes);
        joined.extend_from_slice(right_bytes);
        joined
    };

    runtime
        .alloc_string(&joined, &mut roots)
        .unwrap_or_else(|error| {
            eprintln!("Hopper String concatenation failed: {error:?}");
            std::process::abort();
        })
        .as_ptr()
}

/// A descriptor-aware managed-object allocator entry point.
///
/// The Hopper-visible ABI is `(size: Int, descriptor: pointer) -> GcRef`.
/// `descriptor` must point to a live `TypeDescriptor` whose trace offsets and
/// destructor remain valid for the lifetime of the compiled code.
pub fn alloc_object_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::object_address())
}

pub fn array_new_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::array_new_address())
}

pub fn array_reserve_entry() -> GcEntryAddress {
    GcEntryAddress(alloc_entry::array_reserve_address())
}

pub fn array_write_next_entry() -> *const u8 {
    array_write_next_impl as *const u8
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

    let runtime = runtime();
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

    let runtime = runtime();
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

    #[cfg(debug_assertions)]
    {
        let has_older_live_root = roots.slots().iter().any(|root| {
            root.return_address != return_address
                && unsafe { root.slot.cast::<usize>().read() } != 0
        });
        let has_current_live_root = roots.slots().iter().any(|root| {
            root.return_address == return_address
                && unsafe { root.slot.cast::<usize>().read() } != 0
        });
        if (has_older_live_root && FORCE_NEXT_COLLECTION.with(|armed| armed.replace(false)))
            || (has_current_live_root
                && FORCE_NEXT_COLLECTION_WITH_LIVE_ROOT.with(|armed| armed.replace(false)))
        {
            LAST_PROBED_ROOTS.with(|last| {
                *last.borrow_mut() = Some(ProbedRoots {
                    return_address,
                    roots: roots
                        .slots()
                        .iter()
                        .map(|root| ProbedRoot {
                            return_address: root.return_address,
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
    }

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

pub(super) extern "C" fn array_new_impl(
    element_size: i64,
    capacity: i64,
    descriptor: *const TypeDescriptor,
    caller_sp: usize,
    return_address: usize,
    caller_fp: usize,
) -> RawGcRef {
    let element_size = usize::try_from(element_size).unwrap_or_else(|_| {
        eprintln!("Array allocation: invalid element size");
        std::process::abort();
    });
    let capacity = usize::try_from(capacity).unwrap_or_else(|_| {
        eprintln!("Array allocation: invalid capacity");
        std::process::abort();
    });
    if descriptor.is_null() || element_size == 0 {
        eprintln!("Array allocation: invalid arguments");
        std::process::abort();
    }

    let runtime = runtime();
    let mut runtime = runtime.lock().unwrap_or_else(|_| {
        eprintln!("Array allocation: runtime lock poisoned");
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
    let descriptor = unsafe { &*descriptor };
    let owner = runtime
        .alloc_object(
            std::mem::size_of::<array::HopperArray>(),
            &array::ARRAY_OWNER_DESCRIPTOR,
            &mut roots,
        )
        .unwrap_or_else(|error| {
            eprintln!("Array owner allocation failed: {error:?}");
            std::process::abort();
        });
    let mut owner_root = owner.as_ptr();
    let buffer_size = array_buffer_size(capacity, element_size);
    let mut roots = StackAndLocalRoots {
        stack: &mut roots,
        local: &mut owner_root,
    };
    let buffer = runtime
        .alloc_object(buffer_size, descriptor, &mut roots)
        .unwrap_or_else(|error| {
            eprintln!("Array buffer allocation failed: {error:?}");
            std::process::abort();
        });
    unsafe {
        // Use the rooted local rather than the original allocation result so
        // this remains correct if the collector ever becomes moving.
        let owner = owner_root.cast::<array::HopperArray>();
        let buffer = buffer.as_ptr().cast::<array::HopperArrayBuffer>();
        (*buffer).length = 0;
        (*buffer).capacity = capacity;
        (*owner).buffer = buffer.cast::<GcHeader>();
    }
    owner_root
}

pub(super) extern "C" fn array_reserve_impl(
    array_ref: RawGcRef,
    requested_capacity: i64,
    element_size: i64,
    descriptor: *const TypeDescriptor,
    caller_sp: usize,
    return_address: usize,
    caller_fp: usize,
) {
    let requested_capacity = usize::try_from(requested_capacity).unwrap_or_else(|_| {
        eprintln!("Array reserve: invalid capacity");
        std::process::abort();
    });
    let element_size = usize::try_from(element_size).unwrap_or_else(|_| {
        eprintln!("Array reserve: invalid element size");
        std::process::abort();
    });
    if array_ref.is_null() || descriptor.is_null() || element_size == 0 {
        eprintln!("Array reserve: invalid arguments");
        std::process::abort();
    }

    let runtime_mutex = runtime();
    let mut runtime = runtime_mutex.lock().unwrap_or_else(|_| {
        eprintln!("Array reserve: runtime lock poisoned");
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
    {
        let has_current_live_root = roots.slots().iter().any(|root| {
            root.return_address == return_address
                && unsafe { root.slot.cast::<usize>().read() } != 0
        });
        if has_current_live_root
            && FORCE_NEXT_COLLECTION_WITH_LIVE_ROOT.with(|armed| armed.replace(false))
        {
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
    }
    let owner = array_ref.cast::<array::HopperArray>();
    let (old_buffer, length, capacity) = unsafe {
        let buffer = (*owner).buffer.cast::<array::HopperArrayBuffer>();
        if buffer.is_null() {
            eprintln!("Array reserve: uninitialized Array handle");
            std::process::abort();
        }
        ((*owner).buffer, (*buffer).length, (*buffer).capacity)
    };
    if requested_capacity <= capacity {
        return;
    }

    let size = array_buffer_size(requested_capacity, element_size);
    let descriptor = unsafe { &*descriptor };
    let new_buffer = runtime
        .alloc_object(size, descriptor, &mut roots)
        .unwrap_or_else(|error| {
            eprintln!("Array reserve allocation failed: {error:?}");
            std::process::abort();
        });
    unsafe {
        let old_buffer = old_buffer.cast::<array::HopperArrayBuffer>();
        let new_buffer_ptr = new_buffer.as_ptr().cast::<array::HopperArrayBuffer>();
        (*new_buffer_ptr).length = length;
        (*new_buffer_ptr).capacity = requested_capacity;
        std::ptr::copy_nonoverlapping(
            (*old_buffer).elements.as_ptr(),
            (*new_buffer_ptr).elements.as_mut_ptr(),
            length * element_size,
        );
        (*owner).buffer = new_buffer.as_ptr();
    }
}

pub(super) extern "C" fn array_write_next_impl(
    array_ref: RawGcRef,
    index: i64,
    value: *const u8,
    element_size: i64,
) {
    let index = usize::try_from(index).unwrap_or_else(|_| {
        eprintln!("Array write: invalid index");
        std::process::abort();
    });
    let element_size = usize::try_from(element_size).unwrap_or_else(|_| {
        eprintln!("Array write: invalid element size");
        std::process::abort();
    });
    if array_ref.is_null() || value.is_null() || element_size == 0 {
        eprintln!("Array write: invalid arguments");
        std::process::abort();
    }

    let owner = array_ref.cast::<array::HopperArray>();
    unsafe {
        let buffer = (*owner).buffer.cast::<array::HopperArrayBuffer>();
        if buffer.is_null() || index != (*buffer).length || index >= (*buffer).capacity {
            eprintln!("Array write: append index out of bounds");
            std::process::abort();
        }
        std::ptr::copy_nonoverlapping(
            value,
            (*buffer).elements.as_mut_ptr().add(index * element_size),
            element_size,
        );
        // The collector scans elements only through length, so publish length
        // after the new element's bytes (including any GC references) exist.
        (*buffer).length = index + 1;
    }
}

fn array_buffer_size(capacity: usize, element_size: usize) -> usize {
    array::ARRAY_BUFFER_ELEMENTS_OFFSET
        .checked_add(capacity.checked_mul(element_size).unwrap_or_else(|| {
            eprintln!("Array allocation size overflow");
            std::process::abort();
        }))
        .filter(|size| *size <= u32::MAX as usize)
        .unwrap_or_else(|| {
            eprintln!("Array allocation size overflow");
            std::process::abort();
        })
}

struct StackAndLocalRoots<'a> {
    stack: &'a mut dyn RootSet,
    local: &'a mut RawGcRef,
}

impl RootSet for StackAndLocalRoots<'_> {
    fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
        self.stack.visit_roots(visitor);
        visitor.visit_gc_ref((self.local as *mut RawGcRef).cast::<GcRef>());
    }
}

pub extern "C" fn print_string(gc_ref: RawGcRef) {
    let str_ref = gc_ref.cast::<HopperString>();

    let s: &str = unsafe {
        let len = usize::try_from((*str_ref).len).expect("valid Hopper String length");
        let src = (*str_ref).bytes;
        let bytes = slice::from_raw_parts(src, len);
        str::from_utf8_unchecked(bytes)
    };
    println!("{s}");
}
