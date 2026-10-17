//! Precise roots in the active Hopper frames at a native allocation call.
//!
//! Requires Cranelift's preserved-frame-pointer ABI on supported targets.
//! Native frames are not walked: the first return address outside registered
//! Hopper code ends the walk.

use std::mem::{align_of, size_of};

/// Bounds of the currently running native thread stack (grows downward)
#[derive(Clone, Copy, Debug)]
struct StackBounds {
    low: usize,
    high: usize,
}

impl StackBounds {
    fn contains(self, addr: usize, len: usize) -> bool {
        addr >= self.low && addr.checked_add(len).is_some_and(|end| end <= self.high)
    }

    #[cfg(target_os = "macos")]
    fn current_thread() -> Self {
        // SAFETY: both pthread APIs query the calling thread
        // on macOS/AArch64, the stack base returned is the high address
        unsafe {
            let thread = libc::pthread_self();
            let high = libc::pthread_get_stackaddr_np(thread) as usize;
            let size = libc::pthread_get_stacksize_np(thread);
            let low = high
                .checked_sub(size)
                .expect("thread stack bounds underflow");
            assert!(size > 0, "thread stack bounds unavailable");
            Self { low, high }
        }
    }

    #[cfg(target_os = "linux")]
    fn current_thread() -> Self {
        use std::mem::MaybeUninit;
        use std::ptr;

        // SAFETY: pthread_getattr_np initializes attr for the current thread.
        // Destroy it after querying the stack base and size, including when
        // pthread_attr_getstack fails.
        unsafe {
            let mut attr = MaybeUninit::<libc::pthread_attr_t>::uninit();
            assert_eq!(
                libc::pthread_getattr_np(libc::pthread_self(), attr.as_mut_ptr()),
                0,
                "thread stack attributes unavailable"
            );
            let mut base = ptr::null_mut();
            let mut size = 0;
            let result = libc::pthread_attr_getstack(attr.as_ptr(), &mut base, &mut size);
            let destroy_result = libc::pthread_attr_destroy(attr.as_mut_ptr());
            assert_eq!(result, 0, "thread stack bounds unavailable");
            assert_eq!(
                destroy_result, 0,
                "could not destroy thread stack attributes"
            );
            assert!(size > 0, "empty thread stack");
            let low = base as usize;
            let high = low.checked_add(size).expect("thread stack bounds overflow");
            Self { low, high }
        }
    }
}

use crate::ffi::AllocationCallSite;
use crate::memory_recycling::{GcRef, RootSet, RootVisitor};
use crate::stackmaps::StackMapRegistry;

pub(crate) struct RootSlot {
    #[cfg(debug_assertions)]
    pub(crate) return_address: usize,
    pub(crate) slot: *mut GcRef,
}

pub(crate) struct StackRootSet {
    slots: Vec<RootSlot>,
}

impl StackRootSet {
    pub(crate) fn new(caller: AllocationCallSite, registry: &StackMapRegistry) -> Self {
        let bounds = StackBounds::current_thread();
        let mut slots = Vec::new();
        let mut fp = caller.caller_fp;
        let mut sp = caller.caller_sp;
        let mut pc = caller.return_address;

        loop {
            let range = registry
                .function_containing(pc)
                .expect("allocator caller must be registered Hopper code");
            assert!(
                bounds.contains(sp, 1),
                "Hopper SP outside current thread's stack"
            );
            assert!(
                bounds.contains(fp, 2 * size_of::<usize>()),
                "Hopper frame record outside current thread's stack"
            );
            assert_eq!(
                fp.checked_sub(sp),
                Some(range.frame_to_fp_offset as usize),
                "Hopper FP and safepoint SP disagree with frame layout"
            );

            // A safepoint with no live GC values may have no user stack map.
            if let Some(map) = registry.get(pc) {
                for &offset in &map.root_offsets {
                    let offset = offset as usize;
                    let end = offset
                        .checked_add(size_of::<GcRef>())
                        .expect("root slot offset overflow");
                    assert!(
                        end <= map.stack_span as usize,
                        "root exceeds stack-map span"
                    );
                    assert!(
                        end <= range.frame_to_fp_offset as usize,
                        "root exceeds Hopper frame below FP"
                    );
                    let address = sp.checked_add(offset).expect("root address overflow");
                    assert!(
                        bounds.contains(address, size_of::<GcRef>()),
                        "GC root outside current thread's stack"
                    );
                    assert_eq!(address % align_of::<GcRef>(), 0, "unaligned root slot");
                    slots.push(RootSlot {
                        #[cfg(debug_assertions)]
                        return_address: pc,
                        slot: address as *mut GcRef,
                    });
                }
            }

            assert_eq!(fp % align_of::<usize>(), 0, "unaligned frame pointer");
            // SAFETY: FP was captured at an active Hopper call or read from a
            // younger Hopper frame with frame pointers preserved. The saved FP
            // and return address occupy the first two words of the frame record
            // on the supported x86-64 and AArch64 ABIs. No native frame is read.
            let (parent_fp, parent_pc) = unsafe {
                let frame = fp as *const usize;
                (frame.read(), frame.add(1).read())
            };
            if registry.function_containing(parent_pc).is_none() {
                break;
            }
            assert!(
                parent_fp > fp,
                "Hopper frame-pointer chain is not increasing"
            );
            assert!(
                bounds.contains(parent_fp, 2 * size_of::<usize>()),
                "Hopper parent frame outside current thread's stack"
            );
            assert_eq!(
                parent_fp % align_of::<usize>(),
                0,
                "unaligned parent frame pointer"
            );
            let parent_range = registry.function_containing(parent_pc).unwrap();
            sp = parent_fp
                .checked_sub(parent_range.frame_to_fp_offset as usize)
                .expect("Hopper parent SP underflow");
            fp = parent_fp;
            pc = parent_pc;
        }

        Self { slots }
    }

    #[cfg(debug_assertions)]
    pub(crate) fn slots(&self) -> &[RootSlot] {
        &self.slots
    }
}

impl RootSet for StackRootSet {
    fn visit_roots(&mut self, visitor: &mut dyn RootVisitor) {
        for slot in &self.slots {
            visitor.visit_gc_ref(slot.slot);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_bounds_check_complete_words_and_overflow() {
        let bounds = StackBounds {
            low: 0x1000,
            high: 0x2000,
        };
        assert!(bounds.contains(0x1000, 8));
        assert!(bounds.contains(0x1ff8, 8));
        assert!(!bounds.contains(0x0fff, 8));
        assert!(!bounds.contains(0x1ffc, 8));
        assert!(!bounds.contains(usize::MAX - 4, 8));
    }

    #[test]
    fn current_thread_stack_contains_local_variable() {
        let word = 0usize;
        let address = &word as *const usize as usize;
        assert!(StackBounds::current_thread().contains(address, size_of::<usize>()));
    }
}
