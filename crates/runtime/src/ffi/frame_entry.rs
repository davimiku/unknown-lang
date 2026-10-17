//! Capture the return PC saved in the *current Hopper frame* before calling
//! into Rust. This is a dedicated execution-boundary check, not a general
//! trampoline for native interop.

use std::arch::global_asm;

#[cfg(not(any(
    all(target_os = "macos", target_arch = "aarch64"),
    all(target_os = "macos", target_arch = "x86_64"),
    all(target_os = "linux", target_arch = "aarch64"),
    all(target_os = "linux", target_arch = "x86_64"),
)))]
compile_error!("Hopper frame entry requires macOS or Linux on AArch64 or x86-64");

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
global_asm!(
    ".globl _hopper_enter_frame",
    "_hopper_enter_frame:",
    "ldr x0, [x29, #8]",
    "b {impl_fn}",
    impl_fn = sym super::enter_frame_impl,
);

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
global_asm!(
    ".globl hopper_enter_frame",
    "hopper_enter_frame:",
    "ldr x0, [x29, #8]",
    "b {impl_fn}",
    impl_fn = sym super::enter_frame_impl,
);

#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
global_asm!(
    ".globl _hopper_enter_frame",
    "_hopper_enter_frame:",
    "mov rdi, [rbp + 8]",
    "jmp {impl_fn}",
    impl_fn = sym super::enter_frame_impl,
);

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
global_asm!(
    ".globl hopper_enter_frame",
    "hopper_enter_frame:",
    "mov rdi, [rbp + 8]",
    "jmp {impl_fn}",
    impl_fn = sym super::enter_frame_impl,
);

unsafe extern "C" {
    fn hopper_enter_frame();
}

pub(super) fn address() -> *const u8 {
    hopper_enter_frame as *const u8
}
