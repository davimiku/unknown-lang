//! Native entry point for calls from generated Hopper code.
//!
//! This is a function called by the Hopper runtime before any Rust function
//! creates its own stack frame.
//!
//! It tail-branches to Rust with the Hopper caller's SP, return address, and FP
//! passed as additional arguments (C ABI).

use std::arch::global_asm;

// We don't support Windows yet or non-mainstream architectures
#[cfg(not(any(
    all(target_os = "macos", target_arch = "aarch64"),
    all(target_os = "macos", target_arch = "x86_64"),
    all(target_os = "linux", target_arch = "aarch64"),
    all(target_os = "linux", target_arch = "x86_64"),
)))]
compile_error!("Hopper GC allocation entry requires macOS or Linux on AArch64 or x86-64");

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
global_asm!(
    ".globl _hopper_alloc_string_entry",
    "_hopper_alloc_string_entry:",
    "mov x2, sp",
    "mov x3, x30",
    "mov x4, x29",
    "b {impl_fn}",
    impl_fn = sym super::alloc_string_impl,
);

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
global_asm!(
    ".globl _hopper_alloc_object_entry",
    "_hopper_alloc_object_entry:",
    "mov x2, sp",
    "mov x3, x30",
    "mov x4, x29",
    "b {impl_fn}",
    impl_fn = sym super::alloc_object_impl,
);

#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
global_asm!(
    ".globl _hopper_alloc_string_entry",
    "_hopper_alloc_string_entry:",
    "lea rdx, [rsp + 8]",
    "mov rcx, [rsp]",
    "mov r8, rbp",
    "jmp {impl_fn}",
    impl_fn = sym super::alloc_string_impl,
);

#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
global_asm!(
    ".globl _hopper_alloc_object_entry",
    "_hopper_alloc_object_entry:",
    "lea rdx, [rsp + 8]",
    "mov rcx, [rsp]",
    "mov r8, rbp",
    "jmp {impl_fn}",
    impl_fn = sym super::alloc_object_impl,
);

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
global_asm!(
    ".globl hopper_alloc_string_entry",
    "hopper_alloc_string_entry:",
    "mov x2, sp",
    "mov x3, x30",
    "mov x4, x29",
    "b {impl_fn}",
    impl_fn = sym super::alloc_string_impl,
);

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
global_asm!(
    ".globl hopper_alloc_object_entry",
    "hopper_alloc_object_entry:",
    "mov x2, sp",
    "mov x3, x30",
    "mov x4, x29",
    "b {impl_fn}",
    impl_fn = sym super::alloc_object_impl,
);

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
global_asm!(
    ".globl hopper_alloc_string_entry",
    "hopper_alloc_string_entry:",
    "lea rdx, [rsp + 8]",
    "mov rcx, [rsp]",
    "mov r8, rbp",
    "jmp {impl_fn}",
    impl_fn = sym super::alloc_string_impl,
);

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
global_asm!(
    ".globl hopper_alloc_object_entry",
    "hopper_alloc_object_entry:",
    "lea rdx, [rsp + 8]",
    "mov rcx, [rsp]",
    "mov r8, rbp",
    "jmp {impl_fn}",
    impl_fn = sym super::alloc_object_impl,
);

unsafe extern "C" {
    #[link_name = "hopper_alloc_string_entry"]
    fn alloc_string_entry_symbol(bytes: *const u8, len: i64) -> *mut u8;

    #[link_name = "hopper_alloc_object_entry"]
    fn alloc_object_entry_symbol(size: i64, descriptor: *const u8) -> *mut u8;
}

pub(super) fn address() -> *const u8 {
    alloc_string_entry_symbol as *const u8
}

pub(super) fn object_address() -> *const u8 {
    alloc_object_entry_symbol as *const u8
}
