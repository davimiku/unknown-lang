use std::io::Write;

use cranelift::prelude::types::{F64, I64};
use cranelift::prelude::Type;
use runtime::ffi::bool::HopperBool;
use runtime::ffi::float::HopperFloat;

// TODO: newtype all of these and impl fmt::display?
// TODO: once there is a name for the language, change these names
// 'X' is a placeholder

pub(crate) const PRINT_INT: &str = "__print_int";
pub(crate) const PRINT_FLOAT: &str = "__print_float";
pub(crate) const PRINT_BOOL: &str = "__print_bool";

pub(crate) const ALLOC_STRING: &str = "__alloc_string";
pub(crate) const ALLOC_OBJECT: &str = "__alloc_object";
pub(crate) const PRINT_STRING: &str = "__print_string";
pub(crate) const ENTER_FRAME: &str = "__hopper_enter_frame";
pub(crate) const LEAVE_FRAME: &str = "__hopper_leave_frame";
pub(crate) const ARRAY_NEW: &str = "__array_new";
pub(crate) const ARRAY_RESERVE: &str = "__array_reserve";
pub(crate) const ARRAY_WRITE_NEXT: &str = "__array_write_next";
pub(crate) const STRING_BYTE_LEN: &str = "__string_byte_len";
pub(crate) const STRING_CONCAT: &str = "__string_concat";
pub(crate) const STRING_EQ: &str = "__string_eq";

/// Only `MayCollect` entries may invoke the collector.
/// Its entry address can only come from a runtime shim that captures the Hopper caller.
/// `NoGc` means no collection by the Hopper runtime, not that there's no allocation by Rust.
#[derive(Clone, Copy)]
pub(crate) enum NativeEntry {
    NoGc(*const u8),
    MayCollect(runtime::ffi::GcEntryAddress),
}

impl NativeEntry {
    pub(crate) fn address(self) -> *const u8 {
        match self {
            Self::NoGc(address) => address,
            Self::MayCollect(entry) => entry.as_ptr(),
        }
    }
}

pub(crate) struct NativeBuiltin {
    pub name: &'static str,
    pub params: &'static [Type],
    pub returns: &'static [Type],
    pub entry: NativeEntry,
}

/// Contains the ABI, GC effect, and JIT symbol table for native builtins.
///
/// This may change in the future, we don't yet have a way for users to
/// register custom native entry points and it's not prioritized at the moment
pub(crate) fn native_builtins() -> [NativeBuiltin; 14] {
    [
        NativeBuiltin {
            name: ENTER_FRAME,
            params: &[],
            returns: &[],
            entry: NativeEntry::NoGc(runtime::ffi::enter_frame_entry()),
        },
        NativeBuiltin {
            name: LEAVE_FRAME,
            params: &[],
            returns: &[],
            entry: NativeEntry::NoGc(runtime::ffi::leave_frame_entry()),
        },
        NativeBuiltin {
            name: ALLOC_STRING,
            params: &[I64, I64],
            returns: &[I64],
            entry: NativeEntry::MayCollect(runtime::ffi::alloc_string_entry()),
        },
        NativeBuiltin {
            name: ALLOC_OBJECT,
            params: &[I64, I64],
            returns: &[I64],
            entry: NativeEntry::MayCollect(runtime::ffi::alloc_object_entry()),
        },
        NativeBuiltin {
            name: PRINT_STRING,
            params: &[I64],
            returns: &[],
            entry: NativeEntry::NoGc(runtime::ffi::print_string as *const u8),
        },
        NativeBuiltin {
            name: PRINT_INT,
            params: &[I64],
            returns: &[],
            entry: NativeEntry::NoGc(print_int as *const u8),
        },
        NativeBuiltin {
            name: PRINT_FLOAT,
            params: &[F64],
            returns: &[],
            entry: NativeEntry::NoGc(print_float as *const u8),
        },
        NativeBuiltin {
            name: PRINT_BOOL,
            params: &[I64],
            returns: &[],
            entry: NativeEntry::NoGc(print_bool as *const u8),
        },
        NativeBuiltin {
            name: ARRAY_NEW,
            params: &[I64, I64, I64],
            returns: &[I64],
            entry: NativeEntry::MayCollect(runtime::ffi::array_new_entry()),
        },
        NativeBuiltin {
            name: ARRAY_RESERVE,
            params: &[I64, I64, I64, I64],
            returns: &[],
            entry: NativeEntry::MayCollect(runtime::ffi::array_reserve_entry()),
        },
        NativeBuiltin {
            name: ARRAY_WRITE_NEXT,
            params: &[I64, I64, I64, I64],
            returns: &[],
            entry: NativeEntry::NoGc(runtime::ffi::array_write_next_entry()),
        },
        NativeBuiltin {
            name: STRING_BYTE_LEN,
            params: &[I64],
            returns: &[I64],
            entry: NativeEntry::NoGc(runtime::ffi::string_byte_len as *const u8),
        },
        NativeBuiltin {
            name: STRING_CONCAT,
            params: &[I64, I64],
            returns: &[I64],
            entry: NativeEntry::MayCollect(runtime::ffi::string_concat_entry()),
        },
        NativeBuiltin {
            name: STRING_EQ,
            params: &[I64, I64],
            returns: &[I64],
            entry: NativeEntry::NoGc(runtime::ffi::string_eq as *const u8),
        },
    ]
}

/// Prints an integer to stdout with a newline
pub(crate) extern "C" fn print_int(i: HopperFloat) {
    let s = i.to_string();
    let buf = s.as_bytes();
    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(buf).expect("succeeded writing bytes");
    let _ = stdout.write(b"\n").expect("succeeded writing newline");
    let _ = stdout.flush();
}

/// Prints a float to stdout with a newline
pub(crate) extern "C" fn print_float(f: HopperFloat) {
    let mut buffer = ryu::Buffer::new();
    let s = buffer.format(f);

    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(s.as_bytes()).expect("succeeded writing bytes");
    let _ = stdout.write(b"\n").expect("succeeded writing newline");
    let _ = stdout.flush();
}

/// Prints a bool to stdout with a newline
pub(crate) extern "C" fn print_bool(b: HopperBool) {
    let b: bool = b.into();
    let mut s = b.to_string();
    s.push('\n');

    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(s.as_bytes()).expect("succeeded writing bytes");
    let _ = stdout.flush();
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn native_builtins_have_unique_names_and_explicit_gc_effects() {
        let mut names = HashSet::new();
        let mut collecting = Vec::new();
        for builtin in native_builtins() {
            assert!(names.insert(builtin.name), "duplicate native builtin");
            assert!(!builtin.entry.address().is_null());
            if matches!(builtin.entry, NativeEntry::MayCollect(_)) {
                collecting.push(builtin.name);
            }
        }
        assert_eq!(
            collecting,
            [
                ALLOC_STRING,
                ALLOC_OBJECT,
                ARRAY_NEW,
                ARRAY_RESERVE,
                STRING_CONCAT
            ]
        );

        let string_allocator = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == ALLOC_STRING)
            .unwrap();
        assert_eq!(string_allocator.params, &[I64, I64]);
        assert_eq!(string_allocator.returns, &[I64]);
        assert_eq!(
            string_allocator.entry.address(),
            runtime::ffi::alloc_string_entry().as_ptr()
        );

        let object_allocator = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == ALLOC_OBJECT)
            .unwrap();
        assert_eq!(object_allocator.params, &[I64, I64]);
        assert_eq!(object_allocator.returns, &[I64]);
        assert_eq!(
            object_allocator.entry.address(),
            runtime::ffi::alloc_object_entry().as_ptr()
        );

        let array_new = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == ARRAY_NEW)
            .unwrap();
        assert_eq!(array_new.params, &[I64, I64, I64]);
        assert_eq!(array_new.returns, &[I64]);
        assert_eq!(
            array_new.entry.address(),
            runtime::ffi::array_new_entry().as_ptr()
        );

        let array_reserve = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == ARRAY_RESERVE)
            .unwrap();
        assert_eq!(array_reserve.params, &[I64, I64, I64, I64]);
        assert!(array_reserve.returns.is_empty());
        assert_eq!(
            array_reserve.entry.address(),
            runtime::ffi::array_reserve_entry().as_ptr()
        );

        let array_write_next = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == ARRAY_WRITE_NEXT)
            .unwrap();
        assert_eq!(array_write_next.params, &[I64, I64, I64, I64]);
        assert!(array_write_next.returns.is_empty());
        assert_eq!(
            array_write_next.entry.address(),
            runtime::ffi::array_write_next_entry()
        );

        let string_byte_len = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == STRING_BYTE_LEN)
            .unwrap();
        assert_eq!(string_byte_len.params, &[I64]);
        assert_eq!(string_byte_len.returns, &[I64]);
        assert_eq!(
            string_byte_len.entry.address(),
            runtime::ffi::string_byte_len as *const u8
        );

        let string_concat = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == STRING_CONCAT)
            .unwrap();
        assert_eq!(string_concat.params, &[I64, I64]);
        assert_eq!(string_concat.returns, &[I64]);
        assert!(matches!(string_concat.entry, NativeEntry::MayCollect(_)));
        assert_eq!(
            string_concat.entry.address(),
            runtime::ffi::string_concat_entry().as_ptr()
        );

        let string_eq = native_builtins()
            .into_iter()
            .find(|builtin| builtin.name == STRING_EQ)
            .unwrap();
        assert_eq!(string_eq.params, &[I64, I64]);
        assert_eq!(string_eq.returns, &[I64]);
        assert_eq!(
            string_eq.entry.address(),
            runtime::ffi::string_eq as *const u8
        );
    }
}
