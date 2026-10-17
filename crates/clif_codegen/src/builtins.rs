use std::fmt;
use std::io::Write;

use cranelift::prelude::types::{F64, I64};
use cranelift::prelude::Type;

// TODO: newtype all of these and impl fmt::display?
// TODO: once there is a name for the language, change these names
// 'X' is a placeholder

/// Language `Int` is a Rust `i64`
pub(crate) type XInt = i64;

/// Language `Float` is a Rust `f64`
pub(crate) type XFloat = f64;

/// Useful alias for internal tests and other usage, the tag of
/// a tagged union is a Rust `i64`
pub(crate) type XTag = i64;

/// Language `Bool` is a unit union type which is represented as a Rust `i64`
///
/// repr(transparent) is mandatory for FFI through "extern "C"" functions
/// once repr(crabi) is stabilized, we would use that and implement that repr
/// in the codegen here too
/// https://github.com/rust-lang/rust/pull/105586
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct XBool(i64);

impl Default for XBool {
    fn default() -> Self {
        FALSE
    }
}

/// Use this constant for "true" when doing any FFI from Rust
pub const TRUE: XBool = XBool(1);
/// Use this constant for "false" when going any FFI from Rust
pub const FALSE: XBool = XBool(0);

impl From<bool> for XBool {
    fn from(value: bool) -> Self {
        Self(value as i64)
    }
}

impl From<XBool> for bool {
    fn from(value: XBool) -> Self {
        value.0 != 0
    }
}

impl From<&XBool> for bool {
    fn from(value: &XBool) -> Self {
        (*value).into()
    }
}

impl fmt::Display for XBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let b: bool = self.into();
        write!(f, "{b}")
    }
}

pub(crate) const PRINT_INT: &str = "__print_int";
pub(crate) const PRINT_FLOAT: &str = "__print_float";
pub(crate) const PRINT_BOOL: &str = "__print_bool";

pub(crate) const ALLOC_STRING: &str = "__alloc_string";
pub(crate) const PRINT_STRING: &str = "__print_string";
pub(crate) const ENTER_FRAME: &str = "__hopper_enter_frame";
pub(crate) const LEAVE_FRAME: &str = "__hopper_leave_frame";

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
pub(crate) fn native_builtins() -> [NativeBuiltin; 7] {
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
    ]
}

/// Prints an integer to stdout with a newline
pub(crate) extern "C" fn print_int(i: XInt) {
    let s = i.to_string();
    let buf = s.as_bytes();
    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(buf).expect("succeeded writing bytes");
    let _ = stdout.write(b"\n").expect("succeeded writing newline");
    let _ = stdout.flush();
}

/// Prints a float to stdout with a newline
pub(crate) extern "C" fn print_float(f: XFloat) {
    let mut buffer = ryu::Buffer::new();
    let s = buffer.format(f);

    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(s.as_bytes()).expect("succeeded writing bytes");
    let _ = stdout.write(b"\n").expect("succeeded writing newline");
    let _ = stdout.flush();
}

/// Prints a bool to stdout with a newline
pub(crate) extern "C" fn print_bool(b: XBool) {
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
        assert_eq!(collecting, [ALLOC_STRING]);

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
    }
}
