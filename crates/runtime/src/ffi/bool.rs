//! Types and functions related to interop between a Hopper `Bool`
//! and the equivalent Rust type

use std::fmt;

/// Language `Bool` is a unit union type which is represented as a Rust `i64`
///
/// repr(transparent) is mandatory for FFI through "extern "C"" functions
/// once repr(crabi) is stabilized, we would use that and implement that repr
/// in the codegen here too
/// https://github.com/rust-lang/rust/pull/105586
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HopperBool(i64);

impl Default for HopperBool {
    fn default() -> Self {
        FALSE
    }
}

/// Use this constant for "true" when doing any FFI from Rust
pub const TRUE: HopperBool = HopperBool(1);
/// Use this constant for "false" when going any FFI from Rust
pub const FALSE: HopperBool = HopperBool(0);

impl From<bool> for HopperBool {
    fn from(value: bool) -> Self {
        Self(value as i64)
    }
}

impl From<HopperBool> for bool {
    fn from(value: HopperBool) -> Self {
        value.0 != 0
    }
}

impl From<&HopperBool> for bool {
    fn from(value: &HopperBool) -> Self {
        (*value).into()
    }
}

impl fmt::Display for HopperBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let b: bool = self.into();
        write!(f, "{b}")
    }
}
