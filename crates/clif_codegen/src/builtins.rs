use std::fmt;
use std::io::Write;

// TODO: newtype all of these and impl fmt::display?
// TODO: once there is a name for the language, change these names
// 'X' is a placeholder

/// Language `Int` is a Rust `i64`
pub(crate) type XInt = i64;

/// Language `Float` is a Rust `f64`
pub(crate) type XFloat = f64;

/// Language `Bool` is a Rust `i64`
///
/// repr(transparent) is mandatory for FFI through "extern "C"" functions
/// once repr(crabi) is stabilized, we would use that and implement that repr
/// in the codegen here too
/// https://github.com/rust-lang/rust/pull/105586
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct XBool(i64);

pub const TRUE: XBool = XBool(1);
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

/// Prints an integer to stdout with a newline
pub(crate) extern "C" fn print_int(i: XInt) {
    let mut s = i.to_string();
    s.push('\n');
    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(s.as_bytes()).expect("succeeded writing bytes");
    let _ = stdout.flush();
}

/// Prints a float to stdout with a newline
// TODO: replace fmt::to_string() with ryu?
// https://lib.rs/crates/ryu
pub(crate) extern "C" fn print_float(f: XFloat) {
    let mut s = f.to_string();
    s.push('\n');

    let stdout = &mut std::io::stdout().lock();
    let _ = stdout.write(s.as_bytes()).expect("succeeded writing bytes");
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
