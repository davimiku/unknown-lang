use std::{fmt, io};

// TODO: newtype all of these and impl io::Write?
// TODO: once there is a name for the language, change these names
// 'X' is a placeholder

/// Language `Int` is a Rust `i64`
pub(crate) type XInt = i64;

/// Language `Float` is a Rust `f64`
pub(crate) type XFloat = f64;

/// Language `Bool` is a Rust `i64`
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct XBool(i64);

pub const TRUE: XBool = XBool(1);
pub const FALSE: XBool = XBool(0);

impl From<bool> for XBool {
    fn from(value: bool) -> Self {
        match value {
            true => TRUE,
            false => FALSE,
        }
    }
}

impl From<XBool> for bool {
    fn from(value: XBool) -> Self {
        match value.0 {
            0 => false,
            1 => true,
            _ => unreachable!(),
        }
    }
}

impl From<&XBool> for bool {
    fn from(value: &XBool) -> Self {
        (*value).into()
    }
}

impl fmt::Display for XBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bool_self: bool = self.into();
        bool_self.fmt(f)
    }
}

impl io::Write for XBool {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        todo!()
    }

    fn flush(&mut self) -> io::Result<()> {
        todo!()
    }
}

pub(crate) const PRINT_INT: &str = "__print_int";
pub(crate) const PRINT_FLOAT: &str = "__print_float";
pub(crate) const PRINT_BOOL: &str = "__print_bool";

/// Prints an integer to stdout with a newline
pub(crate) extern "C" fn print_int(i: XInt) {
    // FIXME: leaving this for example, but should use io::Write impl instead
    // because we don't need any of the formatting stuff
    // let bytes = i.to_string().as_bytes();
    // let s = std::io::stdout()
    //     .lock()
    //     .write(&bytes)
    //     .expect("succeeded writing bytes");
    println!("{i}");
}

/// Prints a float to stdout with a newline
pub(crate) extern "C" fn print_float(f: XFloat) {
    println!("{f}");
}

/// Prints a bool to stdout with a newline
pub(crate) extern "C" fn print_bool(b: XBool) {
    println!("{b}");
}
