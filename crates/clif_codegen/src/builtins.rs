use std::{fmt, io::Write};

// TODO: newtype all of these and impl io::Write

/// Language `Int` is a Rust `i64`
pub(crate) type XInt = i64;

/// Language `Float` is a Rust `f64`
pub(crate) type XFloat = f64;

/// Language `Bool` is a Rust `i64`
#[repr(i64)]
#[derive(Default, Debug)]
pub(crate) enum XBool {
    #[default]
    False,
    True,
}

impl fmt::Display for XBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            XBool::False => f.write_str("false"),
            XBool::True => f.write_str("true"),
        }
    }
}

pub(crate) const PRINT_INT: &str = "__print_int";
pub(crate) const PRINT_FLOAT: &str = "__print_float";
pub(crate) const PRINT_BOOL: &str = "__print_bool";

/// Prints an integer to stdout with a newline
pub(crate) extern "C" fn print_int(i: XInt) {
    // FIXME: leaving this for example, but should use io::Write impl instead
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
