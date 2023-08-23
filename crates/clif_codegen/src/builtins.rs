use std::fmt;

/// Language `Float` is a Rust `i64`
pub(crate) type XInt = i64;

/// Language `Float` is a Rust `f64`
pub(crate) type XFloat = f64;

/// Language `Bool` is a Rust `i64`
pub(crate) type XBool = i64;

#[repr(i64)]
#[derive(Default, Debug)]
pub(crate) enum Bool {
    #[default]
    False,
    True,
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Bool::False => f.write_str("false"),
            Bool::True => f.write_str("true"),
        }
    }
}

pub(crate) const PRINT_INT: &str = "__print_int";
pub(crate) const PRINT_FLOAT: &str = "__print_float";
pub(crate) const PRINT_BOOL: &str = "__print_bool";

pub(crate) fn print_int(i: XInt) {
    println!("{i}");
}

pub(crate) fn print_float(f: XFloat) {
    println!("{f}");
}

pub(crate) fn print_bool(b: Bool) {
    println!("{b}");
}
