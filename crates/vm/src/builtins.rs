use vm_types::{string::VMString, VMBool, VMFloat, VMInt};

// TODO: move to a shared "builtins" crate
// remove duplication with vm_codegen crate
pub const PRINT_STRING: u8 = 1;
pub const PRINT_INT: u8 = 2;
pub const PRINT_FLOAT: u8 = 3;
pub const PRINT_BOOL: u8 = 4;

pub const LEN_STRING: u8 = 5;

#[inline]
pub(crate) fn print_string(s: String) {
    println!("{s}");
}

#[inline]
pub(crate) fn print_int(int: VMInt) {
    println!("{int}");
}

#[inline]
pub(crate) fn print_float(float: VMFloat) {
    println!("{float:?}");
}

#[inline]
pub(crate) fn print_bool(b: VMBool) {
    println!("{}", b != 0);
}

#[inline]
pub(crate) fn len_string(s: VMString) -> VMInt {
    s.length()
}
