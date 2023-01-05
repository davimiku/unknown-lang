use crate::VM;

// TODO: move to a shared "builtins" crate
// remove duplication with codegen crate
pub const PRINT_STR_CONSTANT: u8 = 0;
pub const PRINT_STR: u8 = 1;
pub const PRINT_INT: u8 = 2;
pub const PRINT_FLOAT: u8 = 3;
pub const PRINT_BOOL: u8 = 4;

impl VM<'_> {
    pub(crate) fn print_str_constant(&mut self) {
        let (idx, len) = self.stack.pop_string_literal();

        let s = self.chunk.get_str_constant(idx, len);
        println!("{s}");
    }

    pub(crate) fn print_int(&mut self) {
        let int = self.stack.pop_int();

        println!("{int}");
    }

    pub(crate) fn print_float(&mut self) {
        let float = self.stack.pop_float();

        println!("{float}");
    }

    pub(crate) fn print_bool(&mut self) {
        let b = self.stack.pop_bool();

        println!("{b}");
    }
}
