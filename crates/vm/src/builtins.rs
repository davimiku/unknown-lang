use vm_types::XInt;

use crate::VM;

// TODO: move to a shared "builtins" crate
// remove duplication with codegen crate
pub const PRINT_STRING: u8 = 1;
pub const PRINT_INT: u8 = 2;
pub const PRINT_FLOAT: u8 = 3;
pub const PRINT_BOOL: u8 = 4;

pub const LEN_STRING: u8 = 5;

impl VM {
    pub(crate) fn print_string(&mut self) {
        let s = self.stack.pop_string();

        let s = self.deref_string(s);

        println!("{s}");
    }

    pub(crate) fn print_int(&mut self) {
        let int = self.stack.pop_int();

        println!("{int}");
    }

    pub(crate) fn print_float(&mut self) {
        let float = self.stack.pop_float();

        println!("{float:?}");
    }

    pub(crate) fn print_bool(&mut self) {
        let b = self.stack.pop_bool();

        println!("{}", b != 0);
    }

    pub(crate) fn len_string(&mut self) {
        let s = self.stack.pop_string();

        let len = s.length();

        // TODO: should length be u32 or i32?
        self.stack.push_int(len as XInt);
    }
}
