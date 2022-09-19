use crate::VM;

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
