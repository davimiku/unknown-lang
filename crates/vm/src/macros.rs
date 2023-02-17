/// Implements arithmetic operations for integers
#[macro_export]
macro_rules! int_bin_op {
    ($self: ident, $f: ident) => {{
        let b = $self.stack.pop_int();
        let a = $self.stack.pop_int();

        let res = a.$f(b);

        $self.stack.push_int(res);
    }};
}

/// Implements arithmetic operations for floats
#[macro_export]
macro_rules! float_bin_op {
    ($self: ident, $f: ident) => {{
        let b = $self.stack.pop_float();
        let a = $self.stack.pop_float();

        let res = a.$f(b);

        $self.stack.push_float(res);
    }};
}
