/// Pops two values from the stack, performs the provided
/// arithmetic operation, and returns the result.
#[macro_export]
macro_rules! arithmetic {
    ($self: ident, $t: ty, $F: path) => {{
        let b = $self.stack.pop();
        let b = <$t>::from_le_bytes(b);

        let a = $self.stack.pop();
        let a = <$t>::from_le_bytes(a);

        let res = $F(a, b).to_le_bytes();

        $self.stack.push(res);
    }};
}
