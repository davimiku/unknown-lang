const STACK_MAX: usize = 256;
const LANES: usize = 8;

/// Values stored in the stack
/// 8 bytes (64-bit) that can be interpreted as float, int, bool, etc.
/// based on the bytecode operation. All values (currently) are 8-byte.
type Value = [u8; LANES];

/// Stack representation of a String is a (ptr, len)
///
/// ptr: pointer to the string data
/// len: byte length of the string data
type StrValue = (Value, Value);

#[derive(Debug)]
pub struct ValueStack(Vec<Value>);
/// The base "word" size of the stack is 64-bit (8 bytes)
///
/// Many values are 1 word, such as Int, Float, Bool.
/// Larger values or compound stack values should take
/// care to call the appropriate push and pop functions.

impl ValueStack {
    /// Adds a bytes value to the top of the stack
    pub fn push(&mut self, val: Value) {
        self.0.push(val);
    }

    pub fn push_int<I: Into<i64>>(&mut self, val: I) {
        self.push(val.into().to_le_bytes())
    }

    pub fn push_float<F: Into<f64>>(&mut self, val: F) {
        self.push(val.into().to_le_bytes())
    }

    pub fn push_str(&mut self, val: &str) {
        let ptr = val.as_ptr();
        let len = val.len();
        // self.push(val);
    }

    /// Removes the top value of the stack and returns it as bytes
    pub fn pop(&mut self) -> Value {
        self.0.pop().unwrap()
    }

    /// Removes the top value of the stack and returns it as an i64
    pub fn pop_int(&mut self) -> i64 {
        i64::from_le_bytes(self.pop())
    }

    /// Removes the top value of the stack and returns it as an i64
    pub fn pop_float(&mut self) -> f64 {
        f64::from_le_bytes(self.pop())
    }

    pub fn peek(&self) -> Value {
        *self.0.last().unwrap()
    }

    pub fn peek_int(&self) -> i64 {
        i64::from_le_bytes(self.peek())
    }

    pub fn peek_float(&self) -> f64 {
        f64::from_le_bytes(self.peek())
    }

    pub fn peek_at(&self, index: usize) -> &Value {
        &self.0[index]
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }
}

impl Default for ValueStack {
    fn default() -> Self {
        Self(Vec::with_capacity(STACK_MAX))
    }
}

impl ValueStack {
    pub fn from_values(values: &[Value]) -> Self {
        Self(values.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_value() {
        let mut stack = ValueStack::default();

        stack.push_int(1);

        assert_eq!(1, stack.peek_int());
    }

    #[test]
    fn push_twice() {
        let mut stack = ValueStack::default();

        stack.push_int(1);
        stack.push_int(2);

        assert_eq!(2, stack.peek_int());
    }

    #[test]
    fn addition() {
        let mut stack = ValueStack::default();

        stack.push_int(1_i64);
        stack.push_int(2_i64);

        let b = stack.pop_int();
        let a: i64 = stack.pop_int();

        let c = a + b;
        stack.push_int(c);

        assert_eq!(3, stack.peek_int());
    }
}
