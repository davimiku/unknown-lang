const STACK_MAX: usize = 256;
const LANES: usize = 8;

/// Values stored in the stack
/// 8 bytes (64-bit) that can be interpreted as float, int, bool, etc.
/// based on the bytecode operation. All values (currently) are 8-byte.
type Value = [u8; LANES];

pub struct ValueStack {
    /// Values in the stack are 64-bit
    /// This can be i64, f64, bool, or a pointer which
    /// must be cast/intepreted when popped.
    pub(crate) values: [Value; STACK_MAX],

    /// Index of the current top of the stack
    pub current: usize,
}

impl ValueStack {
    /// Adds a value to the top of the stack
    ///
    /// TODO: elide bounds checking with unsafe?
    pub fn push<T: Into<Value>>(&mut self, value: T) {
        self.values[self.current] = value.into();
        self.current += 1;
    }

    /// Removes the top value of the stack and returns it
    ///
    /// TODO: elide bounds checking with unsafe?
    pub fn pop<T: From<Value>>(&mut self) -> T {
        self.current -= 1;
        self.values[self.current].into()
    }

    pub fn peek(&self) -> Value {
        self.values[self.current]
    }

    pub fn peek_at(&self, index: usize) -> &Value {
        &self.values[index]
    }

    pub fn reset(&mut self) {
        self.current = 0;
    }
}

impl Default for ValueStack {
    fn default() -> Self {
        ValueStack {
            values: [[0; LANES]; STACK_MAX],
            current: 0,
        }
    }
}
