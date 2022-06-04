const STACK_MAX: usize = 256;

pub(crate) type Value = f64;

pub(crate) struct ValueStack {
    pub(crate) values: [Value; STACK_MAX],
    pub(crate) current: usize,
}

impl ValueStack {
    /// Adds a value to the top of the stack
    ///
    /// TODO: elide bounds checking with unsafe?
    pub(crate) fn push(&mut self, value: Value) {
        self.values[self.current] = value;
        self.current += 1;
    }

    /// Removes the top value of the stack and returns it
    ///
    /// TODO: elide bounds checking with unsafe?
    pub(crate) fn pop(&mut self) -> Value {
        self.current -= 1;
        self.values[self.current]
    }

    pub(crate) fn peek(&self) -> Value {
        self.values[self.current]
    }

    pub(crate) fn reset(&mut self) {
        self.current = 0;
    }
}

impl Default for ValueStack {
    fn default() -> Self {
        ValueStack {
            values: [0.0; STACK_MAX],
            current: 0,
        }
    }
}
