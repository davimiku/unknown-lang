// TODO: move Stack to the vm crate?

const STACK_MAX: usize = 256;
const WORD_SIZE: usize = 8;

/// Values stored in the stack
/// 8 bytes (64-bit) that can be interpreted as float, int, bool, etc.
/// based on the bytecode operation. All values (currently) are 8-byte.
type Word = [u8; WORD_SIZE];

/// Stack representation of a String constant is a (idx, len)
///
/// idx: index to first byte in string constants array
/// len: byte length of the string data
type StringConstant = (u64, u64);

#[derive(Debug)]
pub struct Stack(Vec<Word>);
/// The base "word" size of the stack is 64-bit (8 bytes)
///
/// Many values are 1 word, such as Int, Float, Bool.
/// Larger values or compound stack values should take
/// care to call the appropriate push and pop functions.

impl Stack {
    #[inline]
    pub fn size(&self) -> usize {
        self.0.len()
    }

    /// Adds a bytes value to the top of the stack
    #[inline]
    pub fn push(&mut self, val: Word) {
        self.0.push(val);
    }

    #[inline]
    pub fn push_int<I: Into<i64>>(&mut self, val: I) {
        self.push(val.into().to_le_bytes())
    }

    #[inline]
    pub fn push_float<F: Into<f64>>(&mut self, val: F) {
        self.push(val.into().to_le_bytes())
    }

    #[inline]
    pub fn push_bool(&mut self, val: bool) {
        self.push(i64::from(val).to_le_bytes())
    }

    /// Pushes the stack representation of a String to the stack.
    ///
    /// This is a (u64, u64) representing the (idx, len) of the
    /// bytes for this string constant in the string constants Vec.
    #[inline]
    pub fn push_str_constant(&mut self, idx: u64, len: u64) {
        self.push(idx.to_le_bytes());
        self.push(len.to_le_bytes());
    }

    /// Removes the top Word of the stack and returns it as bytes
    #[inline]
    pub fn pop(&mut self) -> Word {
        self.0.pop().unwrap()
    }

    /// Removes the top 2 Words of the stack and returns both in a tuple
    /// in LIFO order.
    ///
    /// - push A
    /// - push B
    /// - pop_two: returns (A, B)
    #[inline]
    fn pop_two(&mut self) -> (Word, Word) {
        let b = self.pop();
        let a = self.pop();

        (a, b)
    }

    /// Removes the top N words of the stack and returns it as a byte array.
    #[inline]
    fn pop_n<const N: usize>(&mut self, n: usize) -> [Word; N] {
        let start = self.0.len() - n;

        let mut output: [Word; N] = [[0; 8]; N];
        for (i, byte) in self.0.drain(start..).enumerate() {
            output[i] = byte;
        }

        output
    }

    /// Removes the top value of the stack and returns it as an i64
    #[inline]
    pub fn pop_int(&mut self) -> i64 {
        i64::from_le_bytes(self.pop())
    }

    /// Removes the top value of the stack and returns it as an i64
    #[inline]
    pub fn pop_float(&mut self) -> f64 {
        f64::from_le_bytes(self.pop())
    }

    /// Removes the top value of the stack and returns it as a bool
    #[inline]
    pub fn pop_bool(&mut self) -> bool {
        let int = self.pop_int();

        int != 0
    }

    #[inline]
    pub fn pop_string_literal(&mut self) -> StringConstant {
        let [idx, len] = self.pop_n::<2>(2);

        (u64::from_le_bytes(idx), u64::from_le_bytes(len))
    }

    #[inline]
    fn peek(&self) -> Word {
        *self.0.last().unwrap()
    }

    #[inline]
    pub fn peek_int(&self) -> i64 {
        i64::from_le_bytes(self.peek())
    }

    #[inline]
    pub fn peek_float(&self) -> f64 {
        f64::from_le_bytes(self.peek())
    }

    #[inline]
    pub fn peek_at(&self, index: usize) -> &Word {
        &self.0[index]
    }

    #[inline]
    pub fn clear(&mut self) {
        self.0.clear()
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self(Vec::with_capacity(STACK_MAX))
    }
}

impl Stack {
    pub fn from_values(values: &[Word]) -> Self {
        Self(values.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_value() {
        let mut stack = Stack::default();

        stack.push_int(1);

        assert_eq!(1, stack.peek_int());
    }

    #[test]
    fn push_twice() {
        let mut stack = Stack::default();

        stack.push_int(1);
        stack.push_int(2);

        assert_eq!(2, stack.peek_int());
    }

    #[allow(clippy::bool_assert_comparison)] // comparing literal booleans
    #[test]
    fn push_pop_bool() {
        let mut stack = Stack::default();

        stack.push_bool(true);
        stack.push_bool(false);

        assert_eq!(false, stack.pop_bool());
        assert_eq!(true, stack.pop_bool());
    }

    #[test]
    fn addition() {
        let mut stack = Stack::default();

        stack.push_int(1_i64);
        stack.push_int(2_i64);

        let b = stack.pop_int();
        let a: i64 = stack.pop_int();

        let c = a + b;
        stack.push_int(c);

        assert_eq!(3, stack.peek_int());
    }

    #[test]
    fn pop_n_times() {
        let mut stack = Stack::default();

        let word_1: Word = 1_u64.to_le_bytes();
        let word_2: Word = 2_u64.to_le_bytes();
        let word_3: Word = 3_u64.to_le_bytes();
        let word_4: Word = 4_u64.to_le_bytes();
        let word_5: Word = 5_u64.to_le_bytes();

        stack.push(word_1);
        stack.push(word_2);
        stack.push(word_3);
        stack.push(word_4);
        stack.push(word_5);

        let popped = stack.pop_n::<2>(2);

        assert_eq!([word_4, word_5], popped);
    }
}
