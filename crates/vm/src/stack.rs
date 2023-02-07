//! The `Stack` is the storage location of values of
//! a known size during the course of the program.
//!
//! This `stack` module provides an abstraction over a
//! `Vec` to limit it to stack-like operations.
//!
//! The "slots" of the stack are a base size of 4 bytes (32 bits).
//! However, it is common for values to reside in more than
//! one slot. For example, a 64-bit float uses 2 slots.

use codegen::{Bool, Float, Int, StringLiteral};

/// Maximum size of the stack in Slots
const STACK_MAX: usize = 256;

const WORD_SIZE: usize = std::mem::size_of::<Word>();
/// Raw bytes representation of one "word" of the VM
type Word = [u8; 4];
type Word2 = [u8; 8];
type Word4 = [u8; 16]; // ex. StringLiteral
type Word6 = [u8; 24]; // ex. String, List

#[derive(Debug)]
pub struct Stack(Vec<Word>);

impl Stack {
    /// Adds a bytes value to the top of the stack
    #[inline]
    pub(crate) fn push(&mut self, val: Word) {
        self.0.push(val);
    }

    #[inline]
    pub(crate) fn push_two(&mut self, val: Word2) {
        let [first, second] = bytemuck::cast::<Word2, [Word; 2]>(val);

        self.0.push(first);
        self.0.push(second);
    }

    #[inline]
    pub(crate) fn push_int<I: Into<Int>>(&mut self, val: I) {
        self.push(val.into().to_ne_bytes())
    }

    #[inline]
    pub(crate) fn push_float<F: Into<Float>>(&mut self, val: F) {
        self.push_two(val.into().to_ne_bytes())
    }

    #[inline]
    pub(crate) fn push_bool(&mut self, val: bool) {
        self.push(Bool::from(val).to_ne_bytes())
    }

    /// Pushes the stack representation of a String to the stack.
    ///
    /// This is a (u64, u64) representing the (idx, len) of the
    /// bytes for this string constant in the string constants Vec.
    #[inline]
    pub(crate) fn push_str_constant(&mut self, idx: u64, len: u64) {
        self.push_two(idx.to_ne_bytes());
        self.push_two(len.to_ne_bytes());
    }

    /// Removes the top Word of the stack and returns it as bytes
    #[inline]
    pub(crate) fn pop(&mut self) -> Word {
        self.0.pop().unwrap()
    }

    /// Removes the top 2 Words of the stack and returns both in a tuple
    /// in LIFO order.
    ///
    /// - push A
    /// - push B
    /// - pop_two: returns (A, B)
    #[inline]
    pub(crate) fn pop_two(&mut self) -> Word2 {
        let b = self.pop();
        let a = self.pop();

        bytemuck::cast::<[Word; 2], Word2>([a, b])
    }

    /// Removes the top `n` slots of the stack.
    pub(crate) fn pop_n(&mut self, n: u8) {
        for _ in 0..n {
            self.pop();
        }
    }

    /// Removes the top `n` slots of the stack and returns it as an array
    /// with length `N` and elements of `Word`.
    #[inline]
    pub(crate) fn pop_n_static<const N: usize>(&mut self, n: usize) -> [Word; N] {
        let start = self.0.len() - n;

        let mut output: [Word; N] = [[0; 4]; N];
        for (i, byte) in self.0.drain(start..).enumerate() {
            output[i] = byte;
        }

        output
    }

    /// Removes the top slot of the stack and returns it as an i32
    #[inline]
    pub fn pop_int(&mut self) -> i32 {
        i32::from_ne_bytes(self.pop())
    }

    /// Removes the top slot of the stack and returns it as a u32
    #[inline]
    pub fn pop_uint(&mut self) -> u32 {
        u32::from_ne_bytes(self.pop())
    }

    /// Removes the top two slots of the stack and returns it as an f64
    #[inline]
    pub fn pop_float(&mut self) -> f64 {
        f64::from_ne_bytes(self.pop_two())
    }

    /// Removes the top value of the stack and returns it as a bool
    #[inline]
    pub fn pop_bool(&mut self) -> bool {
        let int = self.pop_uint();

        int != 0
    }

    #[inline]
    pub(crate) fn pop_string_literal(&mut self) -> StringLiteral {
        let bytes = self.pop_n_static::<4>(4);
        let [idx, len] = bytemuck::cast::<[Word; 4], [Word2; 2]>(bytes);

        (u64::from_ne_bytes(idx), u64::from_ne_bytes(len))
    }

    /// Mutates the top value of the stack in-place.
    #[inline]
    pub(super) fn replace_top(&mut self, val: Word) {
        self.0[0] = val;
    }

    #[inline]
    pub(crate) fn replace_top_two(&mut self, val: Word2) {
        todo!()
    }

    // not really a stack! used to set locals
    pub(crate) fn set(&mut self, index: usize, val: Word) {
        self.0[index] = val;
    }
}

// Non-mutating functions
impl Stack {
    // TODO: unsafe version for non-test code?
    #[inline]
    pub(crate) fn peek(&self) -> Word {
        *self.0.last().unwrap()
    }

    #[inline]
    fn peek_two(&self) -> Word2 {
        todo!()
    }

    #[inline]
    pub fn peek_int(&self) -> Int {
        Int::from_ne_bytes(self.peek())
    }

    #[inline]
    pub fn peek_float(&self) -> Float {
        Float::from_ne_bytes(self.peek_two())
    }

    #[inline]
    // TODO: make multiple of this (peek_two_at) or use const generics (like pop_n_static)
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

        stack.push_int(1);
        stack.push_int(2);

        let b = stack.pop_int();
        let a = stack.pop_int();

        let c = a + b;
        stack.push_int(c);

        assert_eq!(3, stack.peek_int());
    }

    #[test]
    fn pop_n_times() {
        let mut stack = Stack::default();

        let word_1: Word = 1_i32.to_ne_bytes();
        let word_2: Word = 2_i32.to_ne_bytes();
        let word_3: Word = 3_i32.to_ne_bytes();
        let word_4: Word = 4_i32.to_ne_bytes();
        let word_5: Word = 5_i32.to_ne_bytes();

        stack.push(word_1);
        stack.push(word_2);
        stack.push(word_3);
        stack.push(word_4);
        stack.push(word_5);

        let popped = stack.pop_n_static::<2>(2);

        assert_eq!([word_4, word_5], popped);
    }
}
