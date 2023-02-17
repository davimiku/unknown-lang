//! The `Stack` is the storage location of values of
//! a known size during the course of the program.
//!
//! This `stack` module provides an abstraction over a
//! `Vec` to limit it to stack-like operations.
//!
//! The "slots" of the stack are a base size of 4 bytes (32 bits).
//! However, it is common for values to reside in more than
//! one slot. For example, a 64-bit float uses 2 slots.

use codegen::{XBool, XFloat, XInt, XString};

use crate::{DWord, QWord, Word};

/// Maximum size of the stack in Slots
const STACK_MAX: usize = 256;

const WORD_SIZE: usize = std::mem::size_of::<Word>();

#[derive(Debug)]
pub struct Stack {
    data: Vec<Word>,
}

impl Stack {
    /// Pushes one Word to the top of the stack
    #[inline]
    pub(crate) fn push_word(&mut self, val: Word) {
        self.data.push(val);
    }

    /// Pushes N Words to the top of the stack in the order
    /// of the provided array.
    ///
    /// ```text
    /// push_n [A, B, C]
    /// - Push A
    /// - Push B
    /// - Push C
    /// ```
    #[inline]
    pub(crate) fn push_n_words<const N: usize>(&mut self, words: [Word; N]) {
        for word in words {
            self.data.push(word);
        }
    }

    #[inline]
    pub(crate) fn push_dword(&mut self, dword: DWord) {
        let words = bytemuck::cast(dword);
        self.push_n_words::<2>(words);
    }

    #[inline]
    pub(crate) fn push_qword(&mut self, qword: QWord) {
        let words = bytemuck::cast(qword);
        self.push_n_words::<4>(words);
    }

    #[inline]
    pub(crate) fn push_int<I: Into<XInt>>(&mut self, val: I) {
        self.push_word(val.into().to_le_bytes())
    }

    #[inline]
    pub(crate) fn push_float<F: Into<XFloat>>(&mut self, val: F) {
        let dword = bytemuck::cast(val.into().to_le_bytes());
        self.push_dword(dword)
    }

    #[inline]
    pub(crate) fn push_bool(&mut self, val: bool) {
        self.push_word(XBool::from(val).to_le_bytes())
    }

    /// Pushes the stack representation of a String to the stack.
    #[inline]
    pub(crate) fn push_string(&mut self, s: XString) {
        let words = s.to_bytes();
        let qword = bytemuck::cast(words);

        self.push_qword(qword);
    }

    /// Removes the top Word of the stack
    ///
    /// Panics if the stack is empty.
    #[inline]
    pub(crate) fn pop(&mut self) -> Word {
        // consider unsafe `get_unchecked` and `set_len` if it makes a difference
        // in this hot loop
        self.data.pop().unwrap()
    }

    /// Removes the top DWord (2 Words) of the stack.
    #[inline]
    pub(crate) fn pop_dword(&mut self) -> DWord {
        let words = self.pop_n_static::<2>(2);
        bytemuck::cast(words)
    }

    /// Removes the top QWord (4 Words) of the stack.
    #[inline]
    pub(crate) fn pop_qword(&mut self) -> QWord {
        let words = self.pop_n_static::<4>(4);
        bytemuck::cast(words)
    }

    /// Removes the top `n` slots of the stack.
    ///
    /// Does not return the values.
    // TODO: return the words as a slice?
    // may not be possible because who owns the slice now?
    pub(crate) fn pop_n(&mut self, n: u8) {
        for _ in 0..n {
            self.pop();
        }
    }

    /// Removes the top `n` slots of the stack and returns it as an array
    /// with length `N` and elements of `Word`.
    #[inline]
    pub(crate) fn pop_n_static<const N: usize>(&mut self, n: usize) -> [Word; N] {
        let start = self.data.len() - n;

        let mut output: [Word; N] = [[0; 4]; N];
        for (i, byte) in self.data.drain(start..).enumerate() {
            output[i] = byte;
        }

        output
    }

    /// Removes the top slot of the stack and returns it as an i32
    #[inline]
    pub fn pop_int(&mut self) -> i32 {
        i32::from_le_bytes(self.pop())
    }

    /// Removes the top slot of the stack and returns it as a u32
    #[inline]
    pub fn pop_uint(&mut self) -> u32 {
        u32::from_le_bytes(self.pop())
    }

    /// Removes the top two slots of the stack and returns it as an f64
    #[inline]
    pub fn pop_float(&mut self) -> f64 {
        f64::from_le_bytes(self.pop_dword())
    }

    /// Removes the top value of the stack and returns it as a bool
    #[inline]
    pub fn pop_bool(&mut self) -> bool {
        let int = self.pop_uint();

        int != 0
    }

    #[inline]
    pub(crate) fn pop_string(&mut self) -> XString {
        let bytes = self.pop_qword();
        XString::from_bytes(bytes)
    }

    /// Mutates the top value of the stack in-place.
    #[inline]
    pub(super) fn replace_top_word(&mut self, val: Word) {
        self.data[0] = val;
    }

    #[inline]
    pub(crate) fn replace_top_dword(&mut self, val: DWord) {
        let words: [Word; 2] = bytemuck::cast(val);
        self.data[1] = words[0];
        self.data[0] = words[1];
    }

    /// Sets the value at a given index of the stack.
    ///
    /// Note: This `index` is the index number into the underlying `Vec`
    /// data and it is opposite of the "depth" of the stack. i.e. index 0
    /// is the very bottom of the stack, the last element that could be popped.
    ///
    /// This is an escape hatch out of the proper notion of a "stack", but is
    /// necessary for setting the value of locals during variable definition.
    #[inline]
    pub(crate) fn set_word_at(&mut self, val: Word, index: usize) {
        self.data[index] = val;
    }
}

// Non-mutating functions
impl Stack {
    // TODO: unsafe version for non-test code?
    /// Copies the top word from the stack and returns it.
    ///
    /// Panics if there is not 1 item in the stack.
    #[inline]
    pub(crate) fn peek_word(&self) -> Word {
        self.data.last().copied().unwrap()
    }

    /// Peeks the DWord (2 Words) from the top of the stack
    #[inline]
    pub(crate) fn peek_dword(&self) -> &DWord {
        self.peek_dword_at(self.data.len() - 2)
    }

    /// Peeks the QWord (4 Words) from the top of the stack
    #[inline]
    pub(crate) fn peek_qword(&self) -> &QWord {
        self.peek_qword_at(self.data.len() - 4)
    }

    // #[inline]
    // pub(crate) fn peek_n(&self, n: usize) -> &[Word] {
    //     let peeked: Vec<&Word> = self.0.iter().rev().take(n).collect();
    // }

    #[inline]
    pub(crate) fn peek_int(&self) -> XInt {
        XInt::from_le_bytes(self.peek_word())
    }

    #[inline]
    pub(crate) fn peek_float(&self) -> XFloat {
        XFloat::from_le_bytes(*self.peek_dword())
    }

    #[inline]
    pub(crate) fn peek_string(&self) -> XString {
        XString::from_bytes(*self.peek_qword())
    }

    #[inline]
    pub(crate) fn peek_word_at(&self, index: usize) -> &Word {
        &self.data[index]
    }

    pub(crate) fn peek_n_words_at<const N: usize>(&self, index: usize, n: usize) -> &[Word] {
        todo!()
    }

    pub(crate) fn peek_dword_at(&self, index: usize) -> &DWord {
        let words = &[self.data[index], self.data[index + 1]];
        // TODO: make this part of the DWord constructor and verify its safe-ness
        unsafe { std::mem::transmute(words) }
    }

    /// Peeks and copies four Words from the given
    pub(crate) fn peek_qword_at(&self, index: usize) -> &QWord {
        let words = &[
            self.data[index],
            self.data[index + 1],
            self.data[index + 2],
            self.data[index + 3],
        ];
        // TODO: make this part of the QWord constructor and verify its safe-ness
        unsafe { std::mem::transmute(words) }
    }

    // #[inline]
    // pub(crate) fn clear(&mut self) {
    //     self.data.clear()
    // }
}

impl Default for Stack {
    fn default() -> Self {
        Self {
            data: Vec::with_capacity(STACK_MAX),
        }
    }
}

fn clone_into_array<A, T>(slice: &[T]) -> A
where
    A: Default + AsMut<[T]>,
    T: Clone,
{
    let mut a = A::default();
    <A as AsMut<[T]>>::as_mut(&mut a).clone_from_slice(slice);
    a
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

        let word_1: Word = 1_i32.to_le_bytes();
        let word_2: Word = 2_i32.to_le_bytes();
        let word_3: Word = 3_i32.to_le_bytes();
        let word_4: Word = 4_i32.to_le_bytes();
        let word_5: Word = 5_i32.to_le_bytes();

        stack.push_word(word_1);
        stack.push_word(word_2);
        stack.push_word(word_3);
        stack.push_word(word_4);
        stack.push_word(word_5);

        let popped = stack.pop_n_static::<2>(2);

        assert_eq!([word_4, word_5], popped);
    }
}
