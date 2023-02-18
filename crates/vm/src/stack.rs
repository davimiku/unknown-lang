//! The `Stack` is the storage location of values of
//! a known size during the course of the program.
//!
//! This `stack` module provides an abstraction over a
//! `Vec` to limit it to stack-like operations.
//!
//! The "slots" of the stack are a base size of 1 Word, which
//! is an opaque type.
//!
//! It is possible and common for values to reside in more than
//! one slot. For example, a Float uses 2 slots.

use vm_types::words::{DWord, QWord, Word};
use vm_types::xstring::VMString;
use vm_types::{VMBool, VMFloat, VMInt};

/// Maximum size of the stack in Slots
const STACK_MAX: usize = 256;

#[derive(Debug)]
pub struct Stack {
    data: Vec<Word>,
}

impl Stack {
    #[inline]
    fn extend<I: IntoIterator<Item = Word>>(&mut self, iter: I) {
        self.data.extend(iter)
    }

    /// Pushes one Word to the top of the stack
    #[inline]
    pub(crate) fn push_word<T: Into<Word>>(&mut self, val: T) {
        self.extend([val.into()])
    }

    #[inline]
    pub(crate) fn push_dword<T: Into<DWord>>(&mut self, value: T) {
        let dword: DWord = value.into();
        self.extend(dword.into_iter());
    }

    #[inline]
    pub(crate) fn push_qword<T: Into<QWord>>(&mut self, value: T) {
        let qword: QWord = value.into();
        self.extend(qword.into_iter());
    }

    #[inline]
    pub(crate) fn push_bool<B: Into<VMBool>>(&mut self, val: B) {
        self.push_word(val.into());
    }

    #[inline]
    pub(crate) fn push_int<I: Into<VMInt>>(&mut self, val: I) {
        self.push_word(val.into());
    }

    #[inline]
    pub(crate) fn push_float<F: Into<VMFloat>>(&mut self, val: F) {
        self.push_dword(val.into());
    }

    /// Pushes the stack representation of a String to the stack.
    #[inline]
    pub(crate) fn push_string(&mut self, s: VMString) {
        let q: QWord = s.into();
        self.push_qword(q);
    }

    /// Removes the top `n` slots of the stack and returns it as an array
    /// with length `N` and elements of `Word`.
    ///
    /// Slots are returned in FIFO order, i.e. reversed from the "popping" order.
    /// This ensures that multi-Word values are returned as expected.
    #[inline]
    pub(crate) fn pop_n<const N: usize>(&mut self, n: usize) -> [Word; N] {
        let start = self.data.len() - n;

        let mut output: [Word; N] = [Default::default(); N];
        for (i, word) in self.data.drain(start..).enumerate() {
            output[i] = word;
        }

        output
    }

    /// Removes the top Word of the stack
    ///
    /// Panics if the stack is empty.
    #[inline]
    pub(crate) fn pop_word(&mut self) -> Word {
        // consider unsafe `get_unchecked` and `set_len` if it makes a difference
        // in this hot loop
        self.data.pop().unwrap()
    }

    /// Removes the top DWord (2 Words) of the stack.
    #[inline]
    pub(crate) fn pop_dword(&mut self) -> DWord {
        self.pop_n::<2>(2).into()
    }

    /// Removes the top QWord (4 Words) of the stack.
    #[inline]
    pub(crate) fn pop_qword(&mut self) -> QWord {
        self.pop_n::<4>(4).into()
    }

    /// Removes the top `n` slots of the stack.
    ///
    /// Does not return the values.
    // TODO: return the words as a slice?
    // may not be possible because who owns the slice now?
    pub(crate) fn pop_n_dynamic(&mut self, n: u8) {
        for _ in 0..n {
            self.pop_word();
        }
    }

    /// Removes the top value of the stack and returns it as a bool
    #[inline]
    pub fn pop_bool(&mut self) -> VMBool {
        self.pop_word().into()
    }

    /// Removes the top slot of the stack and returns it as an XInt
    #[inline]
    pub fn pop_int(&mut self) -> VMInt {
        self.pop_word().into()
    }

    /// Removes the top two slots of the stack and returns it as an f64
    #[inline]
    pub fn pop_float(&mut self) -> VMFloat {
        self.pop_dword().into()
    }

    #[inline]
    pub(crate) fn pop_string(&mut self) -> VMString {
        self.pop_qword().into()
    }

    /// Mutates the top value of the stack in-place.
    #[inline]
    pub(super) fn replace_top_word<T: Into<Word>>(&mut self, val: T) {
        let top = self.top_index();
        self.data[top] = val.into();
    }

    #[inline]
    pub(crate) fn replace_top_dword<T: Into<DWord>>(&mut self, val: T) {
        let top = self.top_index();
        let dword: DWord = val.into();
        let words: [Word; 2] = dword.into();
        self.data[top - 1] = words[0];
        self.data[top] = words[1];
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
    #[inline]
    fn top_index(&self) -> usize {
        self.data.len() - 1
    }

    // TODO: unsafe version for non-test code?
    /// Copies the top word from the stack and returns it.
    ///
    /// Panics if there is not 1 item in the stack.
    #[inline]
    pub(crate) fn peek_word(&self) -> &Word {
        self.peek_word_at(self.data.len() - 1)
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
    pub(crate) fn peek_bool(&self) -> VMBool {
        (*self.peek_word()).into()
    }

    #[inline]
    pub(crate) fn peek_int(&self) -> VMInt {
        (*self.peek_word()).into()
    }

    #[inline]
    pub(crate) fn peek_float(&self) -> VMFloat {
        (*self.peek_dword()).into()
    }

    #[inline]
    pub(crate) fn peek_string(&self) -> VMString {
        (*self.peek_qword()).into()
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

        assert_eq!(0, stack.pop_bool());
        assert_eq!(1, stack.pop_bool());
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

        stack.push_word(1_i32);
        stack.push_word(2_i32);
        stack.push_word(3_i32);
        stack.push_word(4_i32);
        stack.push_word(5_i32);

        let popped = stack.pop_n::<2>(2);

        assert_eq!(popped, [4_i32.into(), 5_i32.into()]);
    }
}
