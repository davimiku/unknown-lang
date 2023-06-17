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
//! one slot. For example, a String uses 2 slots.

use std::fmt::{self, Debug};
use std::rc::Rc;

use memory_recycling::{Gc, Trace};
use vm_codegen::FunctionChunk;
use vm_string::VMString;
use vm_types::words::{DWord, DWordBytes, QWord, Word, ZERO_WORD};
use vm_types::{VMBool, VMFloat, VMInt};

/// Maximum size of the stack in Slots
const STACK_MAX: usize = 256;

pub(crate) struct Stack {
    /// Stack data in units of Word
    data: Vec<Word>,

    /// Offset for operations that reach into the internals of the stack
    ///
    /// call frames have a bottom of the stack which is not
    /// the true bottom of the stack.
    pub(crate) offset: usize,
}

impl Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Stack")
            .field("offset", &self.offset)
            .field("data from offset", &self.slice_from_current())
            .finish()
    }
}

impl Stack {
    #[inline]
    fn slice_from_current(&self) -> &[Word] {
        &self.data[self.offset..]
    }

    #[inline]
    fn extend<I: IntoIterator<Item = Word>>(&mut self, iter: I) {
        self.data.extend(iter)
    }

    /// Pushes one Word to the top of the stack
    #[inline]
    pub(crate) fn push_word<T: Into<Word>>(&mut self, val: T) {
        self.data.push(val.into())
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
        self.push_word(val.into());
    }

    /// Pushes the stack representation of a String to the stack.
    #[inline]
    pub(crate) fn push_string(&mut self, s: VMString) {
        let q: DWord = s.into();
        self.push_dword(q);
    }

    #[inline]
    pub(crate) fn push_raw_ptr<T>(&mut self, ptr: *const T) {
        self.push_word(ptr as u64);
    }

    #[inline]
    pub(crate) fn push_rc<T>(&mut self, rc: Rc<T>) {
        let raw_ptr = Rc::into_raw(rc);
        self.push_raw_ptr(raw_ptr);
    }

    #[inline]
    pub(crate) fn push_gc<T: Trace>(&mut self, gc: Gc<T>) {
        let raw_ptr = Gc::into_raw(gc);
        self.push_raw_ptr(raw_ptr);
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

    /// Pops the top `n` slots of the stack, and gives ownership
    /// of these to the caller.
    pub(crate) fn pop_n_as_vec(&mut self, n: usize) -> Vec<Word> {
        (0..n).map(|_| self.pop_word()).collect()
    }

    /// Removes the top `n` slots of the stack without returning the values.
    pub(crate) fn pop_n_discard(&mut self, n: usize) {
        let new_len = self.data.len() - n;
        self.data.truncate(new_len);
    }

    /// Removes the top value of the stack and returns it as a bool
    #[inline]
    pub fn pop_bool(&mut self) -> VMBool {
        self.pop_word().into()
    }

    /// Removes the top slot of the stack and returns it as an VMInt
    #[inline]
    pub fn pop_int(&mut self) -> VMInt {
        self.pop_word().into()
    }

    /// Removes the top slot of the stack and returns it as an VMFloat
    #[inline]
    pub fn pop_float(&mut self) -> VMFloat {
        self.pop_word().into()
    }

    /// Removes the top two slots of the stack and returns it as an VMString
    #[inline]
    pub(crate) fn pop_string(&mut self) -> VMString {
        let bytes: DWordBytes = self.pop_dword().into();
        VMString::from_raw(bytes)
    }

    #[inline]
    pub(crate) fn pop_raw_ptr<T>(&mut self) -> *const T {
        let word = self.pop_word();
        u64::from(word) as *const T
    }

    #[inline]
    pub(crate) fn pop_func_ptr(&mut self) -> *const FunctionChunk {
        self.pop_raw_ptr()
    }

    #[inline]
    pub(crate) fn pop_rc<T>(&mut self) -> Rc<T> {
        let raw_ptr = self.pop_raw_ptr();

        // Safety: this pointer must have been pushed with Rc::into_raw
        unsafe { Rc::from_raw(raw_ptr) }
    }

    #[inline]
    pub(crate) fn pop_gc<T: Trace>(&mut self) -> Gc<T> {
        let raw_ptr = self.pop_raw_ptr();

        // Safety: this pointer must have been pushed with Gc::into_raw
        unsafe { Gc::from_raw(raw_ptr) }
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
    /// The offset represents the start of the stack in the context of the
    /// current call frame.
    ///
    /// This is an escape hatch out of the proper notion of a "stack", but is
    /// necessary for setting the value of locals during variable definition.
    #[inline]
    pub(crate) fn set_word_at(&mut self, val: Word, index: usize) {
        self.data[index + self.offset] = val;
    }

    pub(crate) fn set_dword_at(&mut self, val: DWord, index: usize) {
        // TODO: std::ptr::write_bytes ?
        let words: [Word; 2] = val.into();
        for (i, word) in words.iter().enumerate() {
            self.set_word_at(*word, index + i);
        }
    }
    // pub(crate) fn set_qword_at(&mut self, val: DWord, index: usize) { }

    /// Shifts the stack to the right by the given amount starting
    /// at the given index. The shifted slots are replaced by zeros.
    // TODO: better name needed
    #[inline]
    pub(crate) fn shift_at_end(&mut self, num_slots: usize) {
        let index = self.len() - num_slots;
        self.data.extend_from_within(index..);
        // TODO: can this for loop be optimized with std::ptr::write_bytes?
        for i in 0..num_slots {
            self.data[i + index + self.offset + 1] = ZERO_WORD;
        }
    }
}

// Non-mutating functions
impl Stack {
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

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
        let index = self.data.len() - 1;
        &self.data[index]
    }

    /// Peeks the DWord (2 Words) from the top of the stack
    #[inline]
    pub(crate) fn peek_dword(&self) -> &DWord {
        let index = self.data.len() - 2;
        let words = &[self.data[index], self.data[index + 1]];
        // TODO: make this part of the DWord constructor and verify its safe-ness
        unsafe { std::mem::transmute(words) }
    }

    /// Peeks the QWord (4 Words) from the top of the stack
    #[inline]
    pub(crate) fn peek_qword(&self) -> &QWord {
        todo!("fix: this shouldn't call peek_qword_at anymore because that adds an offset");

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
        (*self.peek_word()).into()
    }

    // #[inline]
    // pub(crate) fn peek_string(&self) -> VMString {
    //     (*self.peek_dword()).into()
    // }

    #[inline]
    pub(crate) fn peek_word_at(&self, index: usize) -> &Word {
        let index = index + self.offset;
        &self.data[index]
    }

    pub(crate) fn peek_n_words_at<const N: usize>(&self, index: usize, n: usize) -> &[Word] {
        todo!()
    }

    pub(crate) fn peek_dword_at(&self, index: usize) -> &DWord {
        let index = index + self.offset;
        let words = &[self.data[index], self.data[index + 1]];
        // TODO: make this part of the DWord constructor and verify its safe-ness
        unsafe { std::mem::transmute(words) }
    }

    /// Peeks and copies four Words from the given
    pub(crate) fn peek_qword_at(&self, index: usize) -> &QWord {
        let index = index + self.offset;
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
            offset: 0,
        }
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

        stack.push_word(1_i64);
        stack.push_word(2_i64);
        stack.push_word(3_i64);
        stack.push_word(4_i64);
        stack.push_word(5_i64);

        let popped = stack.pop_n::<2>(2);

        assert_eq!(popped, [4_i64.into(), 5_i64.into()]);
    }
}
