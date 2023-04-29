// TODO: these 3 are identical, generate with a macro

use bytemuck::cast;
use std::array;

use crate::{VMBool, VMFloat, VMInt};

pub fn word_size_of<T>() -> usize {
    std::mem::size_of::<T>() / WORD_SIZE
}

/// Represents 1 "word", or the base size of values in the VM.
/// No values are smaller than 1 word.
///
/// For example, a `Bool` is 1 Word.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Word {
    bytes: WordBytes,
}
pub const WORD_SIZE: usize = 8;
type WordBytes = [u8; WORD_SIZE];

// Convert From

impl From<WordBytes> for Word {
    fn from(bytes: WordBytes) -> Self {
        Self { bytes }
    }
}

impl From<VMBool> for Word {
    fn from(value: VMBool) -> Self {
        Self {
            bytes: value.to_le_bytes(),
        }
    }
}

impl From<VMInt> for Word {
    fn from(value: VMInt) -> Self {
        Self {
            bytes: value.to_le_bytes(),
        }
    }
}

impl From<VMFloat> for Word {
    fn from(value: VMFloat) -> Self {
        Self {
            bytes: value.to_le_bytes(),
        }
    }
}

// Convert To

impl From<Word> for WordBytes {
    fn from(word: Word) -> Self {
        word.bytes
    }
}

impl From<Word> for VMBool {
    fn from(word: Word) -> Self {
        VMBool::from_le_bytes(word.bytes)
    }
}

impl From<Word> for Vec<u8> {
    fn from(word: Word) -> Self {
        word.bytes.into()
    }
}

impl From<Word> for VMInt {
    fn from(word: Word) -> Self {
        VMInt::from_le_bytes(word.bytes)
    }
}

impl From<Word> for VMFloat {
    fn from(word: Word) -> Self {
        VMFloat::from_le_bytes(word.bytes)
    }
}

// Iterators

impl IntoIterator for Word {
    type Item = u8;
    type IntoIter = array::IntoIter<Self::Item, WORD_SIZE>;

    fn into_iter(self) -> Self::IntoIter {
        self.bytes.into_iter()
    }
}

impl FromIterator<u8> for Word {
    fn from_iter<I: IntoIterator<Item = u8>>(iter: I) -> Self {
        iter.into_iter().collect()
    }
}

/// Represents a "double word", or the size of 2 base values.
///
/// For example, a `Float` (called a "double" in some other languages)
/// is represented by a DWord.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DWord {
    bytes: DWordBytes,
}
pub const DWORD_SIZE: usize = WORD_SIZE * 2;
type DWordBytes = [u8; DWORD_SIZE];

// Convert From

impl From<DWordBytes> for DWord {
    fn from(bytes: DWordBytes) -> Self {
        Self { bytes }
    }
}

impl From<[Word; 2]> for DWord {
    fn from(words: [Word; 2]) -> Self {
        let bytes: DWordBytes = cast([words[0].bytes, words[1].bytes]);
        bytes.into()
    }
}

// Convert To

impl From<DWord> for DWordBytes {
    fn from(value: DWord) -> Self {
        value.bytes
    }
}

impl From<DWord> for Vec<u8> {
    fn from(value: DWord) -> Self {
        value.bytes.into()
    }
}

impl From<DWord> for [Word; 2] {
    fn from(value: DWord) -> Self {
        let bytes: [WordBytes; 2] = cast(value.bytes);
        [bytes[0].into(), bytes[1].into()]
    }
}

// Iterators

impl IntoIterator for DWord {
    type Item = Word;
    type IntoIter = array::IntoIter<Self::Item, 2>;

    fn into_iter(self) -> Self::IntoIter {
        let bytes: [WordBytes; 2] = cast(self.bytes);
        let words: [Word; 2] = [bytes[0].into(), bytes[1].into()];
        words.into_iter()
    }
}

impl FromIterator<u8> for DWord {
    fn from_iter<I: IntoIterator<Item = u8>>(iter: I) -> Self {
        iter.into_iter().collect()
    }
}

/// Represents a "quad word", or the size of 4 base values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QWord {
    bytes: QWordBytes,
}
pub const QWORD_SIZE: usize = WORD_SIZE * 4;
type QWordBytes = [u8; QWORD_SIZE];

// Convert From

impl From<QWordBytes> for QWord {
    fn from(bytes: QWordBytes) -> Self {
        Self { bytes }
    }
}

impl From<[Word; 4]> for QWord {
    fn from(words: [Word; 4]) -> Self {
        let bytes: QWordBytes = cast([
            words[0].bytes,
            words[1].bytes,
            words[2].bytes,
            words[3].bytes,
        ]);
        bytes.into()
    }
}

// Convert To

impl From<QWord> for QWordBytes {
    fn from(value: QWord) -> Self {
        value.bytes
    }
}

impl From<QWord> for Vec<u8> {
    fn from(value: QWord) -> Self {
        value.bytes.into()
    }
}

impl From<QWord> for [Word; 4] {
    fn from(value: QWord) -> Self {
        let bytes: [WordBytes; 4] = cast(value.bytes);
        [
            bytes[0].into(),
            bytes[1].into(),
            bytes[2].into(),
            bytes[3].into(),
        ]
    }
}

// Iterators

impl IntoIterator for QWord {
    type Item = Word;
    type IntoIter = array::IntoIter<Self::Item, 4>;

    fn into_iter(self) -> Self::IntoIter {
        let bytes: [WordBytes; 4] = cast(self.bytes);
        let words: [Word; 4] = [
            bytes[0].into(),
            bytes[1].into(),
            bytes[2].into(),
            bytes[3].into(),
        ];
        words.into_iter()
    }
}

impl FromIterator<u8> for QWord {
    fn from_iter<I: IntoIterator<Item = u8>>(iter: I) -> Self {
        iter.into_iter().collect()
    }
}
