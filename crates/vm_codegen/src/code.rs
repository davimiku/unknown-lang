use std::fmt;

use text_size::TextRange;

use crate::Op;

#[derive(Default, PartialEq, Eq)]
pub(super) struct Code {
    pub(super) bytes: Vec<u8>,
    pub(super) ranges: Vec<TextRange>,
}

impl std::fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Code")
            .field("bytes", &debug_bytes(&self.bytes))
            .field("ranges", &self.ranges)
            .finish()
    }
}

fn debug_bytes(bytes: &[u8]) -> impl fmt::Debug {
    let bytes: Vec<_> = bytes
        .chunks_exact(4)
        .map(|s| {
            let q: [u8; 4] = s.try_into().expect("slice with length 4");
            u32::from_le_bytes(q)
        })
        .collect();

    bytes
}

impl Code {
    pub(super) fn from_op(op: Op, range: TextRange) -> Self {
        Self {
            bytes: vec![op as u8],
            ranges: vec![range],
        }
    }
}

impl Code {
    #[inline]
    pub(super) fn append(&mut self, mut source: Code) {
        self.bytes.append(&mut source.bytes);
        self.ranges.append(&mut source.ranges);
    }

    #[inline]
    pub(super) fn push<B: Into<u8>>(&mut self, source: (B, TextRange)) {
        self.bytes.push(source.0.into());
        self.ranges.push(source.1);
    }

    #[inline]
    pub(super) fn push_u8(&mut self, source: u8) {
        self.bytes.push(source);
    }

    #[inline]
    pub(super) fn push_u16(&mut self, source: u16) {
        let source: [u8; 2] = source.to_le_bytes();

        self.bytes.push(source[0]);
        self.bytes.push(source[1]);
    }

    #[inline]
    pub(super) fn push_u32(&mut self, source: u32) {
        let source: [u8; 4] = source.to_le_bytes();

        // TODO: write_bytes
        self.bytes.push(source[0]);
        self.bytes.push(source[1]);
        self.bytes.push(source[2]);
        self.bytes.push(source[3]);
    }

    #[inline]
    pub(super) fn extend_from_slice(&mut self, source: &[u8]) {
        self.bytes.extend_from_slice(source);
    }

    #[inline]
    pub(super) fn shrink_to_fit(&mut self) {
        self.bytes.shrink_to_fit();
        self.ranges.shrink_to_fit();
    }

    #[inline]
    fn extend<I: IntoIterator<Item = u8>>(&mut self, source: I) {
        self.bytes.extend(source.into_iter());
    }
}

impl From<(u8, TextRange)> for Code {
    fn from(source: (u8, TextRange)) -> Self {
        Self {
            bytes: vec![source.0],
            ranges: vec![source.1],
        }
    }
}

impl From<(Op, TextRange)> for Code {
    fn from(source: (Op, TextRange)) -> Self {
        Self {
            bytes: vec![source.0 as u8],
            ranges: vec![source.1],
        }
    }
}
