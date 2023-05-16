use std::ops::Add;

use crate::{
    as_str::AsStr,
    embedded::{EmbeddedBytes, EmbeddedVMString, MAX_EMBEDDED_LENGTH},
    heap::{new_heap_string, HeapVMString},
    VMString,
};

impl Add for VMString {
    type Output = VMString;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (VMString::Embedded(a), VMString::Embedded(b)) => a + b,
            (VMString::Embedded(a), VMString::Heap(b)) => concat(a, b),
            (VMString::Heap(a), VMString::Embedded(b)) => concat(a, b),
            (VMString::Heap(a), VMString::Heap(b)) => a + b,
        }
    }
}

impl Add for EmbeddedVMString {
    type Output = VMString;

    fn add(self, rhs: Self) -> Self::Output {
        let lhs_len = self.len as usize;
        let rhs_len = rhs.len as usize;
        let new_len = lhs_len + rhs_len;

        if lhs_len == 0 {
            return rhs.into();
        }
        if rhs_len == 0 {
            return self.into();
        }

        if new_len <= MAX_EMBEDDED_LENGTH {
            let mut new_data: EmbeddedBytes = [0; MAX_EMBEDDED_LENGTH];
            let lhs_src = self.as_bytes().as_ptr();
            let rhs_src = rhs.as_bytes().as_ptr();
            let dst = new_data.as_mut_ptr();

            // Safety: `src` and `dst` are aligned and non-overlapping
            // `src` is valid for reads of `len` bytes and `dst` is valid for
            // writes of `len` bytes.
            unsafe {
                std::ptr::copy_nonoverlapping(lhs_src, dst, lhs_len);
                std::ptr::copy_nonoverlapping(rhs_src, dst.add(lhs_len), rhs_len);
            }

            EmbeddedVMString {
                len: new_len as u32,
                bytes: new_data,
            }
            .into()
        } else {
            let mut new_bytes = Vec::with_capacity(new_len);
            new_bytes.extend_from_slice(self.as_bytes());
            new_bytes.extend_from_slice(rhs.as_bytes());

            // Safety: VMString are guaranteed to only contain UTF-8 encoded bytes
            let string = unsafe { String::from_utf8_unchecked(new_bytes) };

            new_heap_string(string)
        }
    }
}

impl Add for HeapVMString {
    type Output = VMString;

    fn add(self, rhs: Self) -> Self::Output {
        concat(self, rhs)
    }
}

// TODO: find or introduce a trait to reduce this duplication?

fn concat<S1, S2>(a: S1, b: S2) -> VMString
where
    S1: AsStr,
    S2: AsStr,
{
    let concatenated = [a.as_str(), b.as_str()].concat();

    new_heap_string(concatenated)
}
