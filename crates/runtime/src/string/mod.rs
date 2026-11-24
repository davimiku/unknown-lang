// TODO - move more into here from ffi.rs
// API should be ffi.rs gets the Runtime (if needed), and passes it
// in here and this does the rest.
// Basically this module (and other modules for builtins) would be
// like Runtime --> GcRef
//
// or for stuff like print_string: GcRef --> side effects

use crate::errors::AllocError;
use crate::memory_recycling::{
    GcHeader, GcRef, Heap, ObjectKind, RootSet, TraceDescriptor, TypeDescriptor,
};

#[repr(C)]
pub struct HopperString {
    pub header: GcHeader,
    /// byte length
    pub len: i64,
    /// UTF-8 encoded bytes
    pub bytes: *mut u8,
}

pub static HOPPER_STRING_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
    name: "String",
    kind: ObjectKind::String,
    trace: TraceDescriptor::None,
    destroy: destroy_hopper_string,
};

unsafe fn destroy_hopper_string(gc_header: *mut GcHeader) {
    let hstring = gc_header.cast::<HopperString>();
    let len = usize::try_from((*hstring).len).expect("valid Hopper String length");
    let bytes_ptr = (*hstring).bytes;
    let slice = std::ptr::slice_from_raw_parts_mut(bytes_ptr, len);
    let bytes = unsafe { Box::from_raw(slice) };
    drop(bytes);
}

impl Heap {
    pub fn alloc_string(
        &mut self,
        bytes: &[u8],
        roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        std::str::from_utf8(bytes).map_err(|_| AllocError::InvalidUtf8)?;
        let byte_len = bytes.len();
        let len = i64::try_from(byte_len).map_err(|_| AllocError::OutOfMemory)?;
        let mut copied = Vec::new();
        copied
            .try_reserve_exact(byte_len)
            .map_err(|_| AllocError::OutOfMemory)?;
        copied.extend_from_slice(bytes);
        let boxed = copied.into_boxed_slice();

        let size = size_of::<HopperString>();
        let string_ref = self.alloc_raw(size, &HOPPER_STRING_DESCRIPTOR, roots)?;
        let ptr = Box::into_raw(boxed) as *mut u8;

        unsafe {
            let gc_header = string_ref.as_ptr();
            let hstring = gc_header.cast::<HopperString>();
            (*hstring).bytes = ptr;
            (*hstring).len = len;
        };
        Ok(string_ref)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_recycling::EmptyRootSet;

    #[test]
    fn alloc_string_copy_stores_length_and_bytes() {
        let mut heap = Heap::default();
        let mut roots = EmptyRootSet;

        let value = b"hopper";
        let gc_ref = heap.alloc_string(value, &mut roots).unwrap();

        unsafe {
            let string = gc_ref.cast::<HopperString>();
            assert_eq!((*string).len, value.len() as i64);
            assert_eq!(
                std::slice::from_raw_parts((*string).bytes, (*string).len as usize),
                value
            );
        }
    }

    #[test]
    fn string_length_is_a_byte_count_and_bytes_need_not_be_terminated() {
        let mut heap = Heap::default();
        let mut roots = EmptyRootSet;

        let value = b"a\0b";
        let gc_ref = heap.alloc_string(value, &mut roots).unwrap();

        unsafe {
            let string = gc_ref.cast::<HopperString>();
            assert_eq!((*string).len, 3);
            assert_eq!(
                std::slice::from_raw_parts((*string).bytes, (*string).len as usize),
                value
            );
        }
    }

    #[test]
    fn string_allocation_rejects_invalid_utf8() {
        let mut heap = Heap::default();
        let mut roots = EmptyRootSet;

        assert!(matches!(
            heap.alloc_string(&[0xff], &mut roots),
            Err(AllocError::InvalidUtf8)
        ));
    }
}
