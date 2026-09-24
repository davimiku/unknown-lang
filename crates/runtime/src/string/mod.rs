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
    pub len: usize,
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
    unsafe {
        let bytes_ptr = (*hstring).bytes;
        let len = (*hstring).len;
        let slice = std::ptr::slice_from_raw_parts_mut(bytes_ptr, len);
        drop(Box::from_raw(slice));
    }
}

impl Heap {
    pub fn alloc_string(
        &mut self,
        bytes: &[u8],
        roots: &mut dyn RootSet,
    ) -> Result<GcRef, AllocError> {
        let len = bytes.len();
        let boxed: Box<[u8]> = bytes.to_vec().into_boxed_slice();

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
            assert_eq!((*string).len, value.len());
            assert_eq!(
                std::slice::from_raw_parts((*string).bytes, (*string).len),
                value
            );
        }
    }
}
