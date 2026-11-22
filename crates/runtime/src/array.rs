use std::mem::{offset_of, size_of};

use crate::{GcHeader, ObjectKind, TraceDescriptor, TypeDescriptor};

/// Stable handle for an Array
#[repr(C)]
pub struct HopperArray {
    pub header: GcHeader,
    /// GC reference to the current backing allocation
    pub buffer: *mut GcHeader,
}

/// A movable backing allocation. Elements begin after the fixed metadata.
#[repr(C)]
pub struct HopperArrayBuffer {
    pub header: GcHeader,
    pub length: usize,
    pub capacity: usize,
    pub elements: [u8; 0],
}

pub const ARRAY_OWNER_BUFFER_OFFSET: usize = offset_of!(HopperArray, buffer);
pub const ARRAY_BUFFER_LENGTH_OFFSET: usize = offset_of!(HopperArrayBuffer, length);
pub const ARRAY_BUFFER_CAPACITY_OFFSET: usize = offset_of!(HopperArrayBuffer, capacity);
pub const ARRAY_BUFFER_ELEMENTS_OFFSET: usize = offset_of!(HopperArrayBuffer, elements);

unsafe fn destroy_array_owner(_header: *mut GcHeader) {}

/// The stable Array handle contains one traced reference to its buffer
pub static ARRAY_OWNER_DESCRIPTOR: TypeDescriptor = TypeDescriptor {
    name: "ArrayOwner",
    kind: ObjectKind::Record,
    trace: TraceDescriptor::Fields(&[offset_of!(HopperArray, buffer) as u32]),
    destroy: destroy_array_owner,
};

const _: () = assert!(ARRAY_BUFFER_LENGTH_OFFSET >= size_of::<GcHeader>());
