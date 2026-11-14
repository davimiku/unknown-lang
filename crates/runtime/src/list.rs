use crate::GcHeader;

/// Header for a managed homogeneous List. Element storage begins immediately
/// after this header and is described by the object's `TraceDescriptor::Array`.
#[repr(C)]
pub struct HopperList {
    pub header: GcHeader,
    pub length: usize,
    pub capacity: usize,
    pub elements: [u8; 0],
}

pub const LIST_ELEMENTS_OFFSET: usize = std::mem::size_of::<HopperList>();
