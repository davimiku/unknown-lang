use std::slice;

use crate::memory_recycling::GcHeader;
use crate::string::HopperString;

pub type RawGcRef = *mut GcHeader;

pub extern "C" fn alloc_string(bytes: *const u8, len: i64) -> RawGcRef {
    let runtime = crate::runtime();
    let mut runtime = runtime.lock().unwrap();

    // temporary root set for bootstrap; later this will come from frame walking
    let mut roots = crate::memory_recycling::EmptyRootSet;
    let gc_ref = runtime
        .heap
        .alloc_string(
            unsafe { std::slice::from_raw_parts(bytes, len as usize) },
            &mut roots,
        )
        .unwrap();

    gc_ref.as_ptr()
}

pub extern "C" fn print_string(gc_ref: RawGcRef) {
    let str_ref = gc_ref.cast::<HopperString>();

    let s: &str = unsafe {
        let len = (*str_ref).len;
        let src = (*str_ref).bytes;
        let bytes = slice::from_raw_parts(src, len);
        str::from_utf8_unchecked(bytes)
    };
    println!("{s}");
}
