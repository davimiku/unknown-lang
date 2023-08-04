pub use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

// TODO: currently getting the string data requires 2 dereferences
// Instead a struct could be created that keeps a Gc pointer for liveness
// and also a raw (unsafe) pointer to access the data directly.
//
// At the cost of stack size = 24 rather than size = 16 ?
// https://github.com/Manishearth/rust-gc/issues/60
#[derive(Trace, Finalize)]
struct EXPERIMENT__GcString {
    holder: Gc<String>, // Gc pointer to the GcBox to keep it alive
    #[unsafe_ignore_trace]
    data: *const u8, // Actual raw pointer to the underlying string data we're referencing
}

struct Test(Gc<u8>);

struct Test2(*const String);

/// Helper function to cast the raw bytes
///
/// # Safety
///
/// The raw_bytes passed in must have been created from Gc::to_raw
pub unsafe fn cast_bytes_to_Gc<T: Trace>(raw_bytes: u64) -> Gc<T> {
    Gc::from_raw(raw_bytes as *const T)
}

// TODO: is it worth newtyping the Gc struct?
// note: I think so, but it is awkward
//
// use gc::Trace;
// use the derive_more crate for AsRef, Deref, etc.
// #[derive(AsRef, Debug, Clone, Deref, PartialEq)]
// pub struct Gc<T: Trace + 'static>(Gc<T>);

// impl<T: Trace> Gc<T> {
//     pub fn new(value: T) -> Self {
//         Self(Gc::new(value))
//     }

//     pub fn into_raw(self) -> *const T {
//         Gc::into_raw(self.0)
//     }

//     pub unsafe fn from_raw(ptr: *const T) -> Self {
//         Self(Gc::from_raw(ptr))
//     }

//     pub fn ptr_eq(this: &Gc<T>, other: &Gc<T>) -> bool {
//         Gc::ptr_eq(&this.0, &other.0)
//     }
// }

// #[cfg(test)]
// mod tests {

//     use super::*;

//     pub fn alloc_2_strings(s1: &str, s2: &str) -> (Gc<String>, Gc<String>) {
//         let a = Gc::new(String::from(s1));
//         let b = Gc::new(String::from(s2));

//         (a, b)
//     }

//     #[test]
//     fn strings_equal_by_contents() {
//         let (a, b) = alloc_2_strings("hello", "hello");

//         // derefs the pointers and compares the String contents
//         assert_eq!(a, b);
//     }

//     #[test]
//     fn pointers_not_equal() {
//         let (a, b) = alloc_2_strings("hello", "hello");

//         assert!(!Gc::ptr_eq(&a, &b));

//         let a = Gc::into_raw(a);
//         let b = Gc::into_raw(b);

//         // raw pointers are not the same
//         assert_ne!(a, b);

//         unsafe {
//             let a = Gc::from_raw(a);
//             let b = Gc::from_raw(b);

//             // derefs the pointers and compares the String contents
//             // because Gc is Deref
//             assert_eq!(a, b);
//         }
//     }

//     #[test]
//     fn pointers_equal() {
//         let a = Gc::new(String::from("yeet"));

//         let b = a.clone();

//         assert!(Gc::ptr_eq(&a, &b));
//         assert_eq!(a, b);

//         let a = Gc::into_raw(a);
//         let b = Gc::into_raw(b);

//         // raw pointers are the same
//         assert_eq!(a, b);
//     }

//     #[test]
//     fn store_pointers_in_bytecode() {
//         let a = Gc::new(String::from("never"));
//         let b = Gc::new(String::from("yeet"));
//         let c = Gc::new(String::from("shredded"));
//         let d = Gc::new(String::from("wheat"));

//         let a_raw = Gc::into_raw(a);
//         let b_raw = Gc::into_raw(b);
//         let c_raw = Gc::into_raw(c);
//         let d_raw = Gc::into_raw(d);

//         let a_ptr = a_raw as *const u8;
//         let b_ptr = b_raw as *const u8;
//         let c_ptr = c_raw as *const u8;
//         let d_ptr = d_raw as *const u8;

//         println!("{a_ptr:?}");
//         println!("{b_ptr:?}");
//         println!("{c_ptr:?}");
//         println!("{d_ptr:?}");
//     }
// }
