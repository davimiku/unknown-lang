use gc::Gc;

pub fn alloc_2_strings(s1: &str, s2: &str) -> (Gc<String>, Gc<String>) {
    let a = Gc::new(String::from(s1));
    let b = Gc::new(String::from(s2));

    (a, b)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn strings_equal_by_contents() {
        let (a, b) = alloc_2_strings("hello", "hello");

        // derefs the pointers and compares the String contents
        assert_eq!(a, b);
    }

    #[test]
    fn pointers_not_equal() {
        let (a, b) = alloc_2_strings("hello", "hello");

        assert!(!Gc::ptr_eq(&a, &b));

        let a = Gc::into_raw(a);
        let b = Gc::into_raw(b);

        // raw pointers are not the same
        assert_ne!(a, b);

        unsafe {
            let a = Gc::from_raw(a);
            let b = Gc::from_raw(b);

            // derefs the pointers and compares the String contents
            assert_eq!(a, b);
        }
    }

    #[test]
    fn pointers_equal() {
        let a = Gc::new(String::from("yeet"));

        let b = a.clone();

        assert!(Gc::ptr_eq(&a, &b));
        assert_eq!(a, b);

        let a = Gc::into_raw(a);
        let b = Gc::into_raw(b);

        // raw pointers are the same
        assert_eq!(a, b);
    }
}
