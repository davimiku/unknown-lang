/// Asserts that the provided enum is the provided variant,
/// and extracts the inner value.
#[macro_export]
macro_rules! assert_matches {
    ($value:expr, $variant:path) => {{
        assert!(matches!($value, $variant(_)));

        if let $variant(x) = $value {
            x
        } else {
            unreachable!()
        }
    }};
}

#[macro_export]
macro_rules! assert_some {
    ($value:expr) => {{
        assert!($value.is_some());
        $value.unwrap()
    }};
}
