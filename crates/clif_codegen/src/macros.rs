macro_rules! assert_byval {
    ($cvalue:expr) => {
        match $cvalue {
            CValue::ByVal { val, .. } => val,
            _ => unreachable!(
                "Internal Compiler Error (CLIF): Expected CValue::ByVal, got {:?}",
                $cvalue
            ),
        }
    };
}

pub(crate) use assert_byval;
