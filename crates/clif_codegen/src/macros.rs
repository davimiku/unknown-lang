macro_rules! assert_val {
    ($cvalue:expr) => {
        match $cvalue {
            CValue::Val { val, .. } => val,
            _ => unreachable!(
                "Internal Compiler Error (CLIF): Expected CValue::ByVal, got {:?}",
                $cvalue
            ),
        }
    };
}

pub(crate) use assert_val;
