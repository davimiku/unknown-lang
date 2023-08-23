use hir::lower;
use jit::JIT;

mod builtins;
mod ext;
mod jit;
#[cfg(test)]
mod tests;
mod translate;

pub fn compile_function(input: &str) -> Result<*const u8, String> {
    let (function, context) = lower(input, hir::LowerTarget::Function);

    if !context.diagnostics.is_empty() {
        for diag in context.diagnostics {
            eprintln!("{diag:?}");
        }
        panic!("Found diagnostics while lowering")
    }

    let function = context.expr(function);
    let function = crate::assert_matches!(function, hir::Expr::Function);

    let mut jit = JIT::with_builtins();
    jit.compile_function(function, &context).map_err(|e| {
        dbg!(&e);
        e.to_string()
    })
}

/// Asserts that the provided enum is the provided variant,
/// and extracts the inner value.
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
pub(crate) use assert_matches;
