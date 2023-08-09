use hir::{lower_input, Interner};
use jit::JIT;

mod builtins;
mod ext;
mod jit;
#[cfg(test)]
mod tests;
mod translate;

pub fn compile(input: &str) -> Result<*const u8, String> {
    let mut interner = Interner::default();
    let (program, context) = lower_input(input, &mut interner);

    if !context.diagnostics.is_empty() {
        for diag in context.diagnostics {
            eprintln!("{diag:?}");
        }
        panic!("Found diagnostics while lowering")
    }

    let program = context.expr(program);
    let program_block = crate::assert_matches!(program, hir::Expr::Block);
    let first = program_block.exprs[0];
    let first = crate::assert_matches!(context.expr(first), hir::Expr::Statement);
    let function = context.expr(*first);
    let function = crate::assert_matches!(function, hir::Expr::Function);

    let mut jit = JIT::with_builtins();
    jit.compile_function(function, &context).map_err(|e| {
        dbg!(&e);
        e.to_string()
    });

    todo!()
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

#[cfg(test)]
mod testz {
    use crate::compile;

    #[test]
    fn test_int() {
        let input = r#"() -> {
            2 + 3
        }"#;

        compile(input);
    }
}
