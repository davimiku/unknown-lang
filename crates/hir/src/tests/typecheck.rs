use util_macros::assert_matches;

use crate::{lower_script, ContextDisplay, Expr, Type};

fn check(input: &str, expected_return_type: &Type) {
    let (root_expr, context) = lower_script(input);

    if !context.diagnostics.is_empty() {
        for diag in context.diagnostics.iter() {
            eprintln!("{}", diag.display(&context));
        }
    }
    assert_eq!(context.diagnostics, vec![]);

    let root_expr = context.expr(root_expr);
    let root_func = assert_matches!(root_expr, Expr::Function);

    let return_type = context.expr_type(root_func.overloads[0].body);
    assert_eq!(return_type, expected_return_type);
}

#[test]
fn int_literal() {
    let input = "1";

    let expected_return_type = Type::IntLiteral(1);

    check(input, &expected_return_type);
}

#[test]
fn int_addition() {
    let input = "1 + 2";

    let expected_return_type = Type::Int;

    check(input, &expected_return_type);
}

#[test]
fn float_addition() {
    let expected_return_type = Type::Float;

    check("1.0 + 2.0", &expected_return_type);
    check("1 + 2.0", &expected_return_type);
    check("1.0 + 2", &expected_return_type);
}
