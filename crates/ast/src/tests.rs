use crate::{expr::Mutability, Expr, TypeExpr};
use util_macros::{assert_matches, assert_some};

mod bindings;
mod branches;
mod functions;
mod loops;

fn parse_expr(input: &str) -> Expr {
    let node = parser::test_parse_expr(input).syntax();
    Expr::cast(node).unwrap()
}

#[test]
fn string_concatenation() {
    let input = r#""Hello " ++ "World!""#;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    assert_matches!(lhs, Expr::StringLiteral);
    let rhs = assert_some!(binary.rhs());
    assert_matches!(rhs, Expr::StringLiteral);

    let op = assert_some!(binary.op());
    assert_eq!(op.text(), "++");
}

#[test]
fn simple_path() {
    let input = "my_func";

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);
    let ident = assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_eq!(ident.as_string(), "my_func");

    assert!(path.member().is_none());
}

#[test]
fn call_empty_arg() {
    let input = "my_func ()";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);

    let callee = assert_some!(call.callee());
    let path = assert_matches!(callee, Expr::Path);
    let ident = assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_eq!(ident.as_string(), "my_func");
    assert!(path.member().is_none());

    let call_args = assert_some!(call.args());
    assert_eq!(0, call_args.args().count());
}

#[test]
fn call_one_arg() {
    let input = "my_func 1";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    let call_args = assert_some!(call.args());
    assert_eq!(1, call_args.args().count());
}

#[test]
fn call_arguments() {
    let input = "my_func (1, 2)";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    let call_args = assert_some!(call.args());
    assert_eq!(2, call_args.args().count());
}

#[test]
fn call_path_no_args() {
    let input = "a.my_func";

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);
    let subject_ident = assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_eq!(subject_ident.as_string(), "a");

    let member = assert_matches!(assert_some!(path.member()), Expr::Path);
    let member = assert_matches!(assert_some!(member.subject()), Expr::Ident);
    assert_eq!(member.as_string(), "my_func");
}

#[test]
fn add_int_and_function() {
    // not a valid expression by the type checker, but should still produce a valid AST
    let input = "1 + (fun () -> { })";
    let expected_lhs = "1";
    let expected_rhs_param_list = None;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    let lhs = assert_matches!(lhs, Expr::IntLiteral);
    let actual_lhs = assert_some!(lhs.as_string());
    assert_eq!(actual_lhs, expected_lhs);

    let rhs = assert_some!(binary.rhs());
    let rhs = assert_matches!(rhs, Expr::Paren);
    let rhs = assert_some!(rhs.expr());
    let rhs = assert_matches!(rhs, Expr::Function);
    assert_eq!(rhs.param_list().params().next(), expected_rhs_param_list);
}

#[test]
fn return_statement() {
    let input = "return 1";

    let parsed = parse_expr(input);

    let return_statement = assert_matches!(parsed, Expr::Return);
    let return_value = assert_some!(return_statement.return_value());
    assert_matches!(return_value, Expr::IntLiteral);
}

#[test]
fn array_literal_int() {
    let input = "[1, 2, 3]";

    let parsed = parse_expr(input);

    let array_literal = assert_matches!(parsed, Expr::ArrayLiteral);
    let items: Vec<_> = array_literal
        .items()
        .map(|item| assert_matches!(item, Expr::IntLiteral))
        .collect();

    let expected: Vec<i64> = vec![1, 2, 3];
    let actual: Vec<i64> = items.iter().map(|item| item.as_i64().unwrap()).collect();

    assert_eq!(actual, expected);
}

#[test]
fn array_literal_index() {
    let input = "[0, 1].0";

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);

    assert_matches!(assert_some!(path.subject()), Expr::ArrayLiteral);
    assert_matches!(assert_some!(path.member()), Expr::IntLiteral);
}

#[test]
fn local_index() {
    let input = r#"arr.1"#;

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);

    assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_matches!(assert_some!(path.member()), Expr::IntLiteral);
}
