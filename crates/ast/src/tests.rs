use super::*;

/// Asserts that the provided `Option` is `Some`
/// and returns the unwrapped value.
macro_rules! assert_some {
    ($value:expr) => {{
        assert!($value.is_some());
        $value.unwrap()
    }};
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

fn parse_node(input: &str) -> SyntaxNode {
    parser::parse(input).syntax()
}

fn parse_expr(input: &str) -> Expr {
    let root = Root::cast(parse_node(input)).expect("valid Root node");
    let mut exprs: Vec<Expr> = root.exprs().collect();
    assert!(exprs.len() == 1);
    exprs.remove(0)
}

#[test]
fn call_no_arguments() {
    let input = "my_func";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    assert!(call.args().is_none());
}

#[test]
fn call_empty_arg() {
    let input = "my_func ()";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    let path = assert_some!(call.path());
    let ident_string = assert_some!(path.ident_strings().next());
    assert_eq!(ident_string, "my_func");
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
    let expected_ident_strings = ["a", "my_func"];

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    let path = assert_some!(call.path());
    let idents = path.ident_strings();
    for (i, ident) in idents.enumerate() {
        assert_eq!(ident, expected_ident_strings[i])
    }
    assert!(call.args().is_none());
}

#[test]
fn empty_function() {
    let input = "() -> { }";
    let expected_param_list = None;
    let expected_return_type = None;

    let parsed = parse_expr(input);

    let function = assert_matches!(parsed, Expr::Function);
    assert_eq!(function.param_list().params().next(), expected_param_list);
    assert_eq!(function.return_type(), expected_return_type);
}

#[test]
fn nullary_function_with_return_type() {
    let input = "() -> Int { }";
    let expected_param_list = None;
    let expected_return_type = "Int";

    let parsed = parse_expr(input);

    let function = assert_matches!(parsed, Expr::Function);
    assert_eq!(function.param_list().params().next(), expected_param_list);
    let return_type = assert_some!(function
        .return_type()
        .and_then(|type_expr| type_expr.as_path())
        .and_then(|path| path.idents().next())
        .and_then(|ident| ident.name_token()));
    assert_eq!(return_type.text(), expected_return_type);
}

#[test]
fn unary_function_with_explicit_param_type() {
    let input = "(a: A, b: B) -> { }";
    let expected_idents = ["a", "b"];
    let expected_type_idents = ["A", "B"];

    let parsed = parse_expr(input);

    let function = assert_matches!(parsed, Expr::Function);
    let param_list = function.param_list();
    let params = param_list.params();
    for (i, param) in params.enumerate() {
        let name = assert_some!(assert_some!(param.ident()).name());
        assert_eq!(name, expected_idents[i]);

        let type_path = assert_some!(param.type_expr().and_then(|t| t.as_path()));
        let type_ident = assert_some!(type_path.ident_strings().next());
        assert_eq!(type_ident, expected_type_idents[i]);
    }
}

#[test]
fn add_int_and_function() {
    // not a valid expression by the type checker, but should still produce an AST
    let input = "1 + (() -> { })";
    let expected_lhs = "1";
    let expected_rhs_param_list = None;
    let expected_rhs_return_type = None;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    let lhs = assert_matches!(lhs, Expr::IntLiteral);
    let lhs = assert_some!(lhs.value_as_string());
    assert_eq!(expected_lhs, lhs);

    let rhs = assert_some!(binary.rhs());
    let rhs = assert_matches!(rhs, Expr::Paren);
    let rhs = assert_matches!(assert_some!(rhs.expr()), Expr::Function);
    assert_eq!(rhs.param_list().params().next(), expected_rhs_param_list);
    assert_eq!(rhs.return_type(), expected_rhs_return_type);
}

#[test]
fn loop_empty_body() {
    let input = "loop { }";

    let parsed = parse_expr(input);

    let loop_expr = assert_matches!(parsed, Expr::Loop);
    let block = assert_some!(loop_expr.block());
    assert!(block.exprs().count() == 0);
}

#[test]
fn if_expr_empty() {
    let input = "if a {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Call);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    assert!(if_expr.else_branch().is_none());
}
