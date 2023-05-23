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

fn parse_expr(input: &str) -> Expr {
    // TODO: remove and switch to more of a opaque box test
    Expr::cast(parser::test_parse_expr(input).syntax()).unwrap()
}

fn check_function(parsed: Expr, expected_idents: &[&str], expected_type_idents: &[&str]) {
    let function = assert_matches!(parsed, Expr::Function);
    let param_list = function.param_list();
    let params = param_list.params();
    for (i, param) in params.enumerate() {
        let name = assert_some!(param.ident());
        assert_eq!(name, expected_idents[i]);

        if let Some(TypeExpr::Path(path)) = param.type_expr() {
            let type_name = path.ident_strings().next().unwrap();
            assert_eq!(type_name, expected_type_idents[i]);
        }
    }
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
    assert_eq!(path.ident_strings().next(), Some(String::from("my_func")));
}

#[test]
fn call_empty_arg() {
    let input = "my_func ()";

    let parsed = parse_expr(input);

    let call = assert_matches!(parsed, Expr::Call);
    let callee = assert_some!(call.callee());
    let path = assert_matches!(callee, Expr::Path);
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

    let path = assert_matches!(parsed, Expr::Path);
    let idents = path.ident_strings();
    for (i, ident) in idents.enumerate() {
        assert_eq!(ident, expected_ident_strings[i])
    }
}

#[test]
fn empty_function() {
    let input = "() -> { }";
    let expected_idents = [];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents);
}

#[test]
fn unary_function() {
    let input = "a -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents);
}

#[test]
fn unary_function_with_parens() {
    let input = "(a) -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = [];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents);
}

#[test]
fn unary_function_with_param_type() {
    let input = "(a: A) -> {}";
    let expected_idents = ["a"];
    let expected_type_idents = ["A"];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents);
}

#[test]
fn binary_function_with_explicit_param_type() {
    let input = "(a: A, b: B) -> { }";
    let expected_idents = ["a", "b"];
    let expected_type_idents = ["A", "B"];

    let parsed = parse_expr(input);

    check_function(parsed, &expected_idents, &expected_type_idents);
}

#[test]
fn add_int_and_function() {
    // not a valid expression by the type checker, but should still produce a valid AST
    let input = "1 + (() -> { })";
    let expected_lhs = "1";
    let expected_rhs_param_list = None;

    let parsed = parse_expr(input);

    let binary = assert_matches!(parsed, Expr::Binary);

    let lhs = assert_some!(binary.lhs());
    let lhs = assert_matches!(lhs, Expr::IntLiteral);
    let lhs = assert_some!(lhs.value_as_string());
    assert_eq!(expected_lhs, lhs);

    let rhs = assert_some!(binary.rhs());
    let rhs = assert_matches!(rhs, Expr::Paren);
    let rhs = assert_some!(rhs.expr());
    let rhs = assert_matches!(rhs, Expr::Function);
    assert_eq!(rhs.param_list().params().next(), expected_rhs_param_list);
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
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    assert!(if_expr.else_branch().is_none());
}

#[test]
fn if_else_expr() {
    let input = "if a {} else {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    let else_branch = assert_some!(if_expr.else_branch());
    assert_matches!(else_branch, Expr::Block);
}

#[test]
fn if_else_if_expr() {
    let input = "if a {} else if b {}";

    let parsed = parse_expr(input);

    let if_expr = assert_matches!(parsed, Expr::If);
    let condition = assert_some!(if_expr.condition_expr());
    assert_matches!(condition, Expr::Path);

    let then_branch = assert_some!(if_expr.then_branch());
    assert_matches!(then_branch, Expr::Block);

    let else_branch = assert_some!(if_expr.else_branch());
    assert_matches!(else_branch, Expr::If);
}

#[test]
fn int_let_binding() {
    let input = "let a = 1";

    let parsed = parse_expr(input);

    let let_binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(let_binding.name().unwrap().text(), "a");
}

#[test]
fn return_statement() {
    let input = "return 1";

    let parsed = parse_expr(input);

    let return_statement = assert_matches!(parsed, Expr::Return);
    let return_value = assert_some!(return_statement.return_value());
    assert_matches!(return_value, Expr::IntLiteral);
}
