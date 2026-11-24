use util_macros::{assert_matches, assert_some};

use crate::{tests::parse_expr, Expr};

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
fn ufcs_target_exposes_receiver_and_member() {
    let parsed = parse_expr("point.distance_from other");
    let call = assert_matches!(parsed, Expr::Call);

    let (receiver, member) = call.ufcs_target().unwrap();
    let receiver = assert_matches!(receiver, Expr::Ident);

    assert_eq!(receiver.as_string(), "point");
    assert_eq!(member.as_string(), "distance_from");
}

#[test]
fn ufcs_target_exposes_operator_member() {
    let parsed = parse_expr("point.+ other");
    let call = assert_matches!(parsed, Expr::Call);

    let (receiver, member) = call.ufcs_target().unwrap();
    let receiver = assert_matches!(receiver, Expr::Ident);

    assert_eq!(receiver.as_string(), "point");
    assert_eq!(member.as_string(), "+");
}

#[test]
fn escaped_binding_name_is_unquoted_in_ast() {
    let parsed = parse_expr("let `+` = 1");
    let binding = assert_matches!(parsed, Expr::LetBinding);

    assert_eq!(binding.name_as_string().as_deref(), Some("+"));
}
