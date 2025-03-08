use util_macros::{assert_matches, assert_some};

use crate::expr::Pattern;
use crate::tests::parse_expr;
use crate::Expr;

#[test]
fn match_arms_empty() {
    // valid syntax, would produce a value of type `Never`
    let input = "match u {}";

    let parsed = parse_expr(input);
    let match_expr = assert_matches!(parsed, Expr::Match);

    let scrutinee = assert_some!(match_expr.scrutinee());
    let scrutinee = assert_matches!(assert_some!(scrutinee.expr()), Expr::Path);
    assert_eq!(assert_some!(scrutinee.subject_as_ident()).as_string(), "u");

    assert_eq!(match_expr.arms().len(), 0);
}

#[test]
fn match_arms_one() {
    let input = "match u {
    .a -> 4
}";

    let parsed = parse_expr(input);
    let match_expr = assert_matches!(parsed, Expr::Match);

    let scrutinee = assert_some!(match_expr.scrutinee());
    let scrutinee = assert_matches!(assert_some!(scrutinee.expr()), Expr::Path);
    assert_eq!(assert_some!(scrutinee.subject_as_ident()).as_string(), "u");

    let arms = match_expr.arms();
    assert_eq!(arms.len(), 1);

    assert_matches!(assert_some!(arms[0].pattern()), Pattern::DotIdentifier);
}

#[test]
fn match_arms_one_with_payload() {
    let input = "match u {
    .a i -> i
}";

    let parsed = parse_expr(input);
    let match_expr = assert_matches!(parsed, Expr::Match);

    let scrutinee = assert_some!(match_expr.scrutinee());
    let scrutinee = assert_matches!(assert_some!(scrutinee.expr()), Expr::Path);
    assert_eq!(assert_some!(scrutinee.subject_as_ident()).as_string(), "u");

    let arms = match_expr.arms();
    assert_eq!(arms.len(), 1);

    let dot_identifier = assert_matches!(assert_some!(arms[0].pattern()), Pattern::DotIdentifier);
    let inner_ident = assert_matches!(
        assert_some!(dot_identifier.inner_pattern()),
        Pattern::Identifier
    );
    assert_eq!(inner_ident.as_string(), "i");
}

#[test]
fn match_arms_two() {
    let input = "match u {
    .a -> 4
    .b -> 8
}";

    let parsed = parse_expr(input);
    let match_expr = assert_matches!(parsed, Expr::Match);

    let scrutinee = assert_some!(match_expr.scrutinee());
    let scrutinee = assert_matches!(assert_some!(scrutinee.expr()), Expr::Path);
    assert_eq!(assert_some!(scrutinee.subject_as_ident()).as_string(), "u");

    let arms = match_expr.arms();
    assert_eq!(arms.len(), 2);

    assert_matches!(assert_some!(arms[0].pattern()), Pattern::DotIdentifier);
    assert_matches!(assert_some!(arms[1].pattern()), Pattern::DotIdentifier);
}

#[test]
fn match_arms_two_with_payloads() {
    let input = "match u {
    .a i -> i
    .b i -> i
}";

    let parsed = parse_expr(input);
    let match_expr = assert_matches!(parsed, Expr::Match);

    let scrutinee = assert_some!(match_expr.scrutinee());
    let scrutinee = assert_matches!(assert_some!(scrutinee.expr()), Expr::Path);
    assert_eq!(assert_some!(scrutinee.subject_as_ident()).as_string(), "u");

    let arms = match_expr.arms();
    assert_eq!(arms.len(), 2);

    assert_matches!(assert_some!(arms[0].pattern()), Pattern::DotIdentifier);
    assert_matches!(assert_some!(arms[1].pattern()), Pattern::DotIdentifier);
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
