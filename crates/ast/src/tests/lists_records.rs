use util_macros::{assert_matches, assert_some};

use crate::{tests::parse_expr, Expr};

#[test]
fn empty_record() {
    let input = "[]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);
}

#[test]
fn list_literal_int() {
    let input = "[1, 2, 3]";

    let parsed = parse_expr(input);

    let list_literal = assert_matches!(parsed, Expr::RecordLiteral);
    // let items: Vec<_> = list_literal
    //     .items()
    //     .map(|item| assert_matches!(item, Expr::IntLiteral))
    //     .collect();

    // let expected: Vec<i64> = vec![1, 2, 3];
    // let actual: Vec<i64> = items.iter().map(|item| item.as_i64().unwrap()).collect();

    // assert_eq!(actual, expected);
}

#[test]
#[ignore = "may remove this index syntax"]
fn list_literal_index() {
    let input = "[0, 1].0";

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);

    assert_matches!(assert_some!(path.subject()), Expr::RecordLiteral);
    assert_matches!(assert_some!(path.member()), Expr::IntLiteral);
}

#[test]
#[ignore = "may remove this index syntax"]
fn local_index() {
    let input = r#"arr.1"#;

    let parsed = parse_expr(input);

    let path = assert_matches!(parsed, Expr::Path);

    assert_matches!(assert_some!(path.subject()), Expr::Ident);
    assert_matches!(assert_some!(path.member()), Expr::IntLiteral);
}
