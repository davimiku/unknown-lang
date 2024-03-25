use util_macros::{assert_matches, assert_some};

use crate::{expr::Mutability, tests::parse_expr, Expr, TypeExpr};

#[test]
fn int_let_binding() {
    let input = "let a = 1";

    let parsed = parse_expr(input);

    let let_binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(let_binding.name().unwrap().text(), "a");

    assert_eq!(let_binding.mutability(), Mutability::Not);

    let value = assert_some!(let_binding.value());
    let value = assert_matches!(value, Expr::IntLiteral);
    assert_eq!(value.as_i64(), Some(1));
}

#[test]
fn int_let_mut_binding() {
    let input = "let mut a = 1";

    let parsed = parse_expr(input);

    let let_binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(let_binding.name().unwrap().text(), "a");

    assert_eq!(let_binding.mutability(), Mutability::Mut);

    let value = assert_some!(let_binding.value());
    let value = assert_matches!(value, Expr::IntLiteral);
    assert_eq!(value.as_i64(), Some(1));
}

#[test]
fn int_let_binding_with_type_annotation() {
    let input = "let a: Int = 1";

    let parsed = parse_expr(input);

    let let_binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(let_binding.name().unwrap().text(), "a");

    assert_eq!(let_binding.mutability(), Mutability::Not);

    let type_annotation = assert_some!(let_binding.type_annotation());
    let type_annotation = assert_matches!(type_annotation, TypeExpr::Ident);
    assert_eq!(type_annotation.as_string(), "Int");

    let value = assert_some!(let_binding.value());
    let value = assert_matches!(value, Expr::IntLiteral);
    assert_eq!(value.as_i64(), Some(1));
}

#[test]
fn int_let_mut_binding_with_type_annotation() {
    let input = "let mut a: Int = 1";

    let parsed = parse_expr(input);

    let let_binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(let_binding.name().unwrap().text(), "a");

    assert_eq!(let_binding.mutability(), Mutability::Mut);

    let type_annotation = assert_some!(let_binding.type_annotation());
    let type_annotation = assert_matches!(type_annotation, TypeExpr::Ident);
    assert_eq!(type_annotation.as_string(), "Int");

    let value = assert_some!(let_binding.value());
    let value = assert_matches!(value, Expr::IntLiteral);
    assert_eq!(value.as_i64(), Some(1));
}
