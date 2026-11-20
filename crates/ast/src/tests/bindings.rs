use util_macros::{assert_matches, assert_some};

use crate::expr::Mutability;
use crate::tests::parse_expr;
use crate::{Expr, TypeExpr};

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

#[test]
fn let_binding_can_import_a_value() {
    let parsed = parse_expr("let sqrt = @core/math/sqrt");
    let binding = assert_matches!(parsed, Expr::LetBinding);
    assert_eq!(binding.name().unwrap().text(), "sqrt");

    let path = assert_matches!(assert_some!(binding.value()), Expr::ModulePath);
    assert_eq!(
        path.module_path(),
        vec!["core".to_owned(), "math".to_owned()]
    );
    assert_eq!(path.symbol().as_deref(), Some("sqrt"));
}

#[test]
fn module_binding_imports_a_namespace() {
    let parsed = parse_expr("module Collections = @core/collections");
    let binding = assert_matches!(parsed, Expr::ImportBinding);
    assert_eq!(binding.name().unwrap().text(), "Collections");
    assert_eq!(
        binding.namespace().unwrap(),
        vec!["core".to_owned(), "collections".to_owned()]
    );
}
