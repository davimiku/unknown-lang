use util_macros::{assert_matches, assert_some};

use crate::tests::parse_expr;
use crate::{Expr, TypeExpr};

#[test]
fn type_binding_alias() {
    let input = "type F = Float";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("F", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let ident = assert_matches!(type_expr, TypeExpr::Ident);
    assert_eq!(ident.as_string(), "Float");
}

#[test]
fn union_type() {
    let input = "type U = union ( a, b: B )";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("U", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let union = assert_matches!(type_expr, TypeExpr::Union__Old);
    let variants = union.variants();

    assert_eq!(variants.len(), 2);
    assert_eq!(variants[0].ident_as_string(), "a");
    assert_eq!(variants[1].ident_as_string(), "b");
}

#[test]
fn union_type_two() {
    let input = "type U = a | b";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("U", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let union = assert_matches!(type_expr, TypeExpr::Union);
    let variants = union.variants();

    assert_eq!(variants.len(), 2);
    assert_eq!(variants[0].ident_as_string(), "a");
    assert_eq!(variants[1].ident_as_string(), "b");
}

#[test]
fn union_type_three() {
    let input = "type U = a | b: B | c";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("U", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let union = assert_matches!(type_expr, TypeExpr::Union);
    let variants = union.variants();

    assert_eq!(variants.len(), 3);
    assert_eq!(variants[0].ident_as_string(), "a");
    assert_eq!(variants[1].ident_as_string(), "b");
    assert_eq!(variants[2].ident_as_string(), "c");

    let b_type = assert_some!(variants[1].type_expr());
    let b_type_ident = assert_matches!(b_type, TypeExpr::Ident);
    assert_eq!(b_type_ident.as_string(), "B");
}
