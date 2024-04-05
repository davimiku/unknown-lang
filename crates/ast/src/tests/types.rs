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
fn type_binding_union() {
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
fn type_binding_union_three() {
    let input = "type U = a | b | c";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("U", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let union = assert_matches!(type_expr, TypeExpr::Union);
    assert_eq!(union.variants().len(), 3);
}
