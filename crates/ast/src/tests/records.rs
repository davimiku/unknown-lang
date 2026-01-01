use parser::test_parse_type_expr;
use util_macros::{assert_matches, assert_some};

use crate::tests::parse_expr;
use crate::{Expr, TypeExpr};

#[test]
fn record_empty() {
    let input = "[]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);
}

#[test]
fn record_one_item() {
    let input = "[ key = value ]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);
}

#[test]
fn record_one_item_trailing_comma() {
    let input = "[ key = value, ]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);

    // TODO - walk AST for more assertions
}

#[test]
fn record_two_items() {
    let input = "[ key1 = value1, key2 = value2 ]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);

    // TODO - walk AST for more assertions
}

#[test]
fn record_two_items_trailing_comma() {
    let input = "[ key1 = value1, key2 = value2, ]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);
    // TODO - walk AST for more assertions
}

#[test]
#[ignore = "FIXME - infinite loop"]
fn invalid_record_literal_no_keys() {
    let input = "[1, 2, 3]";

    let parsed = parse_expr(input);

    let record_literal = assert_matches!(parsed, Expr::RecordLiteral);

    // assert for some level of recovery, maybe CompoundTypeItems have values but no keys
}

#[test]
fn record_type_empty() {
    let input = "type EmptyRecord = []";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("EmptyRecord", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let record = assert_matches!(type_expr, TypeExpr::Record);
    let fields = record.fields();

    assert!(fields.is_empty());
}

#[test]
fn record_type_one_field() {
    let input = "type OneField = [ field: Type ]";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("OneField", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let record = assert_matches!(type_expr, TypeExpr::Record);
    let fields = record.fields();

    assert_eq!(fields.len(), 1);
    let field = &fields[0];
    assert_eq!(field.ident_as_string(), "field");
    let field_type = assert_some!(field.type_expr());
    let type_ident = assert_matches!(field_type, TypeExpr::Ident);
    assert_eq!(type_ident.as_string(), "Type")
}

#[test]
fn record_type_two_fields() {
    let input = "type TwoFields = [ field1: Type1, field2: Type2 ]";

    let parsed = parse_expr(input);

    let type_binding = assert_matches!(parsed, Expr::TypeBinding);

    let name = assert_some!(type_binding.name()).to_string();
    assert_eq!("TwoFields", name);

    let type_expr = assert_some!(type_binding.type_expr());
    let record = assert_matches!(type_expr, TypeExpr::Record);
    let fields = record.fields();

    assert_eq!(fields.len(), 2);

    let field1 = &fields[0];
    assert_eq!(field1.ident_as_string(), "field1");
    let field_type1 = assert_some!(field1.type_expr());
    let type_ident1 = assert_matches!(field_type1, TypeExpr::Ident);
    assert_eq!(type_ident1.as_string(), "Type1");

    let field2 = &fields[1];
    assert_eq!(field2.ident_as_string(), "field2");
    let field_type2 = assert_some!(field2.type_expr());
    let type_ident2 = assert_matches!(field_type2, TypeExpr::Ident);
    assert_eq!(type_ident2.as_string(), "Type2")
}
