use expect_test::expect;

use crate::check_expr;

#[test]
fn parse_int() {
    check_expr(
        "123",
        expect![[r#"
IntExpr@0..3
  IntLiteral@0..3 "123""#]],
    );
}

#[test]
fn parse_int_with_separators() {
    check_expr(
        "123_456_789",
        expect![[r#"
IntExpr@0..11
  IntLiteral@0..11 "123_456_789""#]],
    )
}

#[test]
fn parse_int_preceded_by_emptyspace() {
    check_expr(
        "   9876",
        expect![[r#"
IntExpr@0..7
  Emptyspace@0..3 "   "
  IntLiteral@3..7 "9876""#]],
    );
}

#[test]
fn parse_int_followed_by_emptyspace() {
    check_expr(
        "999   ",
        expect![[r#"
IntExpr@0..6
  IntLiteral@0..3 "999"
  Emptyspace@3..6 "   ""#]],
    );
}

#[test]
fn parse_int_surrounded_by_emptyspace() {
    check_expr(
        " 123     ",
        expect![[r#"
IntExpr@0..9
  Emptyspace@0..1 " "
  IntLiteral@1..4 "123"
  Emptyspace@4..9 "     ""#]],
    );
}

#[test]
fn parse_string_literal() {
    check_expr(
        r#""hello""#,
        expect![[r#"
StringExpr@0..7
  StringExpr@0..7 "\"hello\"""#]],
    )
}
