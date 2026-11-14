use expect_test::expect;

use crate::grammar::check;

#[test]
fn parse_multiple_string_literals() {
    check(
        r#""a1"
"b2"
"c3"
"d4""#,
        expect![[r#"
Root@0..19
  StringLiteralExpr@0..4
    StringLiteralExpr@0..4 "\"a1\""
  Newline@4..5
    Newline@4..5 "\n"
  StringLiteralExpr@5..9
    StringLiteralExpr@5..9 "\"b2\""
  Newline@9..10
    Newline@9..10 "\n"
  StringLiteralExpr@10..14
    StringLiteralExpr@10..14 "\"c3\""
  Newline@14..15
    Newline@14..15 "\n"
  StringLiteralExpr@15..19
    StringLiteralExpr@15..19 "\"d4\"""#]],
    )
}
