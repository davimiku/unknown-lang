use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_simple_infix_expression() {
    check_expr(
        "1+2",
        expect![[r#"
InfixExpr@0..3
  IntLiteralExpr@0..1
    IntLiteral@0..1 "1"
  Plus@1..2 "+"
  IntLiteralExpr@2..3
    IntLiteral@2..3 "2""#]],
    );
}

#[test]
fn parse_binary_concat_expression() {
    check_expr(
        "\"Hello \" ++ \"World!\"",
        expect![[r#"
InfixExpr@0..20
  StringLiteralExpr@0..9
    StringLiteralExpr@0..8 "\"Hello \""
    Emptyspace@8..9 " "
  PlusPlus@9..11 "++"
  Emptyspace@11..12 " "
  StringLiteralExpr@12..20
    StringLiteralExpr@12..20 "\"World!\"""#]],
    )
}

#[test]
fn parse_left_associative_infix_expression() {
    check_expr(
        "1+2+3+4",
        expect![[r#"
InfixExpr@0..7
  InfixExpr@0..5
    InfixExpr@0..3
      IntLiteralExpr@0..1
        IntLiteral@0..1 "1"
      Plus@1..2 "+"
      IntLiteralExpr@2..3
        IntLiteral@2..3 "2"
    Plus@3..4 "+"
    IntLiteralExpr@4..5
      IntLiteral@4..5 "3"
  Plus@5..6 "+"
  IntLiteralExpr@6..7
    IntLiteral@6..7 "4""#]],
    );
}

#[test]
fn parse_right_associative_infix_expression() {
    check_expr(
        "1^2^3^4",
        expect![[r#"
InfixExpr@0..7
  IntLiteralExpr@0..1
    IntLiteral@0..1 "1"
  Caret@1..2 "^"
  InfixExpr@2..7
    IntLiteralExpr@2..3
      IntLiteral@2..3 "2"
    Caret@3..4 "^"
    InfixExpr@4..7
      IntLiteralExpr@4..5
        IntLiteral@4..5 "3"
      Caret@5..6 "^"
      IntLiteralExpr@6..7
        IntLiteral@6..7 "4""#]],
    );
}

#[test]
fn parse_infix_expression_with_mixed_binding_power() {
    check_expr(
        "1+2*3-4",
        expect![[r#"
InfixExpr@0..7
  InfixExpr@0..5
    IntLiteralExpr@0..1
      IntLiteral@0..1 "1"
    Plus@1..2 "+"
    InfixExpr@2..5
      IntLiteralExpr@2..3
        IntLiteral@2..3 "2"
      Star@3..4 "*"
      IntLiteralExpr@4..5
        IntLiteral@4..5 "3"
  Dash@5..6 "-"
  IntLiteralExpr@6..7
    IntLiteral@6..7 "4""#]],
    );
}

#[test]
fn remainder_same_as_multiply() {
    check_expr(
        "2*8%3",
        expect![[r#"
InfixExpr@0..5
  InfixExpr@0..3
    IntLiteralExpr@0..1
      IntLiteral@0..1 "2"
    Star@1..2 "*"
    IntLiteralExpr@2..3
      IntLiteral@2..3 "8"
  Percent@3..4 "%"
  IntLiteralExpr@4..5
    IntLiteral@4..5 "3""#]],
    )
}

#[test]
fn parse_infix_expression_with_emptyspace() {
    check_expr(
        " 1 +   2* 3 ",
        expect![[r#"
InfixExpr@0..12
  IntLiteralExpr@0..3
    Emptyspace@0..1 " "
    IntLiteral@1..2 "1"
    Emptyspace@2..3 " "
  Plus@3..4 "+"
  Emptyspace@4..7 "   "
  InfixExpr@7..12
    IntLiteralExpr@7..8
      IntLiteral@7..8 "2"
    Star@8..9 "*"
    Emptyspace@9..10 " "
    IntLiteralExpr@10..12
      IntLiteral@10..11 "3"
      Emptyspace@11..12 " ""#]],
    );
}

#[test]
fn negation_has_higher_binding_power_than_binary_operators() {
    check_expr(
        "-1+1",
        expect![[r#"
InfixExpr@0..4
  NegationExpr@0..2
    Dash@0..1 "-"
    IntLiteralExpr@1..2
      IntLiteral@1..2 "1"
  Plus@2..3 "+"
  IntLiteralExpr@3..4
    IntLiteral@3..4 "1""#]],
    );
}

#[test]
fn negation_following_binary_operator() {
    check_expr(
        "-1+-1",
        expect![[r#"
InfixExpr@0..5
  NegationExpr@0..2
    Dash@0..1 "-"
    IntLiteralExpr@1..2
      IntLiteral@1..2 "1"
  Plus@2..3 "+"
  NegationExpr@3..5
    Dash@3..4 "-"
    IntLiteralExpr@4..5
      IntLiteral@4..5 "1""#]],
    )
}

#[test]
fn logical_and() {
    check_expr(
        "true and false",
        expect![[r#"
InfixExpr@0..14
  PathExpr@0..5
    Ident@0..5
      Ident@0..4 "true"
      Emptyspace@4..5 " "
  AndKw@5..8 "and"
  Emptyspace@8..9 " "
  PathExpr@9..14
    Ident@9..14
      Ident@9..14 "false""#]],
    )
}

#[test]
fn logical_or() {
    check_expr(
        "true or false",
        expect![[r#"
InfixExpr@0..13
  PathExpr@0..5
    Ident@0..5
      Ident@0..4 "true"
      Emptyspace@4..5 " "
  OrKw@5..7 "or"
  Emptyspace@7..8 " "
  PathExpr@8..13
    Ident@8..13
      Ident@8..13 "false""#]],
    )
}

#[test]
fn parse_int_equality() {
    check_expr(
        "1 == 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  EqualsEquals@2..4 "=="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_int_not_equality() {
    check_expr(
        "1 != 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  BangEquals@2..4 "!="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_int_less_than() {
    check_expr(
        "1 < 1",
        expect![[r#"
InfixExpr@0..5
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  LAngle@2..3 "<"
  Emptyspace@3..4 " "
  IntLiteralExpr@4..5
    IntLiteral@4..5 "1""#]],
    );
}

#[test]
fn parse_int_less_than_or_equal() {
    check_expr(
        "1 <= 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  LAngleEquals@2..4 "<="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_int_greater_than() {
    check_expr(
        "1 > 1",
        expect![[r#"
InfixExpr@0..5
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  RAngle@2..3 ">"
  Emptyspace@3..4 " "
  IntLiteralExpr@4..5
    IntLiteral@4..5 "1""#]],
    );
}

#[test]
fn parse_int_greater_than_or_equal() {
    check_expr(
        "1 >= 1",
        expect![[r#"
InfixExpr@0..6
  IntLiteralExpr@0..2
    IntLiteral@0..1 "1"
    Emptyspace@1..2 " "
  RAngleEquals@2..4 ">="
  Emptyspace@4..5 " "
  IntLiteralExpr@5..6
    IntLiteral@5..6 "1""#]],
    );
}

#[test]
fn parse_string_equality() {
    check_expr(
        r#""a" == "a""#,
        expect![[r#"
InfixExpr@0..10
  StringLiteralExpr@0..4
    StringLiteralExpr@0..3 "\"a\""
    Emptyspace@3..4 " "
  EqualsEquals@4..6 "=="
  Emptyspace@6..7 " "
  StringLiteralExpr@7..10
    StringLiteralExpr@7..10 "\"a\"""#]],
    );
}

#[test]
fn parse_string_not_equality() {
    check_expr(
        r#""a" != "a""#,
        expect![[r#"
InfixExpr@0..10
  StringLiteralExpr@0..4
    StringLiteralExpr@0..3 "\"a\""
    Emptyspace@3..4 " "
  BangEquals@4..6 "!="
  Emptyspace@6..7 " "
  StringLiteralExpr@7..10
    StringLiteralExpr@7..10 "\"a\"""#]],
    );
}
