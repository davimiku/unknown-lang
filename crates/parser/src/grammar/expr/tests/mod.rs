mod functions;
mod literals;

use crate::check_expr;
use expect_test::expect;

#[test]
fn parse_variable_ref() {
    check_expr(
        "a",
        expect![[r#"
Call@0..1
  Path@0..1
    Ident@0..1
      Ident@0..1 "a""#]],
    );
}

#[test]
fn parse_simple_infix_expression() {
    check_expr(
        "1+2",
        expect![[r#"
InfixExpr@0..3
  IntExpr@0..1
    IntLiteral@0..1 "1"
  Plus@1..2 "+"
  IntExpr@2..3
    IntLiteral@2..3 "2""#]],
    );
}

#[test]
fn parse_left_associative_infix_expression() {
    check_expr(
        "1+2+3+4",
        expect![[r#"
InfixExpr@0..7
  InfixExpr@0..5
    InfixExpr@0..3
      IntExpr@0..1
        IntLiteral@0..1 "1"
      Plus@1..2 "+"
      IntExpr@2..3
        IntLiteral@2..3 "2"
    Plus@3..4 "+"
    IntExpr@4..5
      IntLiteral@4..5 "3"
  Plus@5..6 "+"
  IntExpr@6..7
    IntLiteral@6..7 "4""#]],
    );
}

#[test]
fn parse_right_associative_infix_expression() {
    check_expr(
        "1^2^3^4",
        expect![[r#"
InfixExpr@0..7
  IntExpr@0..1
    IntLiteral@0..1 "1"
  Caret@1..2 "^"
  InfixExpr@2..7
    IntExpr@2..3
      IntLiteral@2..3 "2"
    Caret@3..4 "^"
    InfixExpr@4..7
      IntExpr@4..5
        IntLiteral@4..5 "3"
      Caret@5..6 "^"
      IntExpr@6..7
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
    IntExpr@0..1
      IntLiteral@0..1 "1"
    Plus@1..2 "+"
    InfixExpr@2..5
      IntExpr@2..3
        IntLiteral@2..3 "2"
      Star@3..4 "*"
      IntExpr@4..5
        IntLiteral@4..5 "3"
  Dash@5..6 "-"
  IntExpr@6..7
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
    IntExpr@0..1
      IntLiteral@0..1 "2"
    Star@1..2 "*"
    IntExpr@2..3
      IntLiteral@2..3 "8"
  Percent@3..4 "%"
  IntExpr@4..5
    IntLiteral@4..5 "3""#]],
    )
}

#[test]
fn parse_infix_expression_with_emptyspace() {
    check_expr(
        " 1 +   2* 3 ",
        expect![[r#"
InfixExpr@0..12
  IntExpr@0..3
    Emptyspace@0..1 " "
    IntLiteral@1..2 "1"
    Emptyspace@2..3 " "
  Plus@3..4 "+"
  Emptyspace@4..7 "   "
  InfixExpr@7..12
    IntExpr@7..8
      IntLiteral@7..8 "2"
    Star@8..9 "*"
    Emptyspace@9..10 " "
    IntExpr@10..12
      IntLiteral@10..11 "3"
      Emptyspace@11..12 " ""#]],
    );
}

#[test]
// ???
fn do_not_parse_operator_if_getting_rhs_failed() {
    check_expr(
        "(1+",
        expect![[r#"
ParenExpr@0..3
  LParen@0..1 "("
  InfixExpr@1..3
    IntExpr@1..2
      IntLiteral@1..2 "1"
    Plus@2..3 "+"
error at 2..3: expected 
error at 2..3: expected ‘:’, ‘,’ or ‘)’"#]],
    );
}

#[test]
fn parse_negation() {
    check_expr(
        "-1",
        expect![[r#"
NegationExpr@0..2
  Dash@0..1 "-"
  IntExpr@1..2
    IntLiteral@1..2 "1""#]],
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
    IntExpr@1..2
      IntLiteral@1..2 "1"
  Plus@2..3 "+"
  IntExpr@3..4
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
    IntExpr@1..2
      IntLiteral@1..2 "1"
  Plus@2..3 "+"
  NegationExpr@3..5
    Dash@3..4 "-"
    IntExpr@4..5
      IntLiteral@4..5 "1""#]],
    )
}

#[test]
fn logical_and() {
    check_expr(
        "true and false",
        expect![[r#"
InfixExpr@0..14
  BoolExpr@0..5
    TrueLiteral@0..4 "true"
    Emptyspace@4..5 " "
  And@5..8 "and"
  Emptyspace@8..9 " "
  BoolExpr@9..14
    FalseLiteral@9..14 "false""#]],
    )
}

#[test]
fn logical_or() {
    check_expr(
        "true or false",
        expect![[r#"
InfixExpr@0..13
  BoolExpr@0..5
    TrueLiteral@0..4 "true"
    Emptyspace@4..5 " "
  Or@5..7 "or"
  Emptyspace@7..8 " "
  BoolExpr@8..13
    FalseLiteral@8..13 "false""#]],
    )
}

#[test]
fn logical_not() {
    check_expr(
        "not true",
        expect![[r#"
NotExpr@0..8
  Not@0..3 "not"
  Emptyspace@3..4 " "
  BoolExpr@4..8
    TrueLiteral@4..8 "true""#]],
    )
}

#[test]
fn parse_nested_parentheses() {
    check_expr(
        "((((((1))))))",
        expect![[r#"
ParenExpr@0..13
  LParen@0..1 "("
  ParenExpr@1..12
    LParen@1..2 "("
    ParenExpr@2..11
      LParen@2..3 "("
      ParenExpr@3..10
        LParen@3..4 "("
        ParenExpr@4..9
          LParen@4..5 "("
          ParenExpr@5..8
            LParen@5..6 "("
            IntExpr@6..7
              IntLiteral@6..7 "1"
            RParen@7..8 ")"
          RParen@8..9 ")"
        RParen@9..10 ")"
      RParen@10..11 ")"
    RParen@11..12 ")"
  RParen@12..13 ")""#]],
    );
}

#[test]
fn parentheses_affect_precedence() {
    check_expr(
        "3*(2+1)",
        expect![[r#"
InfixExpr@0..7
  IntExpr@0..1
    IntLiteral@0..1 "3"
  Star@1..2 "*"
  ParenExpr@2..7
    LParen@2..3 "("
    InfixExpr@3..6
      IntExpr@3..4
        IntLiteral@3..4 "2"
      Plus@4..5 "+"
      IntExpr@5..6
        IntLiteral@5..6 "1"
    RParen@6..7 ")""#]],
    );
}

#[test]
fn parse_unclosed_parentheses() {
    check_expr(
        "(hello",
        expect![[r#"
ParenExpr@0..6
  LParen@0..1 "("
  Call@1..6
    Path@1..6
      Ident@1..6
        Ident@1..6 "hello"
error at 1..6: expected ‘.’, ‘:’, ‘,’ or ‘)’"#]],
    );
}

#[test]
fn parse_single_ident() {
    check_expr(
        "a",
        expect![[r#"
Call@0..1
  Path@0..1
    Ident@0..1
      Ident@0..1 "a""#]],
    )
}

#[test]
fn parse_one_path() {
    check_expr(
        "a.b",
        expect![[r#"
Call@0..3
  Path@0..3
    Ident@0..1
      Ident@0..1 "a"
    Dot@1..2 "."
    Ident@2..3
      Ident@2..3 "b""#]],
    )
}

#[test]
fn parse_two_nested_path() {
    check_expr(
        "a.b.c",
        expect![[r#"
Call@0..5
  Path@0..5
    Ident@0..1
      Ident@0..1 "a"
    Dot@1..2 "."
    Ident@2..3
      Ident@2..3 "b"
    Dot@3..4 "."
    Ident@4..5
      Ident@4..5 "c""#]],
    )
}

#[test]
fn parse_path_higher_precedence_than_arithmetic() {
    check_expr(
        "a.b * c",
        expect![[r#"
InfixExpr@0..7
  Call@0..4
    Path@0..4
      Ident@0..1
        Ident@0..1 "a"
      Dot@1..2 "."
      Ident@2..4
        Ident@2..3 "b"
        Emptyspace@3..4 " "
  Star@4..5 "*"
  Emptyspace@5..6 " "
  Call@6..7
    Path@6..7
      Ident@6..7
        Ident@6..7 "c""#]],
    )
}

#[test]
fn parse_function_call_no_args() {
    check_expr(
        "func ()",
        expect![[r#"
Call@0..7
  Path@0..5
    Ident@0..5
      Ident@0..4 "func"
      Emptyspace@4..5 " "
  CallArgs@5..7
    LParen@5..6 "("
    RParen@6..7 ")""#]],
    )
}

#[test]
fn parse_function_call_one_int() {
    check_expr(
        "print 1",
        expect![[r#"
Call@0..7
  Path@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..7
    IntExpr@6..7
      IntLiteral@6..7 "1""#]],
    )
}

#[test]
fn parse_function_call_one_path() {
    check_expr(
        "print a",
        expect![[r#"
Call@0..7
  Path@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..7
    Call@6..7
      Path@6..7
        Ident@6..7
          Ident@6..7 "a""#]],
    )
}

#[test]
fn parse_function_call_nested_path() {
    check_expr(
        "print a.b.c",
        expect![[r#"
Call@0..11
  Path@0..6
    Ident@0..6
      Ident@0..5 "print"
      Emptyspace@5..6 " "
  CallArgs@6..11
    Call@6..11
      Path@6..11
        Ident@6..7
          Ident@6..7 "a"
        Dot@7..8 "."
        Ident@8..9
          Ident@8..9 "b"
        Dot@9..10 "."
        Ident@10..11
          Ident@10..11 "c""#]],
    )
}

#[test]
fn parse_function_call_two_args() {
    check_expr(
        "add (1, 2)",
        expect![[r#"
Call@0..10
  Path@0..4
    Ident@0..4
      Ident@0..3 "add"
      Emptyspace@3..4 " "
  CallArgs@4..10
    LParen@4..5 "("
    IntExpr@5..6
      IntLiteral@5..6 "1"
    Comma@6..7 ","
    Emptyspace@7..8 " "
    IntExpr@8..9
      IntLiteral@8..9 "2"
    RParen@9..10 ")""#]],
    )
}

#[test]
fn parse_block_with_one_expr() {
    check_expr(
        "{1}",
        expect![[r#"
BlockExpr@0..3
  LBrace@0..1 "{"
  IntExpr@1..2
    IntLiteral@1..2 "1"
  RBrace@2..3 "}""#]],
    )
}

#[test]
fn parse_block_newline() {
    check_expr(
        r#"{
  1
}"#,
        expect![[r#"
BlockExpr@0..7
  LBrace@0..1 "{"
  Newline@1..2 "\n"
  Emptyspace@2..4 "  "
  IntExpr@4..5
    IntLiteral@4..5 "1"
  Newline@5..6 "\n"
  RBrace@6..7 "}""#]],
    )
}

#[test]
// FIXME: parsing newline should be OK here
fn parse_block_expressions_multiple_lines() {
    check_expr(
        r#"{
  let x = 1
  let y = 2
  x + y
}"#,
        expect![[r#"
BlockExpr@0..35
  LBrace@0..1 "{"
  Newline@1..2 "\n"
  Emptyspace@2..4 "  "
  LetBinding@4..13
    Let@4..7 "let"
    Emptyspace@7..8 " "
    Ident@8..10
      Ident@8..9 "x"
      Emptyspace@9..10 " "
    Equals@10..11 "="
    Emptyspace@11..12 " "
    IntExpr@12..13
      IntLiteral@12..13 "1"
  Newline@13..14 "\n"
  Emptyspace@14..16 "  "
  LetBinding@16..25
    Let@16..19 "let"
    Emptyspace@19..20 " "
    Ident@20..22
      Ident@20..21 "y"
      Emptyspace@21..22 " "
    Equals@22..23 "="
    Emptyspace@23..24 " "
    IntExpr@24..25
      IntLiteral@24..25 "2"
  Newline@25..26 "\n"
  Emptyspace@26..28 "  "
  InfixExpr@28..33
    Call@28..30
      Path@28..30
        Ident@28..30
          Ident@28..29 "x"
          Emptyspace@29..30 " "
    Plus@30..31 "+"
    Emptyspace@31..32 " "
    Call@32..33
      Path@32..33
        Ident@32..33
          Ident@32..33 "y"
  Newline@33..34 "\n"
  RBrace@34..35 "}""#]],
    )
}

#[test]
fn parse_empty_loop() {
    check_expr(
        "loop {}",
        expect![[r#"
LoopExpr@0..7
  Loop@0..4 "loop"
  Emptyspace@4..5 " "
  BlockExpr@5..7
    LBrace@5..6 "{"
    RBrace@6..7 "}""#]],
    )
}
