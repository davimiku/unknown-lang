use crate::check_expr;
use expect_test::expect;

#[test]
fn parse_nullary_function() {
    check_expr(
        "() -> { }",
        expect![[r#"
InfixExpr@0..9
  ParenExpr@0..3
    LParen@0..1 "("
    RParen@1..2 ")"
    Emptyspace@2..3 " "
  Arrow@3..5 "->"
  Emptyspace@5..6 " "
  BlockExpr@6..9
    LBrace@6..7 "{"
    Emptyspace@7..8 " "
    RBrace@8..9 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_explicit_param_type() {
    check_expr(
        "(a: A) -> { }",
        expect![[r#"
InfixExpr@0..13
  ParenExpr@0..7
    LParen@0..1 "("
    ParenExprItem@1..5
      Call@1..2
        Path@1..2
          Ident@1..2
            Ident@1..2 "a"
      Colon@2..3 ":"
      Emptyspace@3..4 " "
      TypeExpr@4..5
        Path@4..5
          Ident@4..5
            Ident@4..5 "A"
    RParen@5..6 ")"
    Emptyspace@6..7 " "
  Arrow@7..9 "->"
  Emptyspace@9..10 " "
  BlockExpr@10..13
    LBrace@10..11 "{"
    Emptyspace@11..12 " "
    RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_inferred_param_type() {
    check_expr(
        "id -> { }",
        expect![[r#"
InfixExpr@0..9
  Call@0..3
    Path@0..3
      Ident@0..3
        Ident@0..2 "id"
        Emptyspace@2..3 " "
  Arrow@3..5 "->"
  Emptyspace@5..6 " "
  BlockExpr@6..9
    LBrace@6..7 "{"
    Emptyspace@7..8 " "
    RBrace@8..9 "}""#]],
    )
}

#[test]
fn parse_unary_function_with_paren() {
    check_expr(
        "(id) -> {}",
        expect![[r#"
InfixExpr@0..10
  ParenExpr@0..5
    LParen@0..1 "("
    Call@1..3
      Path@1..3
        Ident@1..3
          Ident@1..3 "id"
    RParen@3..4 ")"
    Emptyspace@4..5 " "
  Arrow@5..7 "->"
  Emptyspace@7..8 " "
  BlockExpr@8..10
    LBrace@8..9 "{"
    RBrace@9..10 "}""#]],
    );
}

#[test]
fn parse_binary_function_with_inferred_param_types() {
    check_expr(
        "(a, b) -> { }",
        expect![[r#"
InfixExpr@0..13
  ParenExpr@0..7
    LParen@0..1 "("
    ParenExprItem@1..2
      Call@1..2
        Path@1..2
          Ident@1..2
            Ident@1..2 "a"
    Comma@2..3 ","
    Emptyspace@3..4 " "
    ParenExprItem@4..5
      Call@4..5
        Path@4..5
          Ident@4..5
            Ident@4..5 "b"
    RParen@5..6 ")"
    Emptyspace@6..7 " "
  Arrow@7..9 "->"
  Emptyspace@9..10 " "
  BlockExpr@10..13
    LBrace@10..11 "{"
    Emptyspace@11..12 " "
    RBrace@12..13 "}""#]],
    )
}

#[test]
fn parse_binary_function_with_explicit_param_types() {
    check_expr(
        "(a: A, b: B) -> { }",
        expect![[r#"
InfixExpr@0..19
  ParenExpr@0..13
    LParen@0..1 "("
    ParenExprItem@1..5
      Call@1..2
        Path@1..2
          Ident@1..2
            Ident@1..2 "a"
      Colon@2..3 ":"
      Emptyspace@3..4 " "
      TypeExpr@4..5
        Path@4..5
          Ident@4..5
            Ident@4..5 "A"
    Comma@5..6 ","
    Emptyspace@6..7 " "
    ParenExprItem@7..11
      Call@7..8
        Path@7..8
          Ident@7..8
            Ident@7..8 "b"
      Colon@8..9 ":"
      Emptyspace@9..10 " "
      TypeExpr@10..11
        Path@10..11
          Ident@10..11
            Ident@10..11 "B"
    RParen@11..12 ")"
    Emptyspace@12..13 " "
  Arrow@13..15 "->"
  Emptyspace@15..16 " "
  BlockExpr@16..19
    LBrace@16..17 "{"
    Emptyspace@17..18 " "
    RBrace@18..19 "}""#]],
    )
}
