use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_empty_loop() {
    check_expr(
        "loop {}",
        expect![[r#"
LoopExpr@0..7
  LoopKw@0..4 "loop"
  Emptyspace@4..5 " "
  BlockExpr@5..7
    LBrace@5..6 "{"
    RBrace@6..7 "}""#]],
    )
}

#[test]
fn parse_loop_break() {
    check_expr(
        "loop { break }",
        expect![[r#"
LoopExpr@0..14
  LoopKw@0..4 "loop"
  Emptyspace@4..5 " "
  BlockExpr@5..14
    LBrace@5..6 "{"
    Emptyspace@6..7 " "
    BreakStatement@7..13
      BreakKw@7..12 "break"
      Emptyspace@12..13 " "
    RBrace@13..14 "}""#]],
    )
}

#[test]
fn parse_loop_break_value() {
    check_expr(
        "loop { break foo }",
        expect![[r#"
LoopExpr@0..18
  LoopKw@0..4 "loop"
  Emptyspace@4..5 " "
  BlockExpr@5..18
    LBrace@5..6 "{"
    Emptyspace@6..7 " "
    BreakStatement@7..17
      BreakKw@7..12 "break"
      Emptyspace@12..13 " "
      PathExpr@13..17
        Ident@13..17
          Ident@13..16 "foo"
          Emptyspace@16..17 " "
    RBrace@17..18 "}""#]],
    )
}

#[test]
fn parse_loop_longer_example() {
    let input = "loop { 
  i = i * 2
  if i > 10 {
    break foo 
  }
}";
    check_expr(
        input,
        expect![[r#"
LoopExpr@0..54
  LoopKw@0..4 "loop"
  Emptyspace@4..5 " "
  BlockExpr@5..54
    LBrace@5..6 "{"
    Emptyspace@6..7 " "
    Newline@7..8 "\n"
    Emptyspace@8..10 "  "
    InfixExpr@10..19
      PathExpr@10..12
        Ident@10..12
          Ident@10..11 "i"
          Emptyspace@11..12 " "
      Equals@12..13 "="
      Emptyspace@13..14 " "
      InfixExpr@14..19
        PathExpr@14..16
          Ident@14..16
            Ident@14..15 "i"
            Emptyspace@15..16 " "
        Star@16..17 "*"
        Emptyspace@17..18 " "
        IntLiteralExpr@18..19
          IntLiteral@18..19 "2"
    Newline@19..20 "\n"
    Emptyspace@20..22 "  "
    IfExpr@22..52
      IfKw@22..24 "if"
      Emptyspace@24..25 " "
      ConditionExpr@25..32
        InfixExpr@25..32
          PathExpr@25..27
            Ident@25..27
              Ident@25..26 "i"
              Emptyspace@26..27 " "
          RAngle@27..28 ">"
          Emptyspace@28..29 " "
          IntLiteralExpr@29..32
            IntLiteral@29..31 "10"
            Emptyspace@31..32 " "
      ThenBranchExpr@32..52
        BlockExpr@32..52
          LBrace@32..33 "{"
          Newline@33..34 "\n"
          Emptyspace@34..38 "    "
          BreakStatement@38..48
            BreakKw@38..43 "break"
            Emptyspace@43..44 " "
            PathExpr@44..48
              Ident@44..48
                Ident@44..47 "foo"
                Emptyspace@47..48 " "
          Newline@48..49 "\n"
          Emptyspace@49..51 "  "
          RBrace@51..52 "}"
    Newline@52..53 "\n"
    RBrace@53..54 "}""#]],
    )
}

#[test]
fn parse_empty_for_loop() {
    check_expr(
        "for x in xs {}",
        expect![[r#"
            ForInLoop@0..14
              ForKw@0..3 "for"
              Emptyspace@3..4 " "
              Ident@4..6
                Ident@4..5 "x"
                Emptyspace@5..6 " "
              InKw@6..8 "in"
              Emptyspace@8..9 " "
              PathExpr@9..12
                Ident@9..12
                  Ident@9..11 "xs"
                  Emptyspace@11..12 " "
              BlockExpr@12..14
                LBrace@12..13 "{"
                RBrace@13..14 "}""#]],
    )
}
