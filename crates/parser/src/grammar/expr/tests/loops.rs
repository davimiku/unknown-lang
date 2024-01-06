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
