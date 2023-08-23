use expect_test::expect;

use crate::check_expr;

#[test]
fn parse_if_expression() {
    check_expr(
        "if a {}",
        expect![[r#"
            IfExpr@0..7
              If@0..2 "if"
              Emptyspace@2..3 " "
              ConditionExpr@3..5
                PathExpr@3..5
                  Ident@3..5
                    Ident@3..4 "a"
                    Emptyspace@4..5 " "
              ThenBranchExpr@5..7
                BlockExpr@5..7
                  LBrace@5..6 "{"
                  RBrace@6..7 "}""#]],
    )
}

#[test]
fn parse_if_else_expression() {
    check_expr(
        "if a {} else {}",
        expect![[r#"
            IfExpr@0..15
              If@0..2 "if"
              Emptyspace@2..3 " "
              ConditionExpr@3..5
                PathExpr@3..5
                  Ident@3..5
                    Ident@3..4 "a"
                    Emptyspace@4..5 " "
              ThenBranchExpr@5..8
                BlockExpr@5..8
                  LBrace@5..6 "{"
                  RBrace@6..7 "}"
                  Emptyspace@7..8 " "
              ElseBranchExpr@8..15
                Else@8..12 "else"
                Emptyspace@12..13 " "
                BlockExpr@13..15
                  LBrace@13..14 "{"
                  RBrace@14..15 "}""#]],
    )
}

#[test]
fn parse_if_else_if_expression() {
    check_expr(
        "if a {} else if b {} else {}",
        expect![[r#"
            IfExpr@0..28
              If@0..2 "if"
              Emptyspace@2..3 " "
              ConditionExpr@3..5
                PathExpr@3..5
                  Ident@3..5
                    Ident@3..4 "a"
                    Emptyspace@4..5 " "
              ThenBranchExpr@5..8
                BlockExpr@5..8
                  LBrace@5..6 "{"
                  RBrace@6..7 "}"
                  Emptyspace@7..8 " "
              ElseBranchExpr@8..28
                Else@8..12 "else"
                Emptyspace@12..13 " "
                IfExpr@13..28
                  If@13..15 "if"
                  Emptyspace@15..16 " "
                  ConditionExpr@16..18
                    PathExpr@16..18
                      Ident@16..18
                        Ident@16..17 "b"
                        Emptyspace@17..18 " "
                  ThenBranchExpr@18..21
                    BlockExpr@18..21
                      LBrace@18..19 "{"
                      RBrace@19..20 "}"
                      Emptyspace@20..21 " "
                  ElseBranchExpr@21..28
                    Else@21..25 "else"
                    Emptyspace@25..26 " "
                    BlockExpr@26..28
                      LBrace@26..27 "{"
                      RBrace@27..28 "}""#]],
    )
}
