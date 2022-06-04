use lexer::TokenKind;

use crate::grammar::stmt::parse_stmt;
use crate::parser::{marker::CompletedMarker, Parser};
use crate::SyntaxKind;

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        let op = if p.at(TokenKind::Plus) {
            BinaryOp::Add
        } else if p.at(TokenKind::Dash) {
            BinaryOp::Sub
        } else if p.at(TokenKind::Star) {
            BinaryOp::Mul
        } else if p.at(TokenKind::Slash) {
            BinaryOp::Div
        } else if p.at(TokenKind::Percent) {
            BinaryOp::Rem
        } else if p.at(TokenKind::Caret) {
            BinaryOp::Exp
        } else if p.at(TokenKind::And) {
            BinaryOp::And
        } else if p.at(TokenKind::Or) {
            BinaryOp::Or
        } else {
            // Not at an operator, let the caller decide what to do next
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Consume the operator token
        p.bump();

        let m = lhs.precede(p);
        let parsed_rhs = expr_binding_power(p, right_binding_power).is_some();
        lhs = m.complete(p, SyntaxKind::InfixExpr);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn parse_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = if p.at(TokenKind::Int) {
        parse_int_literal(p)
    } else if p.at(TokenKind::String) {
        parse_string_literal(p)
    } else if p.at(TokenKind::False) || p.at(TokenKind::True) {
        parse_bool_literal(p)
    } else if p.at(TokenKind::Ident) {
        parse_variable_ref(p)
    } else if p.at(TokenKind::Dash) {
        parse_negation_expr(p)
    } else if p.at(TokenKind::Not) {
        parse_not_expr(p)
    } else if p.at(TokenKind::LParen) {
        parse_paren_expr(p)
    } else if p.at(TokenKind::LBrace) {
        parse_block(p)
    } else if p.at(TokenKind::Loop) {
        parse_loop(p)
    } else {
        p.error();
        return None;
    };

    Some(cm)
}

fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Int));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntExpr)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::String));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringExpr)
}

fn parse_bool_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::False) || p.at(TokenKind::True));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolExpr)
}

fn parse_variable_ref(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Ident));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::VariableRef)
}

fn parse_negation_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Dash));

    let m = p.start();

    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Eat the operator’s token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Not));

    let m = p.start();

    let op = UnaryOp::Not;
    let ((), right_binding_power) = op.binding_power();

    // Eat the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NotExpr)
}

fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LParen));

    let m = p.start();
    p.bump();
    expr_binding_power(p, 0);
    p.expect(TokenKind::RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

pub(super) fn parse_block(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::LBrace));
    let m = p.start();
    p.bump();

    while !p.at(TokenKind::RBrace) && !p.at_end() {
        parse_stmt(p);
    }

    p.expect(TokenKind::RBrace);

    m.complete(p, SyntaxKind::BlockExpr)
}

fn parse_loop(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(TokenKind::Loop));
    let m = p.start();
    p.bump();

    parse_block(p);

    m.complete(p, SyntaxKind::LoopExpr)
}

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,

    And,
    Or,
}

impl BinaryOp {
    /// Binding power tuple of (left, right)
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Or => (1, 2),
            Self::And => (3, 4),
            Self::Add | Self::Sub => (5, 6),
            Self::Mul | Self::Div | Self::Rem => (7, 8),
            Self::Exp => (10, 9),
        }
    }
}

enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 11),
            Self::Not => ((), 5),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_int() {
        check(
            "123",
            expect![[r#"
Root@0..3
  IntExpr@0..3
    Int@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_int_preceded_by_emptyspace() {
        check(
            "   9876",
            expect![[r#"
Root@0..7
  Emptyspace@0..3 "   "
  IntExpr@3..7
    Int@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_int_followed_by_emptyspace() {
        check(
            "999   ",
            expect![[r#"
Root@0..6
  IntExpr@0..6
    Int@0..3 "999"
    Emptyspace@3..6 "   ""#]],
        );
    }

    #[test]
    fn parse_int_surrounded_by_emptyspace() {
        check(
            " 123     ",
            expect![[r#"
Root@0..9
  Emptyspace@0..1 " "
  IntExpr@1..9
    Int@1..4 "123"
    Emptyspace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_string_literal() {
        check(
            r#""hello""#,
            expect![[r#"
Root@0..7
  StringExpr@0..7
    StringExpr@0..7 "\"hello\"""#]],
        )
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
Root@0..7
  VariableRef@0..7
    Ident@0..7 "counter""#]],
        );
    }

    #[test]
    fn parse_simple_infix_expression() {
        check(
            "1+2",
            expect![[r#"
Root@0..3
  InfixExpr@0..3
    IntExpr@0..1
      Int@0..1 "1"
    Plus@1..2 "+"
    IntExpr@2..3
      Int@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_infix_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
Root@0..7
  InfixExpr@0..7
    InfixExpr@0..5
      InfixExpr@0..3
        IntExpr@0..1
          Int@0..1 "1"
        Plus@1..2 "+"
        IntExpr@2..3
          Int@2..3 "2"
      Plus@3..4 "+"
      IntExpr@4..5
        Int@4..5 "3"
    Plus@5..6 "+"
    IntExpr@6..7
      Int@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_right_associative_infix_expression() {
        check(
            "1^2^3^4",
            expect![[r#"
Root@0..7
  InfixExpr@0..7
    IntExpr@0..1
      Int@0..1 "1"
    Caret@1..2 "^"
    InfixExpr@2..7
      IntExpr@2..3
        Int@2..3 "2"
      Caret@3..4 "^"
      InfixExpr@4..7
        IntExpr@4..5
          Int@4..5 "3"
        Caret@5..6 "^"
        IntExpr@6..7
          Int@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_infix_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
Root@0..7
  InfixExpr@0..7
    InfixExpr@0..5
      IntExpr@0..1
        Int@0..1 "1"
      Plus@1..2 "+"
      InfixExpr@2..5
        IntExpr@2..3
          Int@2..3 "2"
        Star@3..4 "*"
        IntExpr@4..5
          Int@4..5 "3"
    Dash@5..6 "-"
    IntExpr@6..7
      Int@6..7 "4""#]],
        );
    }

    #[test]
    fn remainder_same_as_multiply() {
        check(
            "2*8%3",
            expect![[r#"
Root@0..5
  InfixExpr@0..5
    InfixExpr@0..3
      IntExpr@0..1
        Int@0..1 "2"
      Star@1..2 "*"
      IntExpr@2..3
        Int@2..3 "8"
    Percent@3..4 "%"
    IntExpr@4..5
      Int@4..5 "3""#]],
        )
    }

    #[test]
    fn parse_infix_expression_with_emptyspace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
Root@0..12
  Emptyspace@0..1 " "
  InfixExpr@1..12
    IntExpr@1..3
      Int@1..2 "1"
      Emptyspace@2..3 " "
    Plus@3..4 "+"
    Emptyspace@4..7 "   "
    InfixExpr@7..12
      IntExpr@7..8
        Int@7..8 "2"
      Star@8..9 "*"
      Emptyspace@9..10 " "
      IntExpr@10..12
        Int@10..11 "3"
        Emptyspace@11..12 " ""#]],
        );
    }

    #[test]
    fn do_not_parse_operator_if_gettting_rhs_failed() {
        check(
            "(1+",
            expect![[r#"
Root@0..3
  ParenExpr@0..3
    LParen@0..1 "("
    InfixExpr@1..3
      IntExpr@1..2
        Int@1..2 "1"
      Plus@2..3 "+"
error at 2..3: expected int, string, ‘false’, ‘true’, identifier, ‘-’, ‘not’, ‘(’, ‘{’ or ‘loop’
error at 2..3: expected ‘)’"#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-1",
            expect![[r#"
Root@0..2
  NegationExpr@0..2
    Dash@0..1 "-"
    IntExpr@1..2
      Int@1..2 "1""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_binary_operators() {
        check(
            "-1+1",
            expect![[r#"
Root@0..4
  InfixExpr@0..4
    NegationExpr@0..2
      Dash@0..1 "-"
      IntExpr@1..2
        Int@1..2 "1"
    Plus@2..3 "+"
    IntExpr@3..4
      Int@3..4 "1""#]],
        );
    }

    #[test]
    fn negation_following_binary_operator() {
        check(
            "-1+-1",
            expect![[r#"
Root@0..5
  InfixExpr@0..5
    NegationExpr@0..2
      Dash@0..1 "-"
      IntExpr@1..2
        Int@1..2 "1"
    Plus@2..3 "+"
    NegationExpr@3..5
      Dash@3..4 "-"
      IntExpr@4..5
        Int@4..5 "1""#]],
        )
    }

    #[test]
    fn logical_and() {
        check(
            "true and false",
            expect![[r#"
Root@0..14
  InfixExpr@0..14
    BoolExpr@0..5
      True@0..4 "true"
      Emptyspace@4..5 " "
    And@5..8 "and"
    Emptyspace@8..9 " "
    BoolExpr@9..14
      False@9..14 "false""#]],
        )
    }

    #[test]
    fn logical_or() {
        check(
            "true or false",
            expect![[r#"
Root@0..13
  InfixExpr@0..13
    BoolExpr@0..5
      True@0..4 "true"
      Emptyspace@4..5 " "
    Or@5..7 "or"
    Emptyspace@7..8 " "
    BoolExpr@8..13
      False@8..13 "false""#]],
        )
    }

    #[test]
    fn logical_not() {
        check(
            "not true",
            expect![[r#"
Root@0..8
  NotExpr@0..8
    Not@0..3 "not"
    Emptyspace@3..4 " "
    BoolExpr@4..8
      True@4..8 "true""#]],
        )
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((1))))))",
            expect![[r#"
Root@0..13
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
                Int@6..7 "1"
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
        check(
            "3*(2+1)",
            expect![[r#"
Root@0..7
  InfixExpr@0..7
    IntExpr@0..1
      Int@0..1 "3"
    Star@1..2 "*"
    ParenExpr@2..7
      LParen@2..3 "("
      InfixExpr@3..6
        IntExpr@3..4
          Int@3..4 "2"
        Plus@4..5 "+"
        IntExpr@5..6
          Int@5..6 "1"
      RParen@6..7 ")""#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(hello",
            expect![[r#"
Root@0..6
  ParenExpr@0..6
    LParen@0..1 "("
    VariableRef@1..6
      Ident@1..6 "hello"
error at 1..6: expected ‘+’, ‘-’, ‘*’, ‘/’, ‘%’, ‘^’, ‘and’, ‘or’ or ‘)’"#]],
        );
    }

    #[test]
    fn parse_block_with_one_expr() {
        check(
            "{1}",
            expect![[r#"
Root@0..3
  BlockExpr@0..3
    LBrace@0..1 "{"
    IntExpr@1..2
      Int@1..2 "1"
    RBrace@2..3 "}""#]],
        )
    }

    #[test]
    fn parse_block_with_statements() {
        check(
            r#"{
  let x = 1
  let y = 2
  x + y
}"#,
            expect![[r#"
Root@0..35
  BlockExpr@0..35
    LBrace@0..1 "{"
    Newline@1..2 "\n"
    Emptyspace@2..4 "  "
    VariableDef@4..13
      Let@4..7 "let"
      Emptyspace@7..8 " "
      Ident@8..9 "x"
      Emptyspace@9..10 " "
      Equals@10..11 "="
      Emptyspace@11..12 " "
      IntExpr@12..13
        Int@12..13 "1"
    Newline@13..14 "\n"
    Emptyspace@14..16 "  "
    VariableDef@16..25
      Let@16..19 "let"
      Emptyspace@19..20 " "
      Ident@20..21 "y"
      Emptyspace@21..22 " "
      Equals@22..23 "="
      Emptyspace@23..24 " "
      IntExpr@24..25
        Int@24..25 "2"
    Newline@25..26 "\n"
    Emptyspace@26..28 "  "
    InfixExpr@28..33
      VariableRef@28..30
        Ident@28..29 "x"
        Emptyspace@29..30 " "
      Plus@30..31 "+"
      Emptyspace@31..32 " "
      VariableRef@32..33
        Ident@32..33 "y"
    Newline@33..34 "\n"
    RBrace@34..35 "}""#]],
        )
    }

    #[test]
    fn parse_empty_loop() {
        check(
            "loop {}",
            expect![[r#"
Root@0..7
  LoopExpr@0..7
    Loop@0..4 "loop"
    Emptyspace@4..5 " "
    BlockExpr@5..7
      LBrace@5..6 "{"
      RBrace@6..7 "}""#]],
        )
    }
}
