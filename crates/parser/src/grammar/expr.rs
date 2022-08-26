mod function;
mod type_expr;

use lexer::TokenKind::*;

use crate::grammar::stmt::parse_stmt;
use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::SyntaxKind;

use self::function::parse_fun_expr;
use self::type_expr::parse_type_expr;

pub(super) fn parse_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

pub(super) fn parse_type(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(Ident));

    let m = p.start();
    parse_type_expr(p);
    m.complete(p, SyntaxKind::TypeExpr)
}

pub(super) fn parse_block(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(LBrace));
    let m = p.start();
    p.bump();

    while !p.at(RBrace) && !p.at_end() {
        parse_stmt(p);
    }

    p.expect(RBrace);

    m.complete(p, SyntaxKind::BlockExpr)
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(p)?;

    loop {
        let curr = p.peek()?;
        let op = match curr {
            Plus => BinaryOp::Add,
            Dash => BinaryOp::Sub,
            Star => BinaryOp::Mul,
            Slash => BinaryOp::Div,
            Percent => BinaryOp::Rem,
            Caret => BinaryOp::Exp,
            And => BinaryOp::And,
            Or => BinaryOp::Or,
            Dot => BinaryOp::Path,
            // TODO: can function expressions be parsed as a Binary expression?
            // Arrow => BinaryOp::Fun

            // Not at an operator, so is not a binary expression, so break having
            // just parsed the "lhs"
            _ => break,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Consume the operator token
        p.bump();

        // Starts a new marker that "wraps" the already parsed LHS
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
    let curr = p.peek();
    if curr.is_none() {
        p.error();
        return None;
    }
    let curr = curr.unwrap();

    let cm = match curr {
        IntLiteral => parse_int_literal(p),
        FloatLiteral => parse_float_literal(p),
        String => parse_string_literal(p),
        False => parse_bool_literal(p),
        True => parse_bool_literal(p),
        Ident => parse_name_ref(p),
        Dash => parse_negation_expr(p),
        Not => parse_not_expr(p),
        // TODO: this could be the start of function arguments
        // ex.
        // let add = (a: int, b: int) -> { a + b }
        //           ^
        // ex.
        // let print_hello = () -> { print "hello" }
        //                   ^
        // For now, require a "fun" keyword before the arg list.
        // Without "fun" the parsing requires lookahead until
        // the arrow. It may be implemented this way in the future.
        //
        // TODO: this might also be the start of a tuple?
        // ex.
        // let point = (1, 2)
        //             ^
        // parentheses are already overloaded as a grouping expression & parameters,
        // probably can't overload again as tuples
        LParen => parse_paren_expr(p),
        Fun => parse_fun_expr(p),
        LBrace => parse_block(p),
        Loop => parse_loop(p),
        _ => {
            p.error();
            return None;
        }
    };

    Some(cm)
}

fn parse_int_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(IntLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::IntExpr)
}

fn parse_float_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(FloatLiteral));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::FloatExpr)
}

fn parse_string_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(String));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringExpr)
}

fn parse_bool_literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(False) || p.at(True));

    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolExpr)
}

fn parse_name_ref(p: &mut Parser) -> CompletedMarker {
    // if p.at(Ident) {
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::NameRef)
    // } else {
    //     p.error_and_bump("expected identifier");
    // }
}

fn parse_negation_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(Dash));

    let m = p.start();

    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Consume the operator’s token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NegationExpr)
}

fn parse_not_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(Not));

    let m = p.start();

    let op = UnaryOp::Not;
    let ((), right_binding_power) = op.binding_power();

    // Consume the operator's token.
    p.bump();

    expr_binding_power(p, right_binding_power);

    m.complete(p, SyntaxKind::NotExpr)
}

fn parse_paren_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(LParen));

    let m = p.start();
    p.bump();
    expr_binding_power(p, 0);
    p.expect(RParen);

    m.complete(p, SyntaxKind::ParenExpr)
}

fn parse_loop(p: &mut Parser) -> CompletedMarker {
    assert!(p.at(Loop));
    let m = p.start();
    p.bump();

    parse_block(p);

    m.complete(p, SyntaxKind::LoopExpr)
}

enum BinaryOp {
    /// `+`
    Add,

    /// `-`
    Sub,

    /// `*`
    Mul,

    /// `/`
    Div,

    /// `%`
    Rem,

    /// `**`
    Exp,

    /// `and`
    And,

    /// `or`
    Or,

    /// `.`
    Path,
    // `->`
    // Fun,
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
            Self::Path => (11, 12),
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
    use crate::check_expr;
    use expect_test::expect;

    #[test]
    fn parse_int() {
        check_expr(
            "123",
            expect![[r#"
Root@0..3
  IntExpr@0..3
    IntLiteral@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_int_with_separators() {
        check_expr(
            "123_456_789",
            expect![[r#"
Root@0..11
  IntExpr@0..11
    IntLiteral@0..11 "123_456_789""#]],
        )
    }

    #[test]
    fn parse_int_preceded_by_emptyspace() {
        check_expr(
            "   9876",
            expect![[r#"
Root@0..7
  Emptyspace@0..3 "   "
  IntExpr@3..7
    IntLiteral@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_int_followed_by_emptyspace() {
        check_expr(
            "999   ",
            expect![[r#"
Root@0..6
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
Root@0..9
  Emptyspace@0..1 " "
  IntExpr@1..9
    IntLiteral@1..4 "123"
    Emptyspace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_string_literal() {
        check_expr(
            r#""hello""#,
            expect![[r#"
Root@0..7
  StringExpr@0..7
    StringExpr@0..7 "\"hello\"""#]],
        )
    }

    #[test]
    fn parse_variable_ref() {
        check_expr(
            "counter",
            expect![[r#"
Root@0..7
  NameRef@0..7
    Ident@0..7 "counter""#]],
        );
    }

    #[test]
    fn parse_simple_infix_expression() {
        check_expr(
            "1+2",
            expect![[r#"
Root@0..3
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
Root@0..7
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
Root@0..7
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
Root@0..7
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
Root@0..5
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
Root@0..12
  Emptyspace@0..1 " "
  InfixExpr@1..12
    IntExpr@1..3
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
    fn do_not_parse_operator_if_gettting_rhs_failed() {
        check_expr(
            "(1+",
            expect![[r#"
Root@0..3
  ParenExpr@0..3
    LParen@0..1 "("
    InfixExpr@1..3
      IntExpr@1..2
        IntLiteral@1..2 "1"
      Plus@2..3 "+"
error at 2..3: expected int, float, string, ‘false’, ‘true’, identifier, ‘-’, ‘not’, ‘(’, ‘fun’, ‘{’ or ‘loop’
error at 2..3: expected ‘)’"#]],
        );
    }

    #[test]
    fn parse_negation() {
        check_expr(
            "-1",
            expect![[r#"
Root@0..2
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
Root@0..4
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
Root@0..5
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
Root@0..14
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
Root@0..13
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
Root@0..8
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
Root@0..7
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
Root@0..6
  ParenExpr@0..6
    LParen@0..1 "("
    NameRef@1..6
      Ident@1..6 "hello"
error at 1..6: expected ‘)’"#]],
        );
    }

    #[test]
    fn parse_single_ident() {
        check_expr(
            "a",
            expect![[r#"
Root@0..1
  NameRef@0..1
    Ident@0..1 "a""#]],
        )
    }

    #[test]
    fn parse_one_path() {
        check_expr(
            "a.b",
            expect![[r#"
Root@0..3
  InfixExpr@0..3
    NameRef@0..1
      Ident@0..1 "a"
    Dot@1..2 "."
    NameRef@2..3
      Ident@2..3 "b""#]],
        )
    }

    #[test]
    fn parse_two_nested_path() {
        check_expr(
            "a.b.c",
            expect![[r#"
Root@0..5
  InfixExpr@0..5
    InfixExpr@0..3
      NameRef@0..1
        Ident@0..1 "a"
      Dot@1..2 "."
      NameRef@2..3
        Ident@2..3 "b"
    Dot@3..4 "."
    NameRef@4..5
      Ident@4..5 "c""#]],
        )
    }

    #[test]
    fn path_higher_precedence_than_arithmetic() {
        check_expr(
            "a.b * c",
            expect![[r#"
Root@0..7
  InfixExpr@0..7
    InfixExpr@0..4
      NameRef@0..1
        Ident@0..1 "a"
      Dot@1..2 "."
      NameRef@2..4
        Ident@2..3 "b"
        Emptyspace@3..4 " "
    Star@4..5 "*"
    Emptyspace@5..6 " "
    NameRef@6..7
      Ident@6..7 "c""#]],
        )
    }

    #[test]
    fn parse_block_with_one_expr() {
        check_expr(
            "{1}",
            expect![[r#"
Root@0..3
  BlockExpr@0..3
    LBrace@0..1 "{"
    ExprStmt@1..2
      IntExpr@1..2
        IntLiteral@1..2 "1"
    RBrace@2..3 "}""#]],
        )
    }

    #[test]
    fn parse_block_with_statements() {
        check_expr(
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
        IntLiteral@12..13 "1"
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
        IntLiteral@24..25 "2"
    Newline@25..26 "\n"
    Emptyspace@26..28 "  "
    ExprStmt@28..34
      InfixExpr@28..33
        NameRef@28..30
          Ident@28..29 "x"
          Emptyspace@29..30 " "
        Plus@30..31 "+"
        Emptyspace@31..32 " "
        NameRef@32..33
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
