use logos::Logos;
use text_size::{TextRange, TextSize};

use crate::{Lexer, Token, TokenKind};

fn check(input: &str, kind: TokenKind) {
    let mut lexer = TokenKind::lexer(input);

    assert_eq!(lexer.next(), Some(kind));
    assert_eq!(lexer.slice(), input);
}

fn check_err(input: &str) {
    let mut lexer = TokenKind::lexer(input);

    assert_eq!(lexer.next(), Some(TokenKind::Error));
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, text: &'a str, range: TextRange) -> Self {
        Token { kind, text, range }
    }
}

fn range<S: Into<TextSize>>(start: S, end: S) -> TextRange {
    TextRange::new(start.into(), end.into())
}

#[test]
fn test_basic_operators() {
    let expected = vec![
        Token::new(TokenKind::Plus, "+", range(0, 1)),
        Token::new(TokenKind::Dash, "-", range(1, 2)),
        Token::new(TokenKind::Star, "*", range(2, 3)),
        Token::new(TokenKind::Slash, "/", range(3, 4)),
    ];
    let lexer = Lexer::new("+-*/");

    let actual: Vec<Token> = lexer.collect();

    assert_eq!(actual, expected);
}

#[test]
fn lex_alphabetic_identifier() {
    check("abcd", TokenKind::Ident);
}

#[test]
fn lex_alphanumeric_identifier() {
    check("ab123cde456", TokenKind::Ident);
}

#[test]
fn lex_mixed_case_identifier() {
    check("ABCdef", TokenKind::Ident);
}

#[test]
fn lex_snake_case_identifier() {
    check("abc_def", TokenKind::Ident);
}

#[test]
fn lex_invalid_identifier_start_with_underscore() {
    check("_abc", TokenKind::InvalidLeadingUnderscore);
}

#[test]
fn lex_integer() {
    check("123456", TokenKind::IntLiteral);
}

#[test]
fn lex_integer_with_separators() {
    check("123_456_789", TokenKind::IntLiteral);
}

#[test]
fn lex_float() {
    check("1.23", TokenKind::FloatLiteral);
}

#[test]
fn lex_float_with_separators() {
    check("123_456.789", TokenKind::FloatLiteral);
}

#[test]
fn lex_bang() {
    check("!", TokenKind::Bang);
}

#[test]
fn lex_emptyspace() {
    check("   ", TokenKind::Emptyspace);
}

#[test]
fn lex_and_keyword() {
    check("and", TokenKind::And);
}

#[test]
fn lex_else_keyword() {
    check("else", TokenKind::Else);
}

#[test]
fn lex_for_keyword() {
    check("for", TokenKind::For);
}

#[test]
fn lex_if_keyword() {
    check("if", TokenKind::If);
}

#[test]
fn lex_in_keyword() {
    check("in", TokenKind::In);
}

#[test]
fn lex_let_keyword() {
    check("let", TokenKind::Let);
}

#[test]
fn lex_or_keyword() {
    check("or", TokenKind::Or);
}

#[test]
fn lex_return_keyword() {
    check("return", TokenKind::Return);
}

#[test]
fn lex_try_keyword() {
    check("try", TokenKind::Try);
}

#[test]
fn lex_while_keyword() {
    check("while", TokenKind::While);
}

#[test]
fn lex_plus() {
    check("+", TokenKind::Plus);
}

#[test]
fn lex_plusplus() {
    check("++", TokenKind::PlusPlus);
}

#[test]
fn lex_minus() {
    check("-", TokenKind::Dash);
}

#[test]
fn lex_star() {
    check("*", TokenKind::Star);
}

#[test]
fn lex_caret() {
    check("^", TokenKind::Caret);
}

#[test]
fn lex_left_angle() {
    check("<", TokenKind::LAngle);
}

#[test]
fn lex_left_angle_equals() {
    check("<=", TokenKind::LAngleEquals);
}

#[test]
fn lex_right_angle() {
    check(">", TokenKind::RAngle);
}

#[test]
fn lex_right_angle_equals() {
    check(">=", TokenKind::RAngleEquals);
}

#[test]
fn lex_slash() {
    check("/", TokenKind::Slash);
}

#[test]
fn lex_equals_equals() {
    check("==", TokenKind::EqualsEquals);
}

#[test]
fn lex_dotdot() {
    check("..", TokenKind::DotDot);
}

#[test]
fn lex_dot() {
    check(".", TokenKind::Dot);
}

#[test]
fn lex_comment() {
    check("/* test */", TokenKind::Comment);
}

#[test]
fn lex_empty_comment() {
    check("/**/", TokenKind::Comment);
}

#[test]
fn lex_star_comment() {
    check("/***/", TokenKind::Comment);
}
