use std::fmt;

use logos::Logos;

#[derive(Logos, PartialEq, Debug, Clone, Copy)]
pub enum TokenKind {
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Ident,

    // TODO: implement better numbers
    // https://github.com/maciejhirsz/logos/issues/133#issuecomment-687281059
    #[regex("[0-9]+")]
    Int,

    #[regex("\"[^\"\n]*\"")]
    String,

    // ==========
    // Keywords
    // ==========
    #[token("and")]
    And,

    #[token("else")]
    Else,

    #[token("for")]
    For,

    #[token("if")]
    If,

    #[token("let")]
    Let,

    #[token("loop")]
    Loop,

    #[token("module")]
    Module,

    #[token("mutable")]
    Mutable,

    #[token("not")]
    Not,

    #[token("or")]
    Or,

    #[token("return")]
    Return,

    #[token("struct")]
    Struct,

    #[token("try")]
    Try,

    #[token("type")]
    Type,

    #[token("union")]
    Union,

    #[token("while")]
    While,

    // ==========
    // Well-known values
    // ==========
    #[token("false")]
    False,

    #[token("true")]
    True,

    // ==========
    // Delimiters
    // ==========
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    // ==========
    // Operators
    // ==========
    #[token("+")]
    Plus,

    #[token("-")]
    Dash,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("^")]
    Caret,

    #[token("%")]
    Percent,

    #[token("<")]
    LAngle,

    #[token("<=")]
    LAngleEquals,

    #[token(">")]
    RAngle,

    #[token(">=")]
    RAngleEquals,

    #[token("==")]
    EqualsEquals,

    #[token("!=")]
    BangEquals,

    #[token("=")]
    Equals,

    #[token("..")]
    DotDot,

    #[token(".")]
    Dot,

    // ==========
    // Trivia (Emptyspace, comments)
    // ==========
    #[token("/*", |lex| {
        let len = lex.remainder().find("*/")?;
        lex.bump(len + 2); // include len of `*/`
        Some(())
    })]
    Comment,

    #[regex(" +")]
    Emptyspace,

    #[token("\n")]
    Newline,

    #[error]
    Error,

    Root,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Emptyspace | Self::Comment)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Ident => "identifier",

            // Keywords
            Self::And => "‘and’",
            Self::Else => "‘else’",
            Self::For => "‘for’",
            Self::If => "‘if’",
            Self::Let => "‘let’",
            Self::Loop => "‘loop’",
            Self::Module => "‘module’",
            Self::Mutable => "‘mutable’",
            Self::Not => "‘not’",
            Self::Or => "‘or’",
            Self::Return => "‘return’",
            Self::Struct => "‘struct’",
            Self::Try => "‘try’",
            Self::Type => "‘type’",
            Self::Union => "‘union’",
            Self::While => "‘while’",

            // Literals
            Self::False => "‘false’",
            Self::True => "‘true’",
            Self::Int => "int",
            Self::String => "string",

            // Delimiters
            Self::LBrace => "‘{’",
            Self::RBrace => "‘}’",
            Self::LBracket => "‘[’",
            Self::RBracket => "‘]’",
            Self::LParen => "‘(’",
            Self::RParen => "‘)’",

            // Operators
            Self::Plus => "‘+’",
            Self::Dash => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Caret => "‘^’",
            Self::Percent => "‘%’",

            Self::LAngle => "‘<’",
            Self::LAngleEquals => "‘<=’",
            Self::RAngle => "‘>’",
            Self::RAngleEquals => "‘>=’",
            Self::EqualsEquals => "‘==’",
            Self::BangEquals => "‘!=’",
            Self::Equals => "‘=’",
            Self::DotDot => "‘..’",
            Self::Dot => "‘.’",

            Self::Emptyspace => "emptyspace",
            Self::Newline => "newline",
            Self::Comment => "comment",
            Self::Error => "unrecognized token",

            Self::Root => "root",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = TokenKind::lexer(input);

        assert_eq!(lexer.next(), Some(kind));
        assert_eq!(lexer.slice(), input);
    }

    fn check_err(input: &str) {
        let mut lexer = TokenKind::lexer(input);

        assert_eq!(lexer.next(), Some(TokenKind::Error));
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
        check_err("_abc");
    }

    #[test]
    fn lex_number() {
        check("123456", TokenKind::Int);
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
    fn lex_let_keyword() {
        check("let", TokenKind::Let);
    }

    #[test]
    fn lex_mutable_keyword() {
        check("mutable", TokenKind::Mutable);
    }

    #[test]
    fn lex_not_keyword() {
        check("not", TokenKind::Not);
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
    fn lex_false_value() {
        check("false", TokenKind::False);
    }

    #[test]
    fn lex_true_keyword() {
        check("true", TokenKind::True);
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
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
}