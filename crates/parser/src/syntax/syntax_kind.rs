use std::fmt;

use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Hash, PartialOrd, Ord)]
pub enum SyntaxKind {
    VariableIdent,

    Hash,
    At,

    Ident,

    // Keywords
    And,
    Else,
    For,
    If,
    Let,
    Loop,
    Module,
    Mutable,
    Not,
    Or,
    Return,
    Struct,
    Try,
    Type,
    Union,
    While,

    // Literals
    False,
    True,
    Int,
    String,

    // Delimiters
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,

    // Operators
    Plus,
    Dash,
    Star,
    Slash,
    Caret,
    Percent,

    LAngle,
    LAngleEquals,
    RAngle,
    RAngleEquals,
    EqualsEquals,
    BangEquals,
    Equals,
    DotDot,
    Dot,
    Question,
    Colon,
    SemiColon,

    // Stmt
    ModuleDef,
    VariableDef,
    ExprStmt,

    // Expr
    BlockExpr,
    InfixExpr,
    IntExpr,
    LoopExpr,
    PrefixExpr,
    ParenExpr,
    StringExpr,
    VariableRef,

    Root,
    Comment,
    Emptyspace,
    Newline,
    Error,
}

impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Emptyspace | Self::Comment)
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            // Keywords
            TokenKind::And => Self::And,
            TokenKind::Else => Self::Else,
            TokenKind::For => Self::For,
            TokenKind::If => Self::If,
            TokenKind::Let => Self::Let,
            TokenKind::Loop => Self::Loop,
            TokenKind::Module => Self::Module,
            TokenKind::Mutable => Self::Mutable,
            TokenKind::Not => Self::Not,
            TokenKind::Or => Self::Or,
            TokenKind::Return => Self::Return,
            TokenKind::Struct => Self::Struct,
            TokenKind::Try => Self::Try,
            TokenKind::Type => Self::Type,
            TokenKind::Union => Self::Union,
            TokenKind::While => Self::While,

            // Literals
            TokenKind::Ident => Self::Ident,
            TokenKind::False => Self::False,
            TokenKind::True => Self::True,
            TokenKind::Int => Self::Int,
            TokenKind::String => Self::StringExpr,

            // Delimiters
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LBracket => Self::LBracket,
            TokenKind::RBracket => Self::RBracket,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,

            // Operators
            TokenKind::Plus => Self::Plus,
            TokenKind::Dash => Self::Dash,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Caret => Self::Caret,
            TokenKind::Percent => Self::Percent,
            TokenKind::Equals => Self::Equals,
            TokenKind::Comment => Self::Comment,
            TokenKind::Error => Self::Error,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::LAngleEquals => Self::LAngleEquals,
            TokenKind::RAngle => Self::RAngle,
            TokenKind::RAngleEquals => Self::RAngleEquals,
            TokenKind::EqualsEquals => Self::EqualsEquals,
            TokenKind::BangEquals => Self::BangEquals,
            TokenKind::DotDot => Self::DotDot,
            TokenKind::Dot => Self::Dot,

            TokenKind::Root => Self::Root,
            TokenKind::Emptyspace => Self::Emptyspace,
            TokenKind::Newline => Self::Newline,
        }
    }
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            // Keywords
            // Literals
            SyntaxKind::IntExpr => "number",

            // Delimiters
            SyntaxKind::LParen => "‘(’",
            SyntaxKind::RParen => "‘)’",
            SyntaxKind::LBrace => "‘{’",
            SyntaxKind::RBrace => "‘}’",
            SyntaxKind::LBracket => "‘[’",
            SyntaxKind::RBracket => "‘]’",

            SyntaxKind::Emptyspace => "emptyspace",
            SyntaxKind::Newline => "newline",
            SyntaxKind::VariableIdent => "identifier",

            // Operators
            SyntaxKind::Plus => "‘+’",
            SyntaxKind::Dash => "‘-’",
            SyntaxKind::Star => "‘*’",
            SyntaxKind::Slash => "‘/’",
            SyntaxKind::Equals => "‘=’",
            SyntaxKind::Hash => "‘#’",
            SyntaxKind::Percent => "‘%’",
            SyntaxKind::At => "‘@’",
            SyntaxKind::Comment => "comment",
            SyntaxKind::LAngle => "‘<’",
            SyntaxKind::RAngle => "‘>’",
            SyntaxKind::DotDot => "‘..’",
            SyntaxKind::Dot => "‘.’",
            SyntaxKind::Question => "‘?’",
            SyntaxKind::Colon => "‘:’",
            SyntaxKind::SemiColon => "‘;’",

            _ => unreachable!("{}", format!("unreachable: found {:?}", &self)),
        })
    }
}
