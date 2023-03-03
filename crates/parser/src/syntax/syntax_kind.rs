use std::fmt;

use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Hash, PartialOrd, Ord)]
pub enum SyntaxKind {
    Hash,
    At,

    Ident,
    Call,
    CallArgs,

    // Keywords
    And,
    Else,
    For,
    If,
    Let,
    Loop,
    Module,
    Or,
    Return,
    Struct,
    Try,
    Type,
    Union,
    While,

    // Literals
    FalseLiteral,
    TrueLiteral,
    IntLiteral,
    FloatLiteral,
    StringLiteral,

    // Delimiters
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,

    // Separators
    Colon,
    SemiColon,
    Comma,

    // Operators
    Plus,
    PlusPlus,
    Dash,
    Star,
    Slash,
    Caret,
    Percent,
    Bang,

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

    // Expr
    AssignmentExpr,
    BlockExpr,
    BoolExpr,
    FloatExpr,
    FunExpr,
    IfExpr,
    InfixExpr,
    IntExpr,
    LoopExpr,
    NegationExpr,
    NotExpr,
    ParenExpr,
    Path,
    PathItem,
    StringExpr,
    StructTypeExpr,
    TypeExpr,
    UnionTypeExpr,

    // Supplemental
    ParenExprItem,

    // IfExpr
    ConditionExpr,
    ThenBranchExpr,
    ElseBranchExpr,

    // Expr "statements"
    ImportBinding,
    LetBinding,
    TypeBinding,

    // Type components
    CompoundTypeBlock,
    CompoundTypeItem,

    // Function components
    FunBody,
    // FunParam,
    FunParamList,
    Arrow,
    FunReturnType,

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
            TokenKind::Or => Self::Or,
            TokenKind::Return => Self::Return,
            TokenKind::Struct => Self::Struct,
            TokenKind::Try => Self::Try,
            TokenKind::Type => Self::Type,
            TokenKind::Union => Self::Union,
            TokenKind::While => Self::While,

            // Literals
            TokenKind::Ident => Self::Ident,
            TokenKind::False => Self::FalseLiteral,
            TokenKind::True => Self::TrueLiteral,
            TokenKind::FloatLiteral => Self::FloatLiteral,
            TokenKind::IntLiteral => Self::IntLiteral,
            TokenKind::StringLiteral => Self::StringExpr,

            // Delimiters
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LBracket => Self::LBracket,
            TokenKind::RBracket => Self::RBracket,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,

            // Separators
            TokenKind::Colon => Self::Colon,
            TokenKind::Semicolon => Self::SemiColon,
            TokenKind::Comma => Self::Comma,

            // Operators
            TokenKind::Plus => Self::Plus,
            TokenKind::PlusPlus => Self::PlusPlus,
            TokenKind::Dash => Self::Dash,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Caret => Self::Caret,
            TokenKind::Percent => Self::Percent,
            TokenKind::Bang => Self::Bang,
            TokenKind::Equals => Self::Equals,
            TokenKind::Comment => Self::Comment,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::LAngleEquals => Self::LAngleEquals,
            TokenKind::RAngle => Self::RAngle,
            TokenKind::RAngleEquals => Self::RAngleEquals,
            TokenKind::EqualsEquals => Self::EqualsEquals,
            TokenKind::BangEquals => Self::BangEquals,
            TokenKind::DotDot => Self::DotDot,
            TokenKind::Dot => Self::Dot,
            TokenKind::Arrow => Self::Arrow,

            TokenKind::Root => Self::Root,
            TokenKind::Emptyspace => Self::Emptyspace,
            TokenKind::Newline => Self::Newline,

            TokenKind::Error => Self::Error,
        }
    }
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            // Keywords
            // Literals
            SyntaxKind::IntExpr => "int",
            SyntaxKind::FloatExpr => "float",

            // Delimiters
            SyntaxKind::LParen => "‘(’",
            SyntaxKind::RParen => "‘)’",
            SyntaxKind::LBrace => "‘{’",
            SyntaxKind::RBrace => "‘}’",
            SyntaxKind::LBracket => "‘[’",
            SyntaxKind::RBracket => "‘]’",

            SyntaxKind::Emptyspace => "emptyspace",
            SyntaxKind::Newline => "newline",

            // Operators
            SyntaxKind::Plus => "‘+’",
            SyntaxKind::PlusPlus => "‘++’",
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
