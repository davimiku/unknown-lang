use std::fmt;

use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Hash, PartialOrd, Ord)]
pub enum SyntaxKind {
    Hash,
    At,

    // Keywords
    AndKw,
    BreakKw,
    ElseKw,
    ForKw,
    FunKw,
    IfKw,
    InKw,
    LetKw,
    LoopKw,
    MatchKw,
    ModuleKw,
    MutKw,
    OrKw,
    ReturnKw,
    StructKw,
    TryKw,
    TypeKw,
    WhileKw,

    // Literals
    FalseLiteral,
    TrueLiteral,
    IntLiteral,
    FloatLiteral,
    StringLiteral,

    ListLiteral,
    ArrayType,

    // Delimiters
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `(`
    LParen,
    /// `)`
    RParen,

    // Separators
    /// `:`
    Colon,
    /// `;`
    SemiColon,
    /// `,`
    Comma,

    // Placeholders
    /// `_`
    Underscore,
    /// `_ident` - identifiers with leading underscores
    /// (parse successfully for robustness, produces error at lowering)
    InvalidLeadingUnderscore,

    // Operators
    /// `+`
    Plus,
    /// `++`
    PlusPlus,
    /// `-`
    Dash,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,
    /// `|`
    Bar,
    /// `!`
    Bang,
    /// `~`
    Tilde,

    /// `<`
    LAngle,
    /// `<=`
    LAngleEquals,
    /// `>`
    RAngle,
    /// `>=`
    RAngleEquals,
    /// `==`
    EqualsEquals,
    /// `!=`
    BangEquals,
    /// `=`
    Equals,
    /// `..`
    DotDot,
    /// `.`
    Dot,
    /// `?`
    Question,

    // Expr
    AssignmentExpr,
    BlockExpr,
    FloatLiteralExpr,
    FunExpr,
    IfExpr,
    InfixExpr,
    IntLiteralExpr,
    LoopExpr,
    MatchExpr,
    NegationExpr,
    NotExpr,
    ParenExpr,
    PathExpr,
    PathItem,
    StringLiteralExpr,
    StructTypeExpr,
    IntoStringExpr,
    TypeExpr,
    UnionTypeExpr,

    // Supplemental
    ParenExprItem,

    // related to MatchExpr
    ScrutineeExpr,
    MatchBlock,
    MatchArm,

    // related to IfExpr
    ConditionExpr,
    ThenBranchExpr,
    ElseBranchExpr,

    // Expr "statements"
    ImportBinding,
    LetBinding,
    TypeBinding,
    ForInLoop,
    BreakStatement,
    ReturnStatement,

    // Type components
    CompoundTypeBlock,
    CompoundTypeItem,
    CompoundTypeItemIdent,
    CompoundTypeItemType,
    CompoundTypeItemDefault,

    // Related to patterns
    IdentPattern,
    DotPattern,
    IntLiteralPattern,
    FloatLiteralPattern,
    StringLiteralPattern,
    WildcardPattern,
    Ident,

    Call,
    CallArgs,

    // Function components
    FunBody,
    FunParam,
    FunParamList,
    Arrow,
    FunReturnType,

    Root,
    Comment,
    Emptyspace,
    /// Captured separately from Emptyspace because the grammar *is* newline-sensitive
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
            TokenKind::And => Self::AndKw,
            TokenKind::Break => Self::BreakKw,
            TokenKind::Else => Self::ElseKw,
            TokenKind::For => Self::ForKw,
            TokenKind::Fun => Self::FunKw,
            TokenKind::If => Self::IfKw,
            TokenKind::In => Self::InKw,
            TokenKind::Let => Self::LetKw,
            TokenKind::Loop => Self::LoopKw,
            TokenKind::Match => Self::MatchKw,
            TokenKind::Module => Self::ModuleKw,
            TokenKind::Mut => Self::MutKw,
            TokenKind::Or => Self::OrKw,
            TokenKind::Return => Self::ReturnKw,
            TokenKind::Struct => Self::StructKw,
            TokenKind::Try => Self::TryKw,
            TokenKind::Type => Self::TypeKw,
            TokenKind::While => Self::WhileKw,

            // Literals
            TokenKind::Ident => Self::Ident,
            // TokenKind::False => Self::FalseLiteral,
            // TokenKind::True => Self::TrueLiteral,
            TokenKind::FloatLiteral => Self::FloatLiteral,
            TokenKind::IntLiteral => Self::IntLiteral,
            TokenKind::StringLiteral => Self::StringLiteralExpr,

            // Delimiters
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LBracket => Self::LBracket,
            TokenKind::RBracket => Self::RBracket,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,

            // Placeholders
            TokenKind::Underscore => Self::Underscore,
            TokenKind::InvalidLeadingUnderscore => Self::InvalidLeadingUnderscore,

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
            TokenKind::Bar => Self::Bar,
            TokenKind::Bang => Self::Bang,
            TokenKind::Tilde => Self::Tilde,

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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            // Keywords
            // Literals
            SyntaxKind::IntLiteralExpr => "int",
            SyntaxKind::FloatLiteralExpr => "float",

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
            SyntaxKind::Bang => "‘!’",
            SyntaxKind::Tilde => "‘~’",
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
