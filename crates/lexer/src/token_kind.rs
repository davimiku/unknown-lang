use std::fmt;

use logos::Logos;

#[derive(Logos, PartialEq, Eq, Debug, Clone, Copy)]
#[logos(subpattern decimal = r"[0-9][_0-9]*")]
pub enum TokenKind {
    #[regex("[A-Za-z][A-Za-z0-9_]*")]
    Ident,

    #[regex("_[A-Za-z0-9_]*", priority = 3)]
    InvalidLeadingUnderscore,

    #[token("_")]
    Underscore,

    // TODO: implement better numbers
    // https://github.com/maciejhirsz/logos/issues/133#issuecomment-687281059
    #[regex("(?&decimal)")]
    IntLiteral,

    #[regex(r#"(?&decimal)\.(?&decimal)?"#)]
    FloatLiteral,

    #[regex("\"[^\"\n]*\"")]
    StringLiteral,

    // ==========
    // Keywords
    // ==========
    /// `and`
    #[token("and")]
    And,

    #[token("break")]
    Break,

    /// `else`
    #[token("else")]
    Else,

    /// `for`
    #[token("for")]
    For,

    /// `fun`
    #[token("fun")]
    Fun,

    /// `if`
    #[token("if")]
    If,

    /// `in`
    #[token("in")]
    In,

    /// `let`
    #[token("let")]
    Let,

    /// `loop`
    #[token("loop")]
    Loop,

    /// `match`
    #[token("match")]
    Match,

    /// `module`
    #[token("module")]
    Module,

    /// `mut`
    #[token("mut")]
    Mut,

    /// `or`
    #[token("or")]
    Or,

    /// `return`
    #[token("return")]
    Return,

    /// `struct`
    #[token("struct")]
    Struct,

    /// `try`
    #[token("try")]
    Try,

    /// `type`
    #[token("type")]
    Type,

    /// `while`
    #[token("while")]
    While,

    // ==========
    // Well-known values
    // ==========
    /// `false`
    // #[token("false")]
    // False,

    // /// `true`
    // #[token("true")]
    // True,

    // ==========
    // Delimiters
    // ==========
    /// `{`
    #[token("{")]
    LBrace,

    /// `}`
    #[token("}")]
    RBrace,

    /// `(`
    #[token("(")]
    LParen,

    /// `)`
    #[token(")")]
    RParen,

    /// `[`
    #[token("[")]
    LBracket,

    /// `]`
    #[token("]")]
    RBracket,

    // ==========
    // Separators
    // ==========
    /// `:`
    #[token(":")]
    Colon,

    /// `;`
    #[token(";")]
    Semicolon,

    /// `,`
    #[token(",")]
    Comma,

    // ==========
    // Operators
    // ==========
    /// `+`
    #[token("+")]
    Plus,

    /// `++`
    #[token("++")]
    PlusPlus,

    /// `-`
    #[token("-")]
    Dash,

    /// `*`
    #[token("*")]
    Star,

    /// `/`
    #[token("/")]
    Slash,

    /// `^`
    #[token("^")]
    Caret,

    /// `%`
    #[token("%")]
    Percent,

    /// `|`
    #[token("|")]
    Bar,

    /// `!`
    #[token("!")]
    Bang,

    /// `~`
    #[token("~")]
    Tilde,

    /// `<`
    #[token("<")]
    LAngle,

    /// `<=`
    #[token("<=")]
    LAngleEquals,

    /// `>`
    #[token(">")]
    RAngle,

    /// `>=`
    #[token(">=")]
    RAngleEquals,

    /// `==`
    #[token("==")]
    EqualsEquals,

    /// `!=`
    #[token("!=")]
    BangEquals,

    /// `=`
    #[token("=")]
    Equals,

    /// `..`
    #[token("..")]
    DotDot,

    /// `.`
    #[token(".")]
    Dot,

    /// `->`
    #[token("->")]
    Arrow,

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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Ident => "identifier",
            Self::InvalidLeadingUnderscore => "_invalid_identifier",

            // Keywords
            Self::And => "‘and’",
            Self::Break => "‘break’",
            Self::Else => "‘else’",
            Self::For => "‘for’",
            Self::Fun => "‘fun’",
            Self::If => "‘if’",
            Self::In => "‘in’",
            Self::Let => "‘let’",
            Self::Loop => "‘loop’",
            Self::Match => "‘match’",
            Self::Module => "‘module’",
            Self::Mut => "‘mut’",
            Self::Or => "‘or’",
            Self::Return => "‘return’",
            Self::Struct => "‘struct’",
            Self::Try => "‘try’",
            Self::Type => "‘type’",
            Self::While => "‘while’",

            // Literals
            // Self::False => "‘false’",
            // Self::True => "‘true’",
            Self::IntLiteral => "Int",
            Self::FloatLiteral => "Float",
            Self::StringLiteral => "String",

            // Delimiters
            Self::LBrace => "‘{’",
            Self::RBrace => "‘}’",
            Self::LBracket => "‘[’",
            Self::RBracket => "‘]’",
            Self::LParen => "‘(’",
            Self::RParen => "‘)’",

            // Placehodlers
            Self::Underscore => "‘_’",

            // Separators
            Self::Colon => "‘:’",
            Self::Semicolon => "‘;’",
            Self::Comma => "‘,’",

            // Operators
            Self::Plus => "‘+’",
            Self::PlusPlus => "‘++’",
            Self::Dash => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Caret => "‘^’",
            Self::Percent => "‘%’",
            Self::Bar => "‘|’",
            Self::Bang => "‘!’",
            Self::Tilde => "‘~’",

            Self::LAngle => "‘<’",
            Self::LAngleEquals => "‘<=’",
            Self::RAngle => "‘>’",
            Self::RAngleEquals => "‘>=’",
            Self::EqualsEquals => "‘==’",
            Self::BangEquals => "‘!=’",
            Self::Equals => "‘=’",
            Self::DotDot => "‘..’",
            Self::Dot => "‘.’",

            Self::Arrow => "‘->’",

            Self::Emptyspace => "emptyspace",
            Self::Newline => "newline",
            Self::Comment => "comment",
            Self::Error => "unrecognized token",

            Self::Root => "root",
        })
    }
}
