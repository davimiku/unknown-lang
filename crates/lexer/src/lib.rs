use std::ops::Range;

use logos::Logos;
use text_size::{TextRange, TextSize};
pub use token_kind::TokenKind;

mod token_kind;

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        let range = {
            let Range { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };

        Some(Self::Item { kind, text, range })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub range: TextRange,
}

#[cfg(test)]
mod tests {
    use super::token_kind::TokenKind::*;
    use super::*;

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
            Token::new(Plus, "+", range(0, 1)),
            Token::new(Dash, "-", range(1, 2)),
            Token::new(Star, "*", range(2, 3)),
            Token::new(Slash, "/", range(3, 4)),
        ];
        let lexer = Lexer::new("+-*/");

        let actual: Vec<Token> = lexer.collect();

        assert_eq!(actual, expected);
    }
}
