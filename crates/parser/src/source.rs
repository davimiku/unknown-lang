use lexer::{Token, TokenKind};
use rowan::TextRange;

/// Represents the source of the tokens into the parser,
/// typically borrowed from the lexer
#[derive(Debug)]
pub(super) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    cursor: usize,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(super) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.eat_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    /// Finds the next TokenKind, also eats trivia
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    pub(super) fn peek_next(&mut self) -> Option<&Token> {
        self.eat_trivia();
        let next_idx = self.cursor + 1;

        self.tokens[next_idx..].iter().find(|t| !t.kind.is_trivia())
    }

    pub(super) fn peek_next_kind(&mut self) -> Option<TokenKind> {
        self.peek_next().map(|t| t.kind)
    }

    pub(super) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().is_some_and(TokenKind::is_trivia)
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.tokens.get(self.cursor).map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}
