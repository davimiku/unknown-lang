pub(crate) mod marker;
pub(crate) mod parse_error;

pub(crate) use parse_error::ParseError;

use crate::event::Event;
use crate::grammar;
use crate::source::Source;
use crate::syntax::SyntaxKind;
use lexer::{Token, TokenKind};
use marker::Marker;
use std::mem;

const RECOVERY_SET: [TokenKind; 1] = [TokenKind::Let];

pub(crate) struct Parser<'t, 'input> {
    source: Source<'t, 'input>,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
    pub(crate) entry_point: ParseEntryPoint,
}

impl<'t, 'input> Parser<'t, 'input> {
    pub(crate) fn new(source: Source<'t, 'input>, entry_point: ParseEntryPoint) -> Self {
        Self {
            source,
            events: Vec::new(),
            expected_kinds: Vec::new(),
            entry_point,
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            // If we’re at the end of the input we use the range of the very last token in the input.
            (None, self.source.last_token_range().unwrap())
        };

        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        }));

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn error_and_bump(&mut self, message: &str) {
        self.error_recover(message, &[]);
    }

    // TODO: use a similar TokenSet from RA
    pub(crate) fn error_recover(&mut self, message: &str, recovery: &[TokenKind]) {
        match self.peek() {
            Some(t) => match t {
                TokenKind::LBrace | TokenKind::RBrace => {
                    self.error();
                    return;
                }

                _ => (),
            },
            None => todo!(),
        }

        for kind in recovery {
            if self.at(*kind) {
                self.error();
                return;
            }
        }

        let m = self.start();
        self.error();
        self.bump();
        m.complete(self, SyntaxKind::Error);
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
}

pub(super) enum ParseEntryPoint {
    Root, // should produce a Vec of Stmt

    Expr, // useful for testing parsing single expressions

          // possibly others in the future for metaprogramming (RA has separate entry for macro expansion)
}
