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

/// Struct that maintains state while parsing the tokens
/// into a Concrete Syntax Tree.
#[derive(Debug)]
pub(crate) struct Parser<'t, 'input> {
    /// Token source
    source: Source<'t, 'input>,

    errors: Vec<ParseError>,

    /// Parsing events that have occurred
    events: Vec<Event>,

    /// Temporary list of expected tokens for error reporting
    expected_kinds: Vec<TokenKind>,

    /// The last token just parsed
    pub(crate) last_token_kind: TokenKind,

    // TODO: do we need a 1 token lookahead?
    // pub(crate) next_token_kind: Option<TokenKind>,
    //
    /// Entry point of parsing to control how the events are used
    pub(crate) entry_point: ParseEntryPoint,
}

impl<'t, 'input> Parser<'t, 'input> {
    pub(crate) fn new(source: Source<'t, 'input>, entry_point: ParseEntryPoint) -> Self {
        Self {
            source,
            events: Vec::new(),
            expected_kinds: Vec::new(),
            last_token_kind: TokenKind::Emptyspace,
            errors: Vec::new(),
            entry_point,
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    /// Creates a new `Marker` at the parser's current position
    ///
    /// Inserts a `Placeholder` event which is later replaced with
    /// a `StartNode` event when the `Marker` is completed.
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    /// Bumps if the expected matches actual, errors otherwise
    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn expect_one_of<const N: usize>(&mut self, kinds: [TokenKind; N]) {
        for kind in kinds {
            if self.at(kind) {
                self.bump();
                return;
            }
        }
        self.error();
    }

    pub(crate) fn expect_no_skip(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_no_skip();
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(token) = current_token {
            (Some(token.kind), token.range)
        } else {
            // If already at the end, best thing is the range of the last token
            let range = self.source.last_token_range().unwrap();

            (None, range)
        };

        let expected = mem::take(&mut self.expected_kinds);
        let parse_error = ParseError {
            expected,
            found,
            range,
        };
        self.errors.push(parse_error.clone());
        self.events.push(Event::Error(parse_error));

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn error_no_skip(&mut self) {
        //
    }

    /// Consumes the next token and clears all expected tokens
    ///
    /// Panic: if the input is already at the end
    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.last_token_kind = self.source.peek_kind().unwrap();
        self.source.next_token().expect("a next token to exist");
        self.events.push(Event::AddToken);
    }

    /// Bumps the parser if it's at the given token.
    /// Useful for optional tokens, such as trailing commas
    pub(crate) fn bump_if(&mut self, kind: TokenKind) -> bool {
        self.bump_all_space();
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Consumes all emptyspace and newlines until the next non-space token
    pub(crate) fn bump_all_space(&mut self) {
        while self.at_set(&[TokenKind::Emptyspace, TokenKind::Newline]) {
            self.bump();
        }
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

    /// Checks if the parser is at a certain kind of valid token
    ///
    /// Side effect: adds this token to the current list of expected tokens
    /// in case of a future parse error
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    /// Checks if the parser is at a certain kind of valid token
    ///
    /// Does not add to the list of expected tokens.
    pub(crate) fn debug_assert_at(&mut self, kind: TokenKind) {
        debug_assert_eq!(self.peek(), Some(kind))
    }

    pub(crate) fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().is_some_and(|k| set.contains(&k))
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Peeks for the next TokenKind, also eats trivia
    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        // self. source.peek_next()
        self.source.peek_token()
    }
}

#[derive(Debug)]
pub(super) enum ParseEntryPoint {
    Root, // normal root of a file

    ExprTest, // test parsing single expressions
    TypeTest, // test parsing single type expressions

              // possibly others in the future for metaprogramming (RA has separate entry for macro expansion)
}
