use std::mem;

use crate::{parser::ParseError, Parse};

use super::event::Event;
use crate::syntax::{SyntaxKind, UnknownLanguage};
use lexer::Token;
use rowan::{GreenNodeBuilder, Language};

#[derive(Debug)]
pub(super) struct Sink<'t, 'input> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'input>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParseError>,
}

impl<'t, 'input> Sink<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: Vec::new(),
        }
    }

    pub(super) fn finish(mut self) -> Parse {
        for i in 0..self.events.len() {
            let prev = mem::replace(&mut self.events[i], Event::Placeholder);
            match prev {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut idx = i;
                    let mut forward_parent = forward_parent;

                    // Walk through the forward parent of the forward parent, and the forward parent
                    // of that, and of that, etc. until we reach a StartNode event without a forward
                    // parent.
                    while let Some(fp) = forward_parent {
                        idx += fp;

                        let prev = mem::replace(&mut self.events[idx], Event::Placeholder);

                        forward_parent = match prev {
                            Event::StartNode {
                                kind,
                                forward_parent,
                            } => {
                                kinds.push(kind);
                                forward_parent
                            }

                            _ => unreachable!(),
                        }
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(UnknownLanguage::kind_to_raw(kind));
                    }
                }
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Error(error) => self.errors.push(error),
                Event::Placeholder => {}
            }

            self.eat_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !SyntaxKind::from(token.kind).is_trivia() {
                break;
            }

            self.token()
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder
            .token(UnknownLanguage::kind_to_raw(kind.into()), text);

        self.cursor += 1;
    }
}
