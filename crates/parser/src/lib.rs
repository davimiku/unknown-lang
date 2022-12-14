mod event;
mod grammar;
mod parser;
mod sink;
mod source;
mod syntax;

use self::parser::{ParseEntryPoint, ParseError, Parser};
use lexer::Lexer;
use rowan::GreenNode;
use sink::Sink;
use source::Source;
use std::fmt::Write;
pub use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub fn parse(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source, ParseEntryPoint::Root);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

/// parsing entry point for expressions directly for easier testing
pub fn parse_expr(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source, ParseEntryPoint::ExprText);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<ParseError>,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let tree = format!("{:#?}", self.syntax());

        // Remove the newline from the end
        s.push_str(&tree[0..tree.len() - 1]);

        for error in &self.errors {
            write!(s, "\n{error}").expect("write! shouldn't fail on strings");
        }

        s
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
fn check_error(input: &str, expected_tree: expect_test::Expect, expected_errors: Vec<ParseError>) {
    let parse = parse(input);

    expected_tree.assert_eq(&parse.debug_tree());
    assert_eq!(parse.errors, expected_errors);
}

// Convenience function to test expression parsing directly, since _most_
// language constructs are expressions.
#[cfg(test)]
fn check_expr(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}
