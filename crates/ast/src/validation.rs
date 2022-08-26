use std::fmt;

use parser::SyntaxNode;
use text_size::TextRange;

use crate::IntLiteral;

#[derive(Debug, PartialEq)]
pub struct ValidationError {
    kind: ValidationErrorKind,
    range: TextRange,
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = u32::from(self.range.start());
        let end = u32::from(self.range.end());
        let kind = self.kind;
        write!(f, "error at {start}..{end}: {kind}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValidationErrorKind {
    NumberLiteralTooLarge,
}

impl fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NumberLiteralTooLarge => write!(
                f,
                "integer literal is larger than maximum value, {}",
                i64::MAX
            ),
        }
    }
}

pub fn validate(node: &SyntaxNode) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    for node in node.descendants() {
        if let Some(literal) = IntLiteral::cast(node) {
            validate_int_literal(literal, &mut errors)
        }
    }

    errors
}

fn validate_int_literal(literal: IntLiteral, errors: &mut Vec<ValidationError>) {
    if literal.value().is_none() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::NumberLiteralTooLarge,
            range: literal.0.first_token().unwrap().text_range(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::ops::Range;

    fn check(input: &str, expected_errors: &[(ValidationErrorKind, Range<u32>)]) {
        let parse = parser::parse(input);

        let expected_errors: Vec<_> = expected_errors
            .iter()
            .map(|(kind, range)| ValidationError {
                kind: *kind,
                range: {
                    let start = range.start.into();
                    let end = range.end.into();
                    TextRange::new(start, end)
                },
            })
            .collect();

        assert_eq!(validate(&parse.syntax()), expected_errors);
    }

    #[test]
    fn validate_ok_literal() {
        check("123", &[]);
    }

    #[test]
    #[ignore = "need to implement LSP diagnostics with parsing, potentially removing this kind of check"]
    fn validate_too_large_literal() {
        let input = "1000000000000000000000";
        let len = input.len() as u32;
        check(
            input,
            &[(ValidationErrorKind::NumberLiteralTooLarge, (0..len))],
        )
    }
}
