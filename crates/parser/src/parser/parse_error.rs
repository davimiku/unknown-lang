use lexer::TokenKind;
use std::fmt;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) found: Option<TokenKind>,
    pub(super) range: TextRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        let num_expected = self.expected.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if is_first(idx) {
                write!(f, "{expected_kind}")?;
            } else if is_last(idx) {
                write!(f, " or {expected_kind}")?;
            } else {
                write!(f, ", {expected_kind}")?;
            }
        }

        if let Some(found) = self.found {
            write!(f, " but found {found}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range as StdRange;

    use lexer::TokenKind;
    use text_size::TextRange;

    use crate::parser::ParseError;

    fn check(
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: StdRange<u32>,
        output: &str,
    ) {
        let error = ParseError {
            expected,
            found,
            range: {
                let start = range.start.into();
                let end = range.end.into();
                TextRange::new(start, end)
            },
        };

        assert_eq!(format!("{}", error), output);
    }

    #[test]
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Equals],
            Some(TokenKind::Ident),
            10..20,
            "error at 10..20: expected ‘=’ but found identifier",
        );
    }

    #[test]
    fn one_expected_did_not_find() {
        check(
            vec![TokenKind::RParen],
            None,
            5..6,
            "error at 5..6: expected ‘)’",
        );
    }

    #[test]
    fn two_expected_did_find() {
        check(
            vec![TokenKind::Plus, TokenKind::Dash],
            Some(TokenKind::Equals),
            0..1,
            "error at 0..1: expected ‘+’ or ‘-’ but found ‘=’",
        );
    }

    #[test]
    fn multiple_expected_did_find() {
        check(
            vec![
                TokenKind::IntLiteral,
                TokenKind::Ident,
                TokenKind::Dash,
                TokenKind::LParen,
            ],
            Some(TokenKind::Let),
            100..105,
            "error at 100..105: expected Int, identifier, ‘-’ or ‘(’ but found ‘let’",
        );
    }
}
