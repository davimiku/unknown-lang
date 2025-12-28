use std::fmt::Display;

/// Asserts that the provided enum is the provided variant,
/// and extracts the inner value.
#[macro_export]
macro_rules! assert_matches {
    ($value:expr, $variant:path) => {{
        if let $variant(x) = $value {
            x
        } else {
            dbg!($value);
            unreachable!("Unexpected variant")
        }
    }};
}

#[macro_export]
macro_rules! assert_some {
    ($value:expr) => {{
        assert!($value.is_some());
        $value.unwrap()
    }};
}

enum CompilerPhase {
    Lexer,
    Parser,
    Ast,
    Hir,
    Mir,
    Clif,
    Js,
}

impl Display for CompilerPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerPhase::Lexer => f.write_str("Lexer"),
            CompilerPhase::Parser => f.write_str("Parser"),
            CompilerPhase::Ast => f.write_str("AST"),
            CompilerPhase::Hir => f.write_str("HIR"),
            CompilerPhase::Mir => f.write_str("MIR"),
            CompilerPhase::Clif => f.write_str("CLIF"),
            CompilerPhase::Js => f.write_str("JS"),
        }
    }
}

// TODO - macro (or function) to standardize internal compiler errors
//
// ice!(CompilerPhase.Clif, "Could not compute layout for {}", ty.display(self.context))
// #[macro_export]
// macro_rules! ice {
//
//}
