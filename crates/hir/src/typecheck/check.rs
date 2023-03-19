//!
//! Type checker
//!
//!

use la_arena::Idx;
use text_size::TextRange;

use super::infer::infer_expr;
use super::{Type, TypeCheckResults, TypeDiagnostic, TypeDiagnosticVariant};
use crate::database::Database;
use crate::Expr;

pub(crate) fn check_expr(
    expr: Idx<Expr>,
    expected: Type,
    results: &mut TypeCheckResults,
    database: &Database,
) -> Result<(), TypeDiagnostic> {
    let actual = infer_expr(expr, results, database)?;
    if is_subtype(&actual, &expected) {
        Ok(())
    } else {
        Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::TypeMismatch { expected, actual },
            // TODO: get a real TextRange from database
            range: TextRange::default(),
        })
    }
}

/// Is A a subtype of B
///
/// A subtype allows for A to be used in expressions where B is expected.
///
/// For example:
///    FloatLiteral is a subtype of Float
///    If a Float was required, a FloatLiteral would suffice.
fn is_subtype(a: &Type, b: &Type) -> bool {
    use Type as T;

    if a == b {
        return true;
    }

    match (a, b) {
        (_, T::Top) => true,
        (_, T::Bottom) => false,

        (T::Bool, T::Named(_)) => todo!(),
        (T::BoolLiteral(_), T::Bool) => true,
        (T::BoolLiteral(a), T::BoolLiteral(b)) => a == b,
        (T::BoolLiteral(_), T::Named(_)) => todo!(),

        (T::Float, T::Named(_)) => todo!(),
        (T::FloatLiteral(_), T::Float) => true,
        (T::FloatLiteral(a), T::FloatLiteral(b)) => a == b,
        (T::FloatLiteral(_), T::Named(_)) => todo!(),

        (T::Int, T::Named(_)) => todo!(),
        (T::IntLiteral(_), T::Int) => true,
        (T::IntLiteral(a), T::IntLiteral(b)) => a == b,
        (T::IntLiteral(_), T::Named(_)) => todo!(),

        (T::String, T::Named(_)) => todo!(),
        (T::StringLiteral(_), T::String) => true,
        (T::StringLiteral(a), T::StringLiteral(b)) => a == b,

        (T::Named(_), T::Bool) => todo!(),
        (T::Named(_), T::BoolLiteral(_)) => todo!(),
        (T::Named(_), T::Float) => todo!(),
        (T::Named(_), T::FloatLiteral(_)) => todo!(),
        (T::Named(_), T::Int) => todo!(),
        (T::Named(_), T::IntLiteral(_)) => todo!(),
        (T::Named(_), T::String) => todo!(),
        (T::Named(_), T::Named(_)) => todo!(),

        _ => false,
    }
}
