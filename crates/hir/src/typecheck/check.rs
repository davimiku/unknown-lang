//!
//! Type checker
//!
//!

use la_arena::Idx;

use super::infer::infer_expr;
use super::{Type, TypeDatabase, TypeDiagnostic, TypeDiagnosticVariant};
use crate::database::Database;
use crate::Expr;

pub(crate) fn check_expr(
    idx: Idx<Expr>,
    expected: &Type,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<(), TypeDiagnostic> {
    let actual = infer_expr(idx, type_database, database)?;
    if is_subtype(&actual, expected) {
        Ok(())
    } else {
        Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::TypeMismatch {
                expected: expected.clone(),
                actual,
            },
            range: database.range_of_expr(idx),
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
pub(crate) fn is_subtype(a: &Type, b: &Type) -> bool {
    use Type as T;

    if a == b {
        return true;
    }

    match (a, b) {
        (_, T::Top) => true,
        (_, T::Bottom) => false,

        (T::BoolLiteral(_), T::Bool) => true,
        (T::BoolLiteral(a), T::BoolLiteral(b)) => a == b,

        (T::FloatLiteral(_), T::Float) => true,
        (T::FloatLiteral(a), T::FloatLiteral(b)) => a == b,

        (T::IntLiteral(_), T::Int) => true,
        (T::IntLiteral(a), T::IntLiteral(b)) => a == b,

        (T::StringLiteral(_), T::String) => true,
        (T::StringLiteral(a), T::StringLiteral(b)) => a == b,

        _ => false,
    }
}
