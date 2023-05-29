//!
//! Type checker
//!
//!

use la_arena::Idx;

use super::infer::infer_expr;
use super::{Type, TypeDatabase, TypeDiagnostic, TypeDiagnosticVariant};
use crate::database::Database;
use crate::{Expr, FunctionType};

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
    dbg!(a, b);
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

        (T::Function(a), T::Function(b)) => is_function_subtype(a, b),

        _ => false,
    }
}

/// Is function A a subtype of function B
///
/// `Int -> String` is a subtype of `42 -> String` because parameters are contravariant.
///
/// `Int -> "hello"` is a subtype of `Int -> String` because return types are covariant.
///
/// ```ignore
/// // OK because `42` is a subtype of Int and parameters are contravariant
/// let param_contravariance: 42 -> String = (a: Int) -> {
///     let s: String = "hello"
///     s
/// }
///
/// // OK because `"hello"` is a subtype of String and return types are covariant
/// let return_covariance: Int -> String = (a: Int) -> "hello"
/// ```
fn is_function_subtype(a: &FunctionType, b: &FunctionType) -> bool {
    let params_check = a
        .params
        .iter()
        .zip(b.params.iter())
        .all(|(a_param, b_param)| is_subtype(b_param, a_param));

    let return_check = is_subtype(&a.return_ty, &b.return_ty);

    params_check && return_check
}
