//! Type checking
//!
//!

use la_arena::Idx;

use super::infer::infer_expr;
use super::types::{FuncSignature, SumType};
use super::{Type, TypeDiagnostic};
use crate::{Context, Expr, FunctionType};

/// Checks whether the provided expression is a subtype of the expected expression.
///
/// Calls and is called by `infer_expr` (mutual recursion) as an implementation of
/// bidirectional type checking.
pub(crate) fn check_expr(
    expr: Idx<Expr>,
    expected: Idx<Type>,
    context: &mut Context,
) -> Result<(), Vec<TypeDiagnostic>> {
    let infer_result: Result<_, _> = infer_expr(expr, context).into();
    let actual = infer_result?;

    if is_subtype(actual, expected, context) {
        Ok(())
    } else {
        let range = context.database.range_of_expr(expr);
        Err(vec![TypeDiagnostic::mismatch(expected, actual, range)])
    }
}

/// Is A a subtype of B
///
/// A subtype allows for A to be used in expressions where B is expected.
///
/// For example:
///    FloatLiteral is a subtype of Float
///    If a Float was required, a FloatLiteral would suffice.
pub(crate) fn is_subtype(a: Idx<Type>, b: Idx<Type>, context: &Context) -> bool {
    if a == b {
        return true;
    }

    let a = context.type_(a);
    let b = context.type_(b);
    if a == b {
        return true;
    }

    // TODO: consider widening `a` with `widen_to_scalar`, would that simplify the conditions below?
    // would require mutable reference to TypeDatabase
    match (a, b) {
        (_, Type::Top) => true,
        (_, Type::Bottom) => false,

        (Type::FloatLiteral(_), Type::Float) => true,
        (Type::FloatLiteral(a), Type::FloatLiteral(b)) => a == b,

        (Type::IntLiteral(_), Type::Int) => true,
        (Type::IntLiteral(a), Type::IntLiteral(b)) => a == b,

        (Type::StringLiteral(_), Type::String) => true,
        (Type::StringLiteral(a), Type::StringLiteral(b)) => a == b,

        (Type::Array(a), Type::Array(b)) => is_subtype(a.of, b.of, context),

        (Type::Function(a), Type::Function(b)) => is_function_subtype(a, b, context),

        (Type::Sum(a), Type::Sum(b)) => is_sumtype_subtype(a, b, context),

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
/// let return_covariance: Int -> String = (a: Int) -> { "hello" }
/// ```
fn is_function_subtype(a: &FunctionType, b: &FunctionType, context: &Context) -> bool {
    for a_signature in &a.signatures {
        for b_signature in &b.signatures {
            if is_signature_subtype(a_signature, b_signature, context) {
                return true;
            }
        }
    }
    false
}

fn is_signature_subtype(a: &FuncSignature, b: &FuncSignature, context: &Context) -> bool {
    let params_check = a
        .params
        .iter()
        .zip(b.params.iter())
        .all(|(a_param, b_param)| is_subtype(*b_param, *a_param, context));

    let return_check = is_subtype(a.return_ty, b.return_ty, context);

    params_check && return_check
}

fn is_sumtype_subtype(a: &SumType, b: &SumType, context: &Context) -> bool {
    if a.hash == b.hash {
        return true;
    }

    if a.variants.len() != b.variants.len() {
        return false;
    }
    for ((a_key, a_type), (b_key, b_type)) in a.variants.iter().zip(b.variants.iter()) {
        if *a_key != *b_key {
            return false;
        }
        if !is_subtype(*a_type, *b_type, context) {
            return false;
        }
    }

    true
}
