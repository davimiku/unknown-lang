//!
//! Type checker
//!
//!

use la_arena::Idx;
use text_size::TextRange;

use super::infer::infer_expr;
use super::{Type, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{Context, Expr, Stmt};

pub(crate) fn check_stmt(idx: Idx<Stmt>, context: &Context) -> Option<TypeDiagnostic> {
    let stmt = context.stmt(idx);

    match stmt {
        Stmt::VariableDef(idx) => {
            let local_def = context.local_def(*idx);
            let annotation = local_def.type_annotation;
            todo!()
        }
        Stmt::Expr(idx) => {
            let expr = context.expr(*idx);
        }
    }

    todo!()
}

pub(crate) fn check_expr(
    expr: &Expr,
    expected: Type,
    context: &mut Context,
) -> Option<TypeDiagnostic> {
    let result = infer_expr(expr, context);

    match result {
        Ok(actual) => {
            if is_subtype(&actual, &expected) {
                None
            } else {
                Some(TypeDiagnostic {
                    variant: TypeDiagnosticVariant::TypeMismatch { expected, actual },
                    // TODO: get a real TextRange (from context.database ?)
                    range: TextRange::default(),
                })
            }
        }
        Err(diag) => Some(diag),
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
    use Type::*;
    match (a, b) {
        (Bool, Bool) => true,
        (Bool, Named(_)) => todo!(),

        (BoolLiteral(_), Bool) => true,
        (BoolLiteral(a), BoolLiteral(b)) => a == b,
        (BoolLiteral(_), Named(_)) => todo!(),

        (Float, Float) => true,
        (Float, Named(_)) => todo!(),

        (FloatLiteral(_), Float) => true,
        (FloatLiteral(a), FloatLiteral(b)) => a == b,
        (FloatLiteral(_), Named(_)) => todo!(),

        (Int, Int) => true,
        (Int, Named(_)) => todo!(),

        (IntLiteral(_), Int) => true,
        (IntLiteral(a), IntLiteral(b)) => a == b,
        (IntLiteral(_), Named(_)) => todo!(),

        (String, String) => true,
        (String, Named(_)) => todo!(),

        (StringLiteral(_), String) => true,
        (StringLiteral(a), StringLiteral(b)) => a == b,

        (Named(_), Bool) => todo!(),
        (Named(_), BoolLiteral(_)) => todo!(),
        (Named(_), Float) => todo!(),
        (Named(_), FloatLiteral(_)) => todo!(),
        (Named(_), Int) => todo!(),
        (Named(_), IntLiteral(_)) => todo!(),
        (Named(_), String) => todo!(),
        (Named(_), Named(_)) => todo!(),

        _ => false,
    }
}
