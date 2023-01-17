//!
//! Type checker
//!
//!

use la_arena::Idx;
use text_size::TextRange;

use super::infer::infer_expr;
use super::{Type, TypeCheckResults, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{Context, Expr};

pub(crate) fn check_expr(
    expr: Idx<Expr>,
    expected: &Type,
    results: &mut TypeCheckResults,
    context: &Context,
) -> Result<(), TypeDiagnostic> {
    let actual = infer_expr(expr, results, context)?;

    if is_subtype(&actual, expected) {
        Ok(())
    } else {
        Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::TypeMismatch {
                expected: expected.clone(),
                actual,
            },
            // TODO: get a real TextRange (from context.database ?)
            range: TextRange::default(),
        })
    }
}

pub(crate) fn check_exprs<I>(
    exprs: I,
    results: &mut TypeCheckResults,
    context: &Context,
) -> Result<(), TypeDiagnostic>
where
    I: IntoIterator<Item = Idx<Expr>>,
{
    let exprs = exprs.into_iter();
    let expr_types = exprs.map(|expr| infer_expr(expr, results, context));

    todo!()
}

/// Is A a subtype of B
///
/// A subtype allows for A to be used in expressions where B is expected.
///
/// For example:
///    FloatLiteral is a subtype of Float
///    If a Float was required, a FloatLiteral would suffice.
fn is_subtype(a: &Type, b: &Type) -> bool {
    use Type::*; // TODO: this shadows std::string::String, decide if the tradeoffs are worth

    if a == b {
        return true;
    }

    match (a, b) {
        (Bool, Named(_)) => todo!(),
        (BoolLiteral(_), Bool) => true,
        (BoolLiteral(a), BoolLiteral(b)) => a == b,
        (BoolLiteral(_), Named(_)) => todo!(),

        (Float, Named(_)) => todo!(),
        (FloatLiteral(_), Float) => true,
        (FloatLiteral(a), FloatLiteral(b)) => a == b,
        (FloatLiteral(_), Named(_)) => todo!(),

        (Int, Named(_)) => todo!(),
        (IntLiteral(_), Int) => true,
        (IntLiteral(a), IntLiteral(b)) => a == b,
        (IntLiteral(_), Named(_)) => todo!(),

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
