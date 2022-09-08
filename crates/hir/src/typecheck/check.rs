//!
//! Type checker
//!
//!

use crate::Expr;

pub(crate) fn check(input: Expr, expected: Type) -> Option<TypeDiagnostic> {
    todo!()
}

/// Is A a subtype of B
fn is_subtype(a: Type, b: Type) -> bool {
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
