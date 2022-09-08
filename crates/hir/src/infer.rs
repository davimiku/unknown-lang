//!
//! Type inference

use crate::types::Type;
use crate::{Expr, TypeDiagnostic, TypeDiagnosticVariant};

pub(crate) fn infer(ast: Expr) -> Result<Type, TypeDiagnostic> {
    match ast {
        Expr::Empty => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined {
                name: "MISSING FIXME".to_string(),
            },
            range: Default::default(),
        }),
        Expr::BoolLiteral(b) => Ok(Type::BoolLiteral(b)),
        Expr::FloatLiteral(f) => Ok(Type::FloatLiteral(f)),
        Expr::IntLiteral(i) => Ok(Type::IntLiteral(i)),
        Expr::StringLiteral(s) => Ok(Type::StringLiteral(s)),
        Expr::Binary {
            op,
            lhs,
            rhs,
            lhs_type,
            rhs_type,
        } => todo!(),
        Expr::Unary { op, expr, typ } => todo!(),
        Expr::Block { stmts, typ } => todo!(),
        Expr::VariableRef { name, typ } => todo!(),
        Expr::Call { path, args } => todo!(),
        Expr::Function {
            params,
            body,
            return_type_annotation,
        } => todo!(),
    }
}
