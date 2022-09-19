//!
//! Type inference

use super::{Type, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{BinaryExpr, BinaryOp, BlockExpr, Context, Expr, UnaryExpr};

type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer(ast: &Expr, context: &mut Context) -> InferResult {
    match ast {
        Expr::Empty => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined {
                name: "MISSING FIXME".to_string(),
            },
            range: Default::default(),
        }),
        Expr::BoolLiteral(b) => Ok(Type::BoolLiteral(*b)),
        Expr::FloatLiteral(f) => Ok(Type::FloatLiteral(*f)),
        Expr::IntLiteral(i) => Ok(Type::IntLiteral(*i)),
        Expr::StringLiteral(s) => Ok(Type::StringLiteral(s.clone())),
        Expr::Binary(BinaryExpr { op, lhs, rhs }) => {
            infer_binary(*op, context.expr(*lhs), context.expr(*rhs))
        }
        Expr::Unary(UnaryExpr { op, expr }) => todo!(),
        Expr::Block(BlockExpr { stmts }) => todo!(),
        Expr::VariableRef { name } => todo!(),
        Expr::Call { path, args } => todo!(),
        Expr::Function {
            params,
            body,
            return_type_annotation,
        } => todo!(),
    }
}

fn infer_binary(op: BinaryOp, lhs: &Expr, rhs: &Expr) -> InferResult {
    match op {
        BinaryOp::Add => todo!(),
        BinaryOp::Sub => todo!(),
        BinaryOp::Mul => todo!(),
        BinaryOp::Div => todo!(),
        BinaryOp::Rem => todo!(),
        BinaryOp::Exp => todo!(),
        BinaryOp::Path => todo!(),
    }
}
