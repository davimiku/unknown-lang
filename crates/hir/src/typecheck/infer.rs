//!
//! Type inference

use super::{Type, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{BinaryExpr, BinaryOp, BlockExpr, Context, Expr, UnaryExpr};

type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(expr: &Expr, context: &mut Context) -> InferResult {
    match expr {
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
        Expr::Binary(expr) => infer_binary(expr, context),
        Expr::Unary(expr) => todo!(),
        Expr::Block(expr) => todo!(),
        Expr::VariableRef { name } => todo!(),
        Expr::Call { path, args } => todo!(),
        Expr::Function {
            params,
            body,
            return_type_annotation,
        } => todo!(),
    }
}

fn infer_binary(expr: &BinaryExpr, context: &Context) -> InferResult {
    let lhs = context.expr(expr.lhs);
    let rhs = context.expr(expr.rhs);
    match expr.op {
        BinaryOp::Add => todo!(),
        BinaryOp::Sub => todo!(),
        BinaryOp::Mul => todo!(),
        BinaryOp::Div => todo!(),
        BinaryOp::Rem => todo!(),
        BinaryOp::Exp => todo!(),
        BinaryOp::Path => todo!(),
    }
}
