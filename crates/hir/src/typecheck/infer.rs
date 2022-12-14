//!
//! Type inference

use std::cell::RefCell;

use la_arena::Idx;

use super::{Type, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{BinaryExpr, BinaryOp, Context, Expr};

type InferResult = Result<Type, TypeDiagnostic>;

// TODO: need &mut Context to update the type in the context?
// or have a separate typed HIR?
pub(crate) fn infer_expr(expr_idx: Idx<Expr>, context: &RefCell<Context>) -> InferResult {
    let context_ref = context.borrow();
    let expr = context_ref.expr(expr_idx);

    let inferred_result = match expr {
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
        Expr::Binary(expr) => infer_binary(expr, &context.borrow()),
        Expr::Unary(expr) => todo!(),
        Expr::Block(expr) => todo!(),
        Expr::VariableRef { name } => todo!(),
        Expr::Call(expr) => {
            let name = &expr.path;
            let args = &expr.args;
            let len: u32 = args
                .len()
                .try_into()
                .expect("less than 4 billion arguments");

            // Stops/returns the first inference error found
            let mut arg_types = Vec::with_capacity(len as usize);
            for arg in args {
                let arg_type = infer_expr(*arg, context)?;
                arg_types.push(arg_type);
            }
            // let arg_types: Result<Vec<Type>, TypeDiagnostic> =
            //     args.iter().map(|arg| infer_expr(*arg, context)).collect();
            // let arg_types = arg_types?;

            // this is temporary until we have definition files for builtins
            // number of args checking should be generalized
            if name == "print" {
                if len == 0 || len > 1 {
                    return Err(TypeDiagnostic {
                        variant: TypeDiagnosticVariant::ArgsMismatch {
                            name: "print".to_owned(),
                            expected: 1,
                            actual: len,
                        },
                        range: Default::default(),
                    });
                }
                return Ok(Type::Unit);
            }

            Ok(Type::Undetermined)
        }
        Expr::Function {
            params,
            body,
            return_type_annotation,
        } => todo!(),
        Expr::LetBinding(idx) => {
            // let local_def = context.local_def(*idx);
            // let annotation = local_def.type_annotation;
            todo!()
        }
    };

    if let Ok(ref inferred_type) = inferred_result {
        context
            .borrow_mut()
            .set_type_of(expr_idx, inferred_type.clone());
    }

    inferred_result
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
