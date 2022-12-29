//!
//! Type inference

use la_arena::Idx;

use super::{Type, TypeCheckResults, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{BinaryExpr, BinaryOp, Context, Expr};

type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(
    expr_idx: Idx<Expr>,
    results: &mut TypeCheckResults,
    context: &Context,
) -> InferResult {
    let expr = context.expr(expr_idx);

    let inferred_result = match expr {
        Expr::Empty => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined {
                name: "MISSING FIXME".to_string(),
            },
            range: Default::default(),
        }),
        Expr::BoolLiteral(b) => Ok(Type::BoolLiteral(*b)),
        Expr::FloatLiteral(f) => Ok(Type::FloatLiteral(*f)),
        Expr::IntLiteral(i) => {
            let inferred_type = Type::IntLiteral(*i);
            results.set_type(expr_idx, inferred_type.clone());
            Ok(inferred_type)
        }
        Expr::StringLiteral(s) => Ok(Type::StringLiteral(s.clone())),
        Expr::Binary(expr) => infer_binary(expr, context),
        Expr::Unary(expr) => todo!(),
        Expr::Block(block) => {
            let expr_types: Result<Vec<Type>, TypeDiagnostic> = block
                .exprs
                .iter()
                .map(|arg| infer_expr(*arg, results, context))
                .collect();

            // TODO: clean this up?
            // logic: if expr_types is OK, set the type of this block to the tail_expr type, or Unit if
            // the block is empty
            expr_types.map(|expr_types| {
                let tail_type = expr_types.last();
                tail_type.map_or_else(
                    || Type::Unit,
                    |tail_type| {
                        results.set_type(expr_idx, tail_type.clone());
                        Type::Unit
                    },
                )
            })
        }
        Expr::VariableRef { name } => todo!(),
        Expr::Call(expr) => {
            let name = &expr.path;
            let args = &expr.args;
            let len: u32 = args
                .len()
                .try_into()
                .expect("less than 4 billion arguments");

            // Stops/returns the first inference error found
            let arg_types: Result<Vec<Type>, TypeDiagnostic> = args
                .iter()
                .map(|arg| infer_expr(*arg, results, context))
                .collect();
            let arg_types = arg_types?;

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
        results.set_type(expr_idx, inferred_type.clone());
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
