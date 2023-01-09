//!
//! Type inference

use la_arena::Idx;

use super::{
    builtins::get_builtin_functions, check::check_expr, Type, TypeCheckResults, TypeDiagnostic,
    TypeDiagnosticVariant,
};
use crate::{BinaryExpr, BinaryOp, Context, Expr};

type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(
    expr_idx: Idx<Expr>,
    results: &mut TypeCheckResults,
    context: &Context,
) -> InferResult {
    let expr = context.expr(expr_idx);

    let builtin_signatures = get_builtin_functions();

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
        Expr::Binary(expr) => infer_binary(expr, results, context),
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
                        tail_type.clone()
                    },
                )
            })
        }
        Expr::VariableRef { name } => todo!(),
        Expr::Call(expr) => {
            let name = &expr.path;
            let args = &expr.args;

            // TODO: clean up this deep nesting
            let builtin_signature = builtin_signatures.get(name.as_str());
            match builtin_signature {
                Some(overloads) => {
                    for overload in overloads {
                        // TODO: special-case for length 1 to provide a better diagnostic?
                        if overload.arg_types.len() == args.len() {
                            let overload_is_match = overload.arg_types.iter().zip(args.iter()).all(
                                |(expected, expr)| {
                                    check_expr(*expr, expected, results, context).is_none()
                                },
                            );

                            if overload_is_match {
                                return Ok(overload.return_type.clone());
                            }
                        }
                    }
                    return Err(TypeDiagnostic {
                        variant: TypeDiagnosticVariant::NoOverloadFound {
                            name: name.to_owned(),
                        },
                        range: Default::default(),
                    });
                }
                // TODO: is this possible - will we already check in name resolution?
                None => {
                    return Err(TypeDiagnostic {
                        variant: TypeDiagnosticVariant::Undefined {
                            name: name.to_owned(),
                        },
                        range: Default::default(),
                    })
                }
            }

            // Ok(Type::Undetermined)
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
        Expr::If(if_expr) => {
            todo!()
        }
    };

    if let Ok(ref inferred_type) = inferred_result {
        results.set_type(expr_idx, inferred_type.clone());
    }

    inferred_result
}

fn infer_binary(
    expr: &BinaryExpr,
    results: &mut TypeCheckResults,
    context: &Context,
) -> InferResult {
    // TODO: collect both errors, not early return
    let lhs = context.expr(expr.lhs);
    let lhs_type = infer_expr(expr.lhs, results, context)?;
    let rhs = context.expr(expr.rhs);
    let rhs_type = infer_expr(expr.rhs, results, context)?;
    match expr.op {
        BinaryOp::Add => {
            // check if it's two things that can be added
            // how to do that? Have a map somewhere?
            todo!()
        }
        BinaryOp::Sub => todo!(),
        BinaryOp::Mul => todo!(),
        BinaryOp::Div => todo!(),
        BinaryOp::Rem => todo!(),
        BinaryOp::Exp => todo!(),
        BinaryOp::Path => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use la_arena::Idx;

    use crate::typecheck::{TypeCheckResults, TypeDiagnostic};
    use crate::{assert_err, assert_ok, BlockExpr, Context, Expr, Type};

    use super::{infer_expr, InferResult};

    fn check(input: &str) -> InferResult {
        let parsed = parser::parse(input).syntax();
        let root = ast::Root::cast(parsed).expect("valid Root node");
        let mut context = Context::new();

        let exprs: Vec<Idx<Expr>> = root
            .exprs()
            .map(|expr| context.lower_expr(Some(expr)))
            .collect();

        // wrap everything in a block
        let root = Expr::Block(BlockExpr { exprs });
        let root = context.alloc_expr(root, None);

        infer_expr(root, &mut TypeCheckResults::default(), &context)
    }

    fn check_infer_type(input: &str, expected: Type) {
        let result = check(input);

        let actual = assert_ok!(result);
        assert_eq!(expected, actual);
    }

    fn check_infer_error(input: &str, expected: TypeDiagnostic) {
        let result = check(input);

        let actual = assert_err!(result);
        assert_eq!(expected, actual)
    }

    #[test]
    fn infer_int_literal() {
        let input = "1";
        let expected = Type::IntLiteral(1);

        check_infer_type(input, expected);
    }
}
