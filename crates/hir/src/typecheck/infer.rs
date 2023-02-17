//!
//! Type inference

use la_arena::Idx;
use text_size::TextRange;

use super::builtins::get_builtin_functions;
use super::check::check_expr;
use super::{Type, TypeCheckResults, TypeDiagnostic, TypeDiagnosticVariant};
use crate::{
    BinaryExpr, BinaryOp, BlockExpr, Context, Expr, FunctionExpr, IfExpr, LocalDef, LocalRef,
    LocalRefName,
};

// TODO: needs to have Vec<TypeDiagnostic>
//
// TODO: needs to be a newtype with a monadic bind ("and_then")
// signature that concatenates the Vec<TypeDiagnostic> so it can be chained/collected
type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(
    expr_idx: Idx<Expr>,
    results: &mut TypeCheckResults,
    context: &Context,
) -> InferResult {
    if let Some(already_inferred_type) = results.expr_types.get(expr_idx) {
        return Ok(already_inferred_type.clone());
    }

    let expr = context.expr(expr_idx);
    let builtin_signatures = get_builtin_functions();

    let inferred_result = match expr {
        Expr::Empty => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined {
                name: "MISSING FIXME".to_string(),
            },
            range: Default::default(),
        }),
        Expr::BoolLiteral(b) => infer_bool_literal(*b, results, expr_idx),
        Expr::FloatLiteral(f) => infer_float_literal(*f, results, expr_idx),
        Expr::IntLiteral(i) => infer_int_literal(*i, results, expr_idx),
        // TODO: intern the string in both Expr and Type, to copy the intern key instead of cloning the string
        Expr::StringLiteral(s) => infer_string_literal(s.clone(), results, expr_idx),
        Expr::Binary(expr) => infer_binary(expr, results, context),
        Expr::Unary(expr) => todo!(),
        Expr::Block(block) => infer_block(block, results, context, expr_idx),
        Expr::LocalRef(local_ref) => lower_local_ref(local_ref, results),
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
                                    check_expr(*expr, expected, results, context).is_ok()
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
        Expr::Function(FunctionExpr {
            params,
            body,
            return_type_annotation,
        }) => todo!(),
        Expr::LocalDef(local_def) => infer_local_def(local_def, results, context),
        Expr::If(if_expr) => infer_if_expr(if_expr, results, context),
    };

    if let Ok(ref inferred_type) = inferred_result {
        results.set_expr_type(expr_idx, inferred_type.clone());
    }

    inferred_result
}

fn lower_local_ref(
    expr: &LocalRef,
    results: &mut TypeCheckResults,
) -> Result<Type, TypeDiagnostic> {
    let name = expr.name;
    match name {
        LocalRefName::Resolved(key) => results
            .local_types
            .get(&key)
            .ok_or(TypeDiagnostic {
                variant: TypeDiagnosticVariant::UndefinedLocal { name },
                range: TextRange::default(),
            })
            .cloned(),
        LocalRefName::Unresolved(name) => todo!(),
    }
}

fn infer_local_def(
    local_def: &LocalDef,
    results: &mut TypeCheckResults,
    context: &Context,
) -> Result<Type, TypeDiagnostic> {
    let LocalDef {
        key,
        value,
        type_annotation,
    } = local_def;
    if let Some(annotation) = type_annotation {
        let annotation = context.expr(*annotation);
        todo!();
        // check_expr(*value, todo!(), results, context);
    } else {
        let infer_result = infer_expr(*value, results, context);

        if let Ok(ref ty) = infer_result {
            results.local_types.insert(*key, ty.clone());
        }

        infer_result
    }
}

fn infer_block(
    block: &BlockExpr,
    results: &mut TypeCheckResults,
    context: &Context,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let expr_types: Result<Vec<Type>, TypeDiagnostic> = block
        .exprs
        .iter()
        .map(|arg| infer_expr(*arg, results, context))
        .collect();
    expr_types.map(|expr_types| {
        let tail_type = expr_types.last();
        tail_type.map_or_else(
            || Type::Unit,
            |tail_type| {
                results.set_expr_type(expr_idx, tail_type.clone());
                tail_type.clone()
            },
        )
    })
}

fn infer_bool_literal(
    b: bool,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::BoolLiteral(b);
    // TODO: remove clone() when Type is Copy
    results.set_expr_type(expr_idx, inferred_type.clone());
    Ok(inferred_type)
}

fn infer_float_literal(
    f: f64,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::FloatLiteral(f);
    // TODO: remove clone() when Type is Copy
    results.set_expr_type(expr_idx, inferred_type.clone());
    Ok(inferred_type)
}

fn infer_int_literal(
    i: i32,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::IntLiteral(i);
    // TODO: remove clone() when Type is Copy
    results.set_expr_type(expr_idx, inferred_type.clone());
    Ok(inferred_type)
}

fn infer_string_literal(
    s: String,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::StringLiteral(s);
    // TODO: remove clone() when Type is Copy
    results.set_expr_type(expr_idx, inferred_type.clone());
    Ok(inferred_type)
}

fn infer_if_expr(
    if_expr: &IfExpr,
    results: &mut TypeCheckResults,
    context: &Context,
) -> InferResult {
    let IfExpr {
        then_branch,
        else_branch,
        ..
    } = if_expr;

    if let Some(else_branch) = else_branch {
        let then_type = infer_expr(*then_branch, results, context)?;
        let else_type = infer_expr(*else_branch, results, context)?;

        infer_compatible_type(&then_type, &else_type).ok_or(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Incompatible {
                a: then_type,
                b: else_type,
            },
            range: Default::default(),
        })
    } else {
        Ok(Type::Unit)
    }
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
        BinaryOp::Add => infer_binary_add(&lhs_type, &rhs_type),
        BinaryOp::Concat => infer_binary_concat(&lhs_type, &rhs_type),
        BinaryOp::Sub => todo!(),
        BinaryOp::Mul => todo!(),
        BinaryOp::Div => todo!(),
        BinaryOp::Rem => todo!(),
        BinaryOp::Exp => todo!(),
        BinaryOp::Path => todo!(),
    }
    .map_err(|variant| TypeDiagnostic {
        variant,
        range: TextRange::default(),
    })
}

fn infer_binary_add(lhs_type: &Type, rhs_type: &Type) -> Result<Type, TypeDiagnosticVariant> {
    use Type::*; // TODO: this shadows std::string::String, decide if the tradeoffs are worth
    Ok(match (lhs_type, rhs_type) {
        (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b),
        (IntLiteral(_), Int) | (Int, IntLiteral(_)) | (Int, Int) => Int,
        (FloatLiteral(_), Float) | (Float, FloatLiteral(_)) | (Float, Float) => Float,

        _ => {
            return Err(TypeDiagnosticVariant::BinaryMismatch {
                op: BinaryOp::Add,
                lhs: lhs_type.clone(),
                rhs: lhs_type.clone(),
            })
        }
    })
}

fn infer_binary_concat(lhs_type: &Type, rhs_type: &Type) -> Result<Type, TypeDiagnosticVariant> {
    use Type::*; // TODO: this shadows std::string::String, decide if the tradeoffs are worth
    Ok(match (lhs_type, rhs_type) {
        (StringLiteral(_), String) | (String, StringLiteral(_)) | (String, String) => String,
        // TODO: constant folding before type checking
        (StringLiteral(a), StringLiteral(b)) => String,

        _ => {
            return Err(TypeDiagnosticVariant::BinaryMismatch {
                op: BinaryOp::Concat,
                lhs: lhs_type.clone(),
                rhs: lhs_type.clone(),
            })
        }
    })
}

/// Determines the most specific type that is compatible with two types.
///
/// This is mostly to deal with the built-in subtype relationships
/// between literals and their corresponding primitive types.
fn infer_compatible_type(a: &Type, b: &Type) -> Option<Type> {
    use Type::*; // TODO: this shadows std::string::String, decide if the tradeoffs are worth
    if a == b {
        return Some(a.clone());
    };

    #[rustfmt::skip]
    let compatible_type = match (a, b) {
        // widen to the primitive type when one is a primitive
        (BoolLiteral(_), Bool) | (Bool, BoolLiteral(_)) => Bool,
        (IntLiteral(_), Int) | (Int, IntLiteral(_)) => Int,
        (FloatLiteral(_), Float) | (Float, FloatLiteral(_)) => Float,
        (StringLiteral(_), String) | (String, StringLiteral(_)) => String,

        // widen to the primitive type if both are literals and are not equal
        (BoolLiteral(a), BoolLiteral(b)) => if a == b { BoolLiteral(*a) } else { Bool },
        (IntLiteral(a), IntLiteral(b)) => if a == b { IntLiteral(*a) } else { Int },
        (FloatLiteral(a), FloatLiteral(b)) => if a == b { FloatLiteral(*a) } else { Float },
        (StringLiteral(a), StringLiteral(b)) => if a == b { StringLiteral(a.clone()) } else { String },

        (_, _) => return None,
    };

    Some(compatible_type)
}

#[cfg(test)]
mod tests {
    /// Asserts that the provided `Result` is `Ok`
    /// and returns the unwrapped value.
    macro_rules! assert_ok {
        ($value:expr) => {{
            assert!($value.is_ok());
            $value.unwrap()
        }};
    }

    /// Asserts that the provided `Result` is `Err`
    /// and returns the unwrapped error.
    macro_rules! assert_err {
        ($value:expr) => {{
            assert!($value.is_err());
            $value.unwrap_err()
        }};
    }

    use la_arena::Idx;

    use crate::typecheck::{TypeCheckResults, TypeDiagnostic};
    use crate::{BlockExpr, Context, Expr, Type};

    use super::{infer_expr, InferResult};

    fn check(input: &str) -> InferResult {
        let parsed = parser::parse(input).syntax();
        let root = ast::Root::cast(parsed).expect("valid Root node");
        let mut context = Context::default();

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

    #[test]
    fn infer_let_binding() {
        let input = "let a = 1";
        let expected = Type::IntLiteral(1);

        check_infer_type(input, expected);
    }
}
