//!
//! Type inference

use std::collections::HashMap;

use la_arena::Idx;
use text_size::TextRange;

use super::builtins::{get_builtin_functions, BuiltinFunctionSignature};
use super::check::check_expr;
use super::{Type, TypeCheckResults, TypeDiagnostic, TypeDiagnosticVariant};
use crate::database::Database;
use crate::expr::{
    BinaryExpr, BinaryOp, BlockExpr, Expr, FunctionExpr, IfExpr, LocalDefExpr, LocalRefExpr,
    LocalRefName, UnaryExpr, UnaryOp,
};
use crate::interner::{Interner, Key};
use crate::type_expr::TypeExpr;
use crate::CallExpr;

// TODO: needs to have Vec<TypeDiagnostic>
//
// TODO: make a newtype with a monadic bind/chain ("and_then")
// signature that concatenates the internal Vec<TypeDiagnostic> so it can be chained/collected
type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(
    expr_idx: Idx<Expr>,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> InferResult {
    if let Some(already_inferred_type) = results.expr_types.get(expr_idx) {
        return Ok(already_inferred_type.clone());
    }

    let (expr, range) = database.expr(expr_idx);
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
        Expr::StringLiteral(key) => infer_string_literal(*key, results, expr_idx),

        Expr::LocalRef(local_ref) => lower_local_ref(local_ref, results),

        Expr::Binary(expr) => infer_binary(expr, results, database, interner),
        Expr::Unary(expr) => infer_unary(expr, results, database, interner),
        Expr::Block(block) => infer_block(expr_idx, block, results, database, interner),
        Expr::Call(expr) => match infer_call(expr, builtin_signatures, results, database, interner)
        {
            Ok(value) => value,
            Err(value) => return value,
        },
        Expr::Function(function) => infer_function(function, results, database, interner),
        Expr::LocalDef(local_def) => infer_local_def(local_def, results, database, interner),
        Expr::If(if_expr) => infer_if_expr(if_expr, results, database, interner),
    };

    if let Ok(ref inferred_type) = inferred_result {
        results.set_expr_type(expr_idx, inferred_type.clone());
    }

    inferred_result
}

fn infer_type_expr(type_expr: &TypeExpr, database: &Database) -> Type {
    match type_expr {
        TypeExpr::BoolLiteral(b) => Type::BoolLiteral(*b),
        TypeExpr::FloatLiteral(f) => Type::FloatLiteral(*f),
        TypeExpr::IntLiteral(i) => Type::IntLiteral(*i),
        TypeExpr::StringLiteral(s) => Type::StringLiteral(*s),

        TypeExpr::Binary(_) => todo!(),
        TypeExpr::Unary(_) => todo!(),

        TypeExpr::LocalRef(local_ref) => {
            todo!()
            // local_ref.name
            // database
        }
        TypeExpr::LocalDef(_) => Type::Error,

        TypeExpr::Empty => Type::Undetermined,
    }
}

fn lower_local_ref(
    expr: &LocalRefExpr,
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

fn infer_function(
    function: &FunctionExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> Result<Type, TypeDiagnostic> {
    let FunctionExpr { params, body } = function;

    Ok(Type::Function {
        params: todo!(),
        return_ty: todo!(),
    })
}

fn infer_local_def(
    local_def: &LocalDefExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> Result<Type, TypeDiagnostic> {
    let LocalDefExpr {
        key,
        value,
        type_annotation,
    } = local_def;
    if let Some(annotation) = type_annotation {
        let (annotation, range) = database.type_expr(*annotation);
        let expected = infer_type_expr(annotation, database);
        check_expr(*value, expected.clone(), results, database, interner).map(|_| expected)
    } else {
        let infer_result = infer_expr(*value, results, database, interner);

        if let Ok(ref ty) = infer_result {
            results.local_types.insert(*key, ty.clone());
        }

        infer_result
    }
}

fn infer_block(
    expr_idx: Idx<Expr>,
    block: &BlockExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> Result<Type, TypeDiagnostic> {
    let expr_types: Result<Vec<Type>, TypeDiagnostic> = block
        .exprs
        .iter()
        .map(|arg| infer_expr(*arg, results, database, interner))
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

fn infer_call(
    expr: &CallExpr,
    builtin_signatures: HashMap<&str, Vec<BuiltinFunctionSignature>>,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> Result<Result<Type, TypeDiagnostic>, Result<Type, TypeDiagnostic>> {
    // TODO: why is this a nested Result?
    let name = &expr.path;
    let args = &expr.args;
    let builtin_signature = builtin_signatures.get(name.as_str());
    match builtin_signature {
        Some(overloads) => {
            for overload in overloads {
                // TODO: special-case for length 1 to provide a better diagnostic?
                if overload.arg_types.len() == args.len() {
                    let overload_is_match =
                        overload
                            .arg_types
                            .iter()
                            .zip(args.iter())
                            .all(|(expected, expr)| {
                                check_expr(*expr, expected.clone(), results, database, interner)
                                    .is_ok()
                            });

                    if overload_is_match {
                        return Err(Ok(overload.return_type.clone()));
                    }
                }
            }
            Err(Err(TypeDiagnostic {
                variant: TypeDiagnosticVariant::NoOverloadFound {
                    name: name.to_owned(),
                },
                range: Default::default(),
            }))
        }
        // TODO: is this possible - will we already check in name resolution?
        None => Err(Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined {
                name: name.to_owned(),
            },
            range: Default::default(),
        })),
    }
}

fn infer_bool_literal(
    b: bool,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::BoolLiteral(b);
    results.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_float_literal(
    f: f64,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::FloatLiteral(f);
    results.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_int_literal(
    i: i32,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::IntLiteral(i);
    results.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_string_literal(
    key: Key,
    results: &mut TypeCheckResults,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::StringLiteral(key);
    results.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_if_expr(
    if_expr: &IfExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> InferResult {
    let IfExpr {
        then_branch,
        else_branch,
        ..
    } = if_expr;

    if let Some(else_branch) = else_branch {
        let then_type = infer_expr(*then_branch, results, database, interner)?;
        let else_type = infer_expr(*else_branch, results, database, interner)?;

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

fn infer_unary(
    expr: &UnaryExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> InferResult {
    let UnaryExpr {
        op,
        expr: inner_idx,
    } = expr;

    match op {
        UnaryOp::Neg => todo!(),
        UnaryOp::Not => {
            let inner_type = infer_expr(*inner_idx, results, database, interner);
            match inner_type {
                Ok(ty) => check_expr(*inner_idx, Type::Bool, results, database, interner).map(
                    |_| match ty {
                        Type::Bool => Type::Bool,
                        Type::BoolLiteral(b) => Type::BoolLiteral(!b),
                        _ => unreachable!(),
                    },
                ),
                Err(diag) => Err(diag),
            }
        }
    }
}

fn infer_binary(
    expr: &BinaryExpr,
    results: &mut TypeCheckResults,
    database: &Database,
    interner: &mut Interner,
) -> InferResult {
    // TODO: collect both errors, not early return
    let (lhs, lhs_range) = database.expr(expr.lhs);
    let lhs_type = infer_expr(expr.lhs, results, database, interner)?;
    let (rhs, rhs_range) = database.expr(expr.rhs);
    let rhs_type = infer_expr(expr.rhs, results, database, interner)?;
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
    use Type as T;
    Ok(match (lhs_type, rhs_type) {
        // TODO: add overflow check
        (T::IntLiteral(a), T::IntLiteral(b)) => T::IntLiteral(a + b),
        (T::IntLiteral(_), T::Int) | (T::Int, T::IntLiteral(_)) | (T::Int, T::Int) => T::Int,
        // TODO: add overflow check
        (T::FloatLiteral(a), T::FloatLiteral(b)) => T::FloatLiteral(a + b),
        (T::FloatLiteral(_), T::Float) | (T::Float, T::FloatLiteral(_)) | (T::Float, T::Float) => {
            T::Float
        }

        _ => {
            return Err(TypeDiagnosticVariant::BinaryMismatch {
                op: BinaryOp::Add,
                lhs: lhs_type.clone(),
                rhs: rhs_type.clone(),
            })
        }
    })
}

fn infer_binary_concat(lhs_type: &Type, rhs_type: &Type) -> Result<Type, TypeDiagnosticVariant> {
    use Type as T;
    Ok(match (lhs_type, rhs_type) {
        (T::StringLiteral(_), T::String)
        | (T::String, T::StringLiteral(_))
        | (T::String, T::String) => T::String,

        // TODO: concat these as a new StringLiteral type (constant folding)
        (T::StringLiteral(a), T::StringLiteral(b)) => T::String,

        _ => {
            return Err(TypeDiagnosticVariant::BinaryMismatch {
                op: BinaryOp::Concat,
                lhs: lhs_type.clone(),
                rhs: rhs_type.clone(),
            })
        }
    })
}

/// Determines the most specific type that is compatible with two types.
///
/// This is mostly to deal with the built-in subtype relationships
/// between literals and their corresponding primitive types.
fn infer_compatible_type(a: &Type, b: &Type) -> Option<Type> {
    use Type as T;
    if a == b {
        return Some(a.clone());
    };

    #[rustfmt::skip]
    let compatible_type = match (a, b) {
        // widen to the primitive type when one is a primitive
        (T::BoolLiteral(_), T::Bool) | (T::Bool, T::BoolLiteral(_)) => T::Bool,
        (T::IntLiteral(_), T::Int) | (T::Int, T::IntLiteral(_)) => T::Int,
        (T::FloatLiteral(_), T::Float) | (T::Float, T::FloatLiteral(_)) => T::Float,
        (T::StringLiteral(_), T::String) | (T::String, T::StringLiteral(_)) => T::String,

        // widen to the primitive type if both are literals and are not equal
        (T::BoolLiteral(a), T::BoolLiteral(b)) => if a == b { T::BoolLiteral(*a) } else { T::Bool },
        (T::IntLiteral(a), T::IntLiteral(b)) => if a == b { T::IntLiteral(*a) } else { T::Int },
        (T::FloatLiteral(a), T::FloatLiteral(b)) => if a == b { T::FloatLiteral(*a) } else { T::Float },
        (T::StringLiteral(a), T::StringLiteral(b)) => if a == b { T::StringLiteral(*a) } else { T::String },

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

        infer_expr(
            root,
            &mut TypeCheckResults::default(),
            &context.database,
            &mut context.interner,
        )
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
