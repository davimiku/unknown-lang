//!
//! Type inference

use std::collections::HashMap;

use itertools::Itertools;
use la_arena::Idx;
use text_size::TextRange;

use super::builtins::{get_builtin_functions, BuiltinFunctionSignature};
use super::check::check_expr;
use super::{Type, TypeDatabase, TypeDiagnostic, TypeDiagnosticVariant};
use crate::database::Database;
use crate::expr::{
    BinaryExpr, BinaryOp, BlockExpr, Expr, FunctionExpr, FunctionParam, IfExpr, LocalDefExpr,
    LocalRefExpr, LocalRefName, UnaryExpr, UnaryOp,
};
use crate::interner::Key;
use crate::type_expr::{LocalTypeRefExpr, LocalTypeRefName, TypeExpr};
use crate::typecheck::check::is_subtype;
use crate::typecheck::FunctionType;
use crate::CallExpr;

// TODO: needs to have Vec<TypeDiagnostic>
//
// TODO: make a newtype with a monadic bind/chain ("and_then")
// signature that concatenates the internal Vec<TypeDiagnostic> so it can be chained/collected
type InferResult = Result<Type, TypeDiagnostic>;

pub(crate) fn infer_expr(
    expr_idx: Idx<Expr>,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> InferResult {
    if let Some(already_inferred_type) = type_database.expr_types.get(expr_idx) {
        return Ok(already_inferred_type.clone());
    }

    let (expr, range) = database.expr(expr_idx);
    let builtin_signatures = get_builtin_functions();

    let inferred_result = match expr {
        Expr::Empty => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined { name: todo!() },
            range: database.range_of_expr(expr_idx),
        }),

        Expr::BoolLiteral(b) => infer_bool_literal(*b, type_database, expr_idx),
        Expr::FloatLiteral(f) => infer_float_literal(*f, type_database, expr_idx),
        Expr::IntLiteral(i) => infer_int_literal(*i, type_database, expr_idx),
        Expr::StringLiteral(key) => infer_string_literal(*key, type_database, expr_idx),

        Expr::LocalRef(local_ref) => infer_local_ref(local_ref, type_database),

        Expr::Binary(expr) => infer_binary(expr, type_database, database),
        Expr::Unary(expr) => infer_unary(expr, type_database, database),
        Expr::Block(block) => infer_block(expr_idx, block, type_database, database),
        Expr::Call(expr) => infer_call(expr_idx, expr, builtin_signatures, type_database, database),
        Expr::Function(function) => infer_function(function, type_database, database),
        Expr::LocalDef(local_def) => infer_local_def(local_def, type_database, database),
        Expr::If(if_expr) => infer_if_expr(if_expr, type_database, database),
    };

    if let Ok(ref inferred_type) = inferred_result {
        type_database.set_expr_type(expr_idx, inferred_type.clone());
    }

    inferred_result
}

fn infer_type_expr(
    idx: Idx<TypeExpr>,
    type_expr: &TypeExpr,
    type_database: &mut TypeDatabase,
) -> Type {
    let inferred_type = match type_expr {
        TypeExpr::BoolLiteral(b) => Type::BoolLiteral(*b),
        TypeExpr::FloatLiteral(f) => Type::FloatLiteral(*f),
        TypeExpr::IntLiteral(i) => Type::IntLiteral(*i),
        TypeExpr::StringLiteral(s) => Type::StringLiteral(*s),

        TypeExpr::Binary(_) => todo!(),
        TypeExpr::Unary(_) => todo!(),

        TypeExpr::Call(_) => todo!(),
        TypeExpr::LocalRef(local_ref) => {
            let LocalTypeRefExpr { name } = local_ref;
            match name {
                LocalTypeRefName::Resolved(key) => type_database
                    .get_local_type(key)
                    .cloned()
                    .unwrap_or(Type::Error),
                LocalTypeRefName::Unresolved(_) => Type::Error,
            }
        }
        TypeExpr::LocalDef(_) => Type::Error,

        TypeExpr::Empty => Type::Undetermined,
    };
    type_database.set_type_expr_type(idx, inferred_type.clone());

    inferred_type
}

fn infer_local_ref(
    expr: &LocalRefExpr,
    type_database: &mut TypeDatabase,
) -> Result<Type, TypeDiagnostic> {
    let name = expr.name;
    match name {
        LocalRefName::Resolved(key) => type_database
            .local_defs
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
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let FunctionExpr { params, body } = function;

    // TODO: this really needs to collect the Err(s) into a Vec
    let params: Result<Vec<Type>, TypeDiagnostic> = params
        .iter()
        .map(|param| infer_function_param(param, type_database, database))
        .collect();

    let return_ty = infer_expr(*body, type_database, database);

    match (params, return_ty) {
        (Ok(params), Ok(return_ty)) => Ok(Type::Function(FunctionType {
            params,
            return_ty: Box::new(return_ty),
        })),
        (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err),
        (Err(param_err), Err(body_err)) => {
            todo!();
        }
    }
}

fn infer_function_param(
    param: &FunctionParam,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let FunctionParam { name, ty } = param;

    ty.map(|idx| {
        let (type_expr, range) = database.type_expr(idx);
        let ty = infer_type_expr(idx, type_expr, type_database);

        type_database.local_defs.insert(*name, ty.clone());
        ty
    })
    // TODO: parameter types can be omitted when they could be inferred in another way
    // ex.
    // ```
    // type MyFuncType = String -> Int
    // let f: MyFuncType = s -> { /* ... */ }
    // ```
    // here, `s` can be inferred as `String` because we know the type of `f`
    .ok_or(TypeDiagnostic {
        variant: TypeDiagnosticVariant::Undefined { name: *name },
        range: Default::default(),
    })
}

fn infer_local_def(
    local_def: &LocalDefExpr,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let LocalDefExpr {
        key,
        value,
        type_annotation,
    } = local_def;
    if let Some(idx) = type_annotation {
        let (annotation, range) = database.type_expr(*idx);
        let expected = infer_type_expr(*idx, annotation, type_database);
        check_expr(*value, &expected, type_database, database).map(|_| {
            type_database.local_defs.insert(*key, expected.clone()); // TODO: use .tap for side-effect
            expected
        })
    } else {
        infer_expr(*value, type_database, database).map(|inferred| {
            type_database.local_defs.insert(*key, inferred.clone()); // TODO: use .tap for side-effect
            inferred
        })
    }
}

fn infer_block(
    expr_idx: Idx<Expr>,
    block: &BlockExpr,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let expr_types: Result<Vec<Type>, TypeDiagnostic> = block
        .exprs
        .iter()
        .map(|arg| infer_expr(*arg, type_database, database))
        .collect();
    expr_types.map(|expr_types| {
        let tail_type = expr_types.last();
        tail_type.map_or_else(
            || Type::Unit,
            |tail_type| {
                type_database.set_expr_type(expr_idx, tail_type.clone());

                tail_type.clone()
            },
        )
    })
}

fn infer_call(
    expr_idx: Idx<Expr>,
    expr: &CallExpr,
    builtin_signatures: HashMap<&str, Vec<BuiltinFunctionSignature>>,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let CallExpr {
        callee,
        callee_path,
        args,
    } = expr;

    let resolved_callee = type_database
        .local_defs
        .get(callee)
        .ok_or(TypeDiagnostic {
            variant: TypeDiagnosticVariant::UndefinedFunction {
                name: callee_path.to_owned(),
            },
            range: database.range_of_expr(expr_idx),
        })
        .cloned();

    if resolved_callee.is_ok() {
        return resolved_callee;
    }

    // TODO: homogenize this by injecting builtins into the primordial scope
    let builtin_signature = builtin_signatures.get(callee_path.as_str());
    match builtin_signature {
        Some(signatures) => {
            infer_builtin_call(callee_path, args, signatures, type_database, database)
        }
        // TODO: is this possible - will we already check in name resolution?
        None => Err(TypeDiagnostic {
            variant: TypeDiagnosticVariant::Undefined { name: *callee },
            range: Default::default(),
        }),
    }
}

fn infer_builtin_call(
    name: &str,
    call_args: &Vec<Idx<Expr>>,
    overloads: &Vec<BuiltinFunctionSignature>,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> Result<Type, TypeDiagnostic> {
    let call_arg_types = call_args
        .iter()
        .map(|arg| infer_expr(*arg, type_database, database).unwrap())
        .collect_vec();
    for overload in overloads {
        // TODO: special-case for length 1 to provide a better diagnostic?
        if overload.arg_types.len() == call_args.len() {
            let overload_is_match = overload
                .arg_types
                .iter()
                .zip(&call_arg_types)
                .all(|(expected, actual)| is_subtype(actual, expected));

            if overload_is_match {
                return Ok(overload.return_type.clone());
            }
        }
    }
    let name = name.to_owned();
    Err(TypeDiagnostic {
        variant: TypeDiagnosticVariant::NoOverloadFound { name },
        range: Default::default(),
    })
}

fn infer_bool_literal(
    b: bool,
    type_database: &mut TypeDatabase,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::BoolLiteral(b);
    type_database.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_float_literal(
    f: f64,
    type_database: &mut TypeDatabase,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::FloatLiteral(f);
    type_database.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_int_literal(
    i: i32,
    type_database: &mut TypeDatabase,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::IntLiteral(i);
    type_database.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_string_literal(
    key: Key,
    type_database: &mut TypeDatabase,
    expr_idx: Idx<Expr>,
) -> Result<Type, TypeDiagnostic> {
    let inferred_type = Type::StringLiteral(key);
    type_database.set_expr_type(expr_idx, inferred_type.clone());

    Ok(inferred_type)
}

fn infer_if_expr(
    if_expr: &IfExpr,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> InferResult {
    let IfExpr {
        then_branch,
        else_branch,
        ..
    } = if_expr;

    if let Some(else_branch) = else_branch {
        let then_type = infer_expr(*then_branch, type_database, database)?;
        let else_type = infer_expr(*else_branch, type_database, database)?;

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
    type_database: &mut TypeDatabase,
    database: &Database,
) -> InferResult {
    let UnaryExpr {
        op,
        expr: inner_idx,
    } = expr;

    match op {
        UnaryOp::Neg => todo!(),
        UnaryOp::Not => {
            let inner_type = infer_expr(*inner_idx, type_database, database);
            match inner_type {
                Ok(ty) => {
                    check_expr(*inner_idx, &Type::Bool, type_database, database).map(|_| match ty {
                        Type::Bool => Type::Bool,
                        Type::BoolLiteral(b) => Type::BoolLiteral(!b),
                        _ => unreachable!(),
                    })
                }
                Err(diag) => Err(diag),
            }
        }
    }
}

fn infer_binary(
    expr: &BinaryExpr,
    type_database: &mut TypeDatabase,
    database: &Database,
) -> InferResult {
    // TODO: collect both errors, not early return
    let (lhs, lhs_range) = database.expr(expr.lhs);
    let lhs_type = infer_expr(expr.lhs, type_database, database)?;
    let (rhs, rhs_range) = database.expr(expr.rhs);
    let rhs_type = infer_expr(expr.rhs, type_database, database)?;
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
    // /// Asserts that the provided `Result` is `Ok`
    // /// and returns the unwrapped value.
    // macro_rules! assert_ok {
    //     ($value:expr) => {{
    //         assert!($value.is_ok());
    //         $value.unwrap()
    //     }};
    // }

    // /// Asserts that the provided `Result` is `Err`
    // /// and returns the unwrapped error.
    // macro_rules! assert_err {
    //     ($value:expr) => {{
    //         assert!($value.is_err());
    //         $value.unwrap_err()
    //     }};
    // }

    // use la_arena::Idx;

    // use crate::typecheck::{TypeCheckResults, TypeDiagnostic};
    // use crate::{BlockExpr, Context, Expr, Type};

    // use super::{infer_expr, InferResult};

    // fn check(input: &str) -> InferResult {
    //     let parsed = parser::parse(input).syntax();
    //     let root = ast::Root::cast(parsed).expect("valid Root node");
    //     let mut context = Context::default();

    //     let exprs: Vec<Idx<Expr>> = root
    //         .exprs()
    //         .map(|expr| context.lower_expr(Some(expr)))
    //         .collect();

    //     // wrap everything in a block
    //     let root = Expr::Block(BlockExpr { exprs });
    //     let root = context.alloc_expr(root, None);

    //     infer_expr(
    //         root,
    //         &mut TypeCheckResults::new(&context.interner),
    //         &context.database,
    //     )
    // }

    // fn check_infer_type(input: &str, expected: Type) {
    //     let result = check(input);

    //     let actual = assert_ok!(result);
    //     assert_eq!(expected, actual);
    // }

    // fn check_infer_error(input: &str, expected: TypeDiagnostic) {
    //     let result = check(input);

    //     let actual = assert_err!(result);
    //     assert_eq!(expected, actual)
    // }

    // #[test]
    // fn infer_int_literal() {
    //     let input = "1";
    //     let expected = Type::IntLiteral(1);

    //     check_infer_type(input, expected);
    // }

    // #[test]
    // fn infer_let_binding() {
    //     let input = "let a = 1";
    //     let expected = Type::IntLiteral(1);

    //     check_infer_type(input, expected);
    // }
}
