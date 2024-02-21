//! Type inference algorithm
//!
//! Starting with the root expression to infer, matches the variant of
//! the expression and recursively calls infer when necessary. Base cases are
//! for value literals and types that have already been inferred.

use la_arena::Idx;
use text_size::TextRange;

use super::widen::widen_to_scalar;
use super::{check_expr, Type, TypeDiagnostic, TypeDiagnosticVariant, TypeResult};
use crate::expr::{
    ArrayLiteralExpr, BlockExpr, Expr, FunctionExpr, FunctionParam, IfExpr, IndexIntExpr,
    UnaryExpr, UnaryOp, VarDefExpr, VarRefExpr,
};
use crate::interner::Key;
use crate::type_expr::{TypeExpr, TypeRefExpr};
use crate::{ArrayType, CallExpr, Context, FunctionType};

pub(crate) fn infer_expr(expr_idx: Idx<Expr>, context: &mut Context) -> TypeResult {
    if let Some(already_inferred_type) = context.type_database.expr_types.get(expr_idx) {
        return (*already_inferred_type).into();
    }

    let mut result = TypeResult::new(&context.type_database);
    let (expr, range) = context.database.expr(expr_idx);

    let expr = expr.clone();
    let range = *range;

    // each branch should mutate `result`
    // TODO: more ergonomic if branches return a value or is this fine?
    match expr {
        Expr::Empty => {
            result.push_diag(TypeDiagnostic {
                variant: TypeDiagnosticVariant::Empty { expr: expr_idx },
                range,
            });
        }
        Expr::Statement(inner_idx) => {
            result.chain(infer_expr(inner_idx, context));
            result.ty = context.type_database.unit();
        }
        Expr::ReturnStatement(return_value) => {
            result.chain(infer_expr(return_value, context));
            result.ty = context.type_database.bottom();
        }

        Expr::BoolLiteral(b) => result.chain(infer_bool_literal(expr_idx, b, context)),
        Expr::FloatLiteral(f) => result.chain(infer_float_literal(expr_idx, f, context)),
        Expr::IntLiteral(i) => result.chain(infer_int_literal(expr_idx, i, context)),
        Expr::StringLiteral(key) => result.chain(infer_string_literal(expr_idx, key, context)),
        Expr::ArrayLiteral(array_expr) => result.chain(infer_array_literal(&array_expr, context)),

        Expr::VarRef(var_ref) => result.chain(infer_var_ref(&var_ref, context)),
        Expr::UnresolvedVarRef { key } => result.push_diag(TypeDiagnostic {
            variant: TypeDiagnosticVariant::UnresolvedVarRef { key },
            range,
        }),

        Expr::Unary(expr) => result.chain(infer_unary(expr_idx, &expr, context)),
        Expr::Block(block) => result.chain(infer_block(&block, context)),
        Expr::Call(expr) => result.chain(infer_call(expr_idx, &expr, context)),
        Expr::Function(function) => result.chain(infer_function(&function, context)),
        Expr::VarDef(var_def) => result.chain(infer_var_def(&var_def, context)),
        Expr::If(if_expr) => result.chain(infer_if_expr(&if_expr, context)),
        Expr::Path(_) => todo!(),
        Expr::IndexInt(index_expr) => result.chain(infer_index_int_expr(&index_expr, context)),
        Expr::Module(exprs) => {
            for expr in exprs {
                result.chain(infer_expr(expr, context));
            }
            result.ty = context.type_database.top();
        }
        Expr::Intrinsic(_) => unreachable!("intrinsic shouldn't actually be checked/inferred"),
    };

    context.type_database.set_expr_type(expr_idx, result.ty);
    result
}

/// Infers a type, and if it is a literal type, then widen to the
/// corresponding scalar type.
///
/// ex. StringLiteral("hello") is widened to String
fn infer_expr_widened(expr_idx: Idx<Expr>, context: &mut Context) -> TypeResult {
    let result = infer_expr(expr_idx, context);
    if result.is_ok() {
        widen_to_scalar(result.ty, context).into()
    } else {
        result
    }
}

fn infer_type_expr(idx: Idx<TypeExpr>, context: &mut Context) -> TypeResult {
    let (type_expr, _range) = context.database.type_expr(idx);
    let result: TypeResult = match type_expr {
        TypeExpr::BoolLiteral(b) => context
            .type_database
            .alloc_type(Type::BoolLiteral(*b))
            .into(),
        TypeExpr::FloatLiteral(f) => context
            .type_database
            .alloc_type(Type::FloatLiteral(*f))
            .into(),
        TypeExpr::IntLiteral(i) => context
            .type_database
            .alloc_type(Type::IntLiteral(*i))
            .into(),
        TypeExpr::StringLiteral(s) => context
            .type_database
            .alloc_type(Type::StringLiteral(*s))
            .into(),

        TypeExpr::Binary(_) => todo!(),
        TypeExpr::Unary(_) => todo!(),

        TypeExpr::Call(_) => todo!(),
        TypeExpr::VarRef(TypeRefExpr { symbol, .. }) => {
            context.type_database.get_type_symbol(symbol).into()
        }
        TypeExpr::UnresolvedVarRef { key } => {
            println!("unresolved type key '{}'", context.lookup(*key));
            todo!()
        }
        TypeExpr::LocalDef(_) => todo!(),

        TypeExpr::Empty => todo!(),
    };
    context.type_database.set_type_expr_type(idx, result.ty);

    result
}

fn infer_var_ref(expr: &VarRefExpr, context: &Context) -> TypeResult {
    let name = expr.symbol;
    let local_ty = context.type_database.value_symbols.get(&name);

    let mut result = TypeResult::new(&context.type_database);
    if let Some(ty) = local_ty {
        result.ty = *ty;
    } else {
        result.push_diag(TypeDiagnostic {
            variant: TypeDiagnosticVariant::UndefinedSymbol { name },
            range: TextRange::default(),
        });
    }

    result
}

fn infer_function(function: &FunctionExpr, context: &mut Context) -> TypeResult {
    let FunctionExpr { params, body, .. } = function;
    let mut function_type = FunctionType {
        params: vec![],
        return_ty: context.type_database.unknown(),
    };

    let mut result = TypeResult::new(&context.type_database);

    for param in params {
        result.chain(infer_function_param(param, context));
        function_type.params.push(result.ty);
    }

    let return_type = infer_expr(*body, context);
    result.chain(return_type);
    function_type.return_ty = result.ty;

    result.ty = context
        .type_database
        .alloc_type(Type::Function(function_type));
    result
}

fn infer_function_param(param: &FunctionParam, context: &mut Context) -> TypeResult {
    let FunctionParam {
        symbol, annotation, ..
    } = param;

    if let Some(idx) = annotation {
        let param_type = infer_type_expr(*idx, context);
        context
            .type_database
            .value_symbols
            .insert(*symbol, param_type.ty);

        param_type
    } else {
        context.type_database.unknown().into()
    }

    // TODO: parameter types can be omitted when they could be inferred in another way
    // ex.
    // ```
    // type MyFuncType = String -> Int
    // let f: MyFuncType = s -> { /* ... */ }
    // ```
    // here, `s` should be inferred as `String` because we know the type of `f`
    // or is this not handled here, and should be handled in "check"?
}

fn infer_var_def(var_def: &VarDefExpr, context: &mut Context) -> TypeResult {
    let VarDefExpr {
        symbol: key,
        value,
        type_annotation,
        ..
    } = var_def;

    let mut result = TypeResult::new(&context.type_database);

    if let Some(annotation_idx) = type_annotation {
        let expected = infer_type_expr(*annotation_idx, context);

        // FIXME: doesn't work when `expected` itself errors?
        match check_expr(*value, expected.ty, context) {
            Ok(_) => {
                context
                    .type_database
                    .value_symbols
                    .insert(*key, expected.ty);
            }
            Err(mut diagnostics) => result.diagnostics.append(&mut diagnostics),
        }
    } else {
        result.chain(infer_expr(*value, context));
        if result.is_ok() {
            context.type_database.value_symbols.insert(*key, result.ty);
        }
    }
    // The full definition expression still returns Unit no matter what
    result.ty = context.type_database.unit();

    result
}

fn infer_block(block: &BlockExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    match block {
        BlockExpr::Empty => result.ty = context.type_database.unit(),
        BlockExpr::NonEmpty { exprs } => {
            for idx in exprs {
                result.chain(infer_expr(*idx, context));
            }

            // invariant: BlockExpr is guaranteed to have at least one expression
            // empty blocks are lowered as the `EmptyBlock` variant
            let tail_expr = *exprs.last().unwrap();
            let (tail_expr, ..) = context.database.expr(tail_expr);
            result.ty = match tail_expr {
                Expr::Statement(inner) => context.type_database.get_expr_type(*inner),
                Expr::VarDef(_) => context.type_database.unit(),
                e => {
                    dbg!(e);
                    unreachable!();
                }
            }
        }
    };

    result
}

fn infer_call(expr_idx: Idx<Expr>, expr: &CallExpr, context: &mut Context) -> TypeResult {
    let range = context.database.range_of_expr(expr_idx);
    let CallExpr { callee, args, .. } = expr;
    let mut result = TypeResult::new(&context.type_database);

    result.chain(infer_expr(*callee, context));

    if let Type::Function(FunctionType { params, return_ty }) =
        context.type_database.type_(result.ty)
    {
        if args.len() != params.len() {
            result.push_diag(TypeDiagnostic::num_args_mismatch(
                params.len(),
                args.len(),
                range,
            ));
        }
        for (arg, param) in args.iter().zip(params) {
            result.check(*arg, param, context)
        }

        result.ty = return_ty;
    } else {
        result.push_diag(TypeDiagnostic::expected_function(result.ty, range));
        result.ty = context.type_database.error()
    }
    result
}

fn infer_bool_literal(idx: Idx<Expr>, b: bool, context: &mut Context) -> TypeResult {
    let inferred = Type::BoolLiteral(b);
    let inferred = context.type_database.alloc_type(inferred);
    context.type_database.set_expr_type(idx, inferred);

    inferred.into()
}

fn infer_float_literal(idx: Idx<Expr>, f: f64, context: &mut Context) -> TypeResult {
    let inferred = Type::FloatLiteral(f);
    let inferred = context.type_database.alloc_type(inferred);
    context.type_database.set_expr_type(idx, inferred);

    inferred.into()
}

fn infer_int_literal(idx: Idx<Expr>, i: i64, context: &mut Context) -> TypeResult {
    let inferred = Type::IntLiteral(i);
    let inferred = context.type_database.alloc_type(inferred);
    context.type_database.set_expr_type(idx, inferred);

    inferred.into()
}

fn infer_string_literal(idx: Idx<Expr>, key: Key, context: &mut Context) -> TypeResult {
    let inferred = Type::StringLiteral(key);
    let inferred = context.type_database.alloc_type(inferred);
    context.type_database.set_expr_type(idx, inferred);

    inferred.into()
}

/// Infers the type for an ArrayLiteral
///
/// Loops through the items and infers the types for each of those.
/// The first (non-error) inferred type becomes the expected type for
/// the rest of the elements.
fn infer_array_literal(array_expr: &ArrayLiteralExpr, context: &mut Context) -> TypeResult {
    match array_expr {
        ArrayLiteralExpr::Empty => context
            .type_database
            .alloc_type(Type::array_of(context.type_database.bottom()))
            .into(),
        ArrayLiteralExpr::NonEmpty { elements } => {
            let mut result = TypeResult::new(&context.type_database);
            let mut inner_type: Option<Idx<Type>> = None;
            for idx in elements {
                if let Some(ref inner_type) = inner_type {
                    result.check(*idx, *inner_type, context);
                } else {
                    result.chain(infer_expr(*idx, context));
                    if result.is_ok() {
                        inner_type = Some(widen_to_scalar(result.ty, context));
                    }
                }
            }

            let inner = inner_type.unwrap_or(context.type_database.error());

            result.chain(
                context
                    .type_database
                    .alloc_type(Type::array_of(inner))
                    .into(),
            );
            result
        }
    }
}

fn infer_index_int_expr(index_expr: &IndexIntExpr, context: &mut Context) -> TypeResult {
    let IndexIntExpr { subject, .. } = index_expr;

    let mut result = infer_expr(*subject, context);

    if result.is_ok() {
        match context.type_database.type_(result.ty) {
            // TODO: the index expr needs to be an Int also
            Type::Array(ArrayType { of }) => result.ty = of,

            _ => {
                // TODO: add diagnostic
                // TODO: mutate result.ty to error
                todo!()
            }
        }
    }
    result
}

fn infer_if_expr(if_expr: &IfExpr, context: &mut Context) -> TypeResult {
    let IfExpr {
        condition,
        then_branch,
        else_branch,
    } = if_expr;

    let mut result = TypeResult::new(&context.type_database);

    result.check(*condition, context.type_database.bool(), context);

    result.chain(infer_expr_widened(*then_branch, context));

    if let Some(else_branch) = else_branch {
        result.check(*else_branch, result.ty, context);
    } else {
        result.ty = context.type_database.unit();
    }
    result
}

fn infer_unary(expr_idx: Idx<Expr>, expr: &UnaryExpr, context: &mut Context) -> TypeResult {
    let UnaryExpr {
        op,
        expr: inner_idx,
    } = expr;

    let mut result = infer_expr(*inner_idx, context);
    let inner_type = context.type_database.type_(result.ty);

    match op {
        UnaryOp::Neg => todo!(),
        UnaryOp::Not => result.chain(match inner_type {
            Type::BoolLiteral(b) => infer_bool_literal(expr_idx, !b, context),
            Type::Bool => context.type_database.bool().into(),

            _ => TypeResult::from_diag(
                TypeDiagnostic::mismatch(
                    context.type_database.bool(),
                    result.ty,
                    TextRange::default(),
                ),
                context.type_database.error(),
            ),
        }),

        UnaryOp::IntoString => match inner_type {
            Type::Unit
            | Type::BoolLiteral(_)
            | Type::FloatLiteral(_)
            | Type::IntLiteral(_)
            | Type::StringLiteral(_)
            | Type::Bool
            | Type::Float
            | Type::Int
            // | Type::Function(_)
            // | Type::Array(_) 
            | Type::String=> result.ty = context.type_database.string(),

            _ => result.push_diag(TypeDiagnostic {
                variant: TypeDiagnosticVariant::CannotConvertIntoString { actual: result.ty },
                range: TextRange::default(),
        }),
        },
    }
    result
}

#[cfg(test)]
mod tests {

    use la_arena::Idx;

    use crate::typecheck::TypeResult;
    use crate::{BlockExpr, Context, Expr, Interner, Type};

    use super::infer_expr;

    fn check(input: &str) -> (TypeResult, Context) {
        let parsed = parser::parse(input).syntax();
        let root = ast::Root::cast(parsed).expect("valid Root node");
        let mut context = Context::new(Interner::default());

        let exprs: Vec<Idx<Expr>> = root
            .exprs()
            .map(|expr| context.lower_expr_statement(Some(expr)))
            .collect();

        // wrap everything in a block
        let root = Expr::Block(if exprs.is_empty() {
            BlockExpr::Empty
        } else {
            BlockExpr::NonEmpty { exprs }
        });
        let root = context.alloc_expr(root, None);

        let result = infer_expr(root, &mut context);

        (result, context)
    }

    fn check_infer_type(input: &str, expected: Type) {
        let (result, context) = check(input);

        assert!(result.is_ok());
        let actual = context.type_database.type_(result.ty);

        assert_eq!(actual, expected);
    }

    #[test]
    fn infer_int_literal() {
        let input = "1";
        let expected = Type::IntLiteral(1);

        check_infer_type(input, expected);
    }

    #[test]
    fn infer_int_addition() {
        let input = "2 + 3";
        // TODO: is it possible to overload `+` for IntLiteral to return an IntLiteral ?
        let expected = Type::Int;

        check_infer_type(input, expected);
    }

    #[test]
    fn infer_let_binding() {
        let input = "let a = 1";
        let expected = Type::Unit;

        check_infer_type(input, expected);
    }

    #[test]
    fn infer_block() {
        let input = r#"{
    let a = 1
    a
}"#;
        let expected = Type::IntLiteral(1);

        check_infer_type(input, expected);
    }
}
