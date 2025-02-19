//! Type inference algorithm
//!
//! Starting with the root expression to infer, matches the variant of
//! the expression and recursively calls infer when necessary. Base cases are
//! for value literals and types that have already been inferred.

use itertools::Itertools;
use la_arena::Idx;
use text_size::TextRange;
use util_macros::assert_matches;

use super::types::FuncSignature;
use super::widen::widen_to_scalar;
use super::{check_expr, Type, TypeDiagnostic, TypeDiagnosticVariant, TypeResult};
use crate::expr::{
    BlockExpr, Expr, FunctionExpr, FunctionExprGroup, FunctionParam, IfExpr, IndexIntExpr,
    ListLiteralExpr, LoopExpr, MatchExpr, ReAssignment, UnaryExpr, UnaryOp, VarDefExpr, VarRefExpr,
};
use crate::interner::Key;
use crate::type_expr::{TypeExpr, TypeRefExpr, TypeVarDefExpr, UnionTypeExpr};
use crate::{ArrayType, CallExpr, Context, ContextDisplay, FunctionType, Module};

pub(crate) fn infer_module(module: &Module, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    for expr in module.exprs.iter() {
        result.chain(infer_expr(*expr, context));
    }
    result.ty = context.core_types().top;
    result
}

pub(crate) fn infer_expr(expr_idx: Idx<Expr>, context: &mut Context) -> TypeResult {
    if let Some(already_inferred_type) = context.type_database.expr_types.get(expr_idx) {
        return (*already_inferred_type).into();
    }

    let mut result = TypeResult::new(&context.type_database);
    let (expr, range) = context.database.expr(expr_idx);

    // clone/copy to remove the shared borrow to be able to mutate `context`
    let expr = expr.clone();
    let range = *range;

    // each branch should mutate `result`
    match expr {
        Expr::Empty => result.ty = context.core_types().unit,

        Expr::Statement(inner_idx) => {
            result.chain(infer_expr(inner_idx, context));
            result.ty = context.core_types().unit;
        }
        Expr::BreakStatement(break_value) => {
            result.chain(infer_expr(break_value, context));
            result.ty = context.core_types().unit;
        }
        Expr::ReturnStatement(return_value) => {
            result.chain(infer_expr(return_value, context));
            result.ty = context.core_types().bottom;
        }
        Expr::TypeStatement(type_expr) => {
            result.chain(infer_type_expr(type_expr, context));
            result.ty = context.core_types().unit;
        }

        Expr::FloatLiteral(f) => result.chain(infer_float_literal(expr_idx, f, context)),
        Expr::IntLiteral(i) => result.chain(infer_int_literal(expr_idx, i, context)),
        Expr::StringLiteral(key) => result.chain(infer_string_literal(expr_idx, key, context)),
        Expr::ListLiteral(array_expr) => result.chain(infer_list_literal(&array_expr, context)),

        Expr::VarRef(var_ref) => result.chain(infer_var_ref(&var_ref, context)),
        Expr::UnresolvedVarRef { key } => result.push_diag(TypeDiagnostic {
            variant: TypeDiagnosticVariant::UnresolvedVarRef { key },
            range,
        }),

        Expr::Unary(unary) => result.chain(infer_unary(expr_idx, &unary, context)),
        Expr::Block(block) => result.chain(infer_block(&block, context)),
        Expr::Call(call) => result.chain(infer_call(expr_idx, &call, context)),
        Expr::Function(function) => result.chain(infer_function(&function, context)),
        Expr::VarDef(var_def) => result.chain(infer_var_def(&var_def, context)),
        Expr::ReAssignment(reassignment) => {
            result.chain(infer_reassignment(&reassignment, context))
        }

        Expr::Match(match_expr) => result.chain(infer_match_expr(&match_expr, context)),
        Expr::If(if_expr) => result.chain(infer_if_expr(&if_expr, context)),
        Expr::Loop(loop_expr) => result.chain(infer_loop_expr(&loop_expr, context)),
        Expr::Path(_) => todo!("typecheck paths"),
        // TODO - this should be just a part of a Path, ex. `>Color<.green` and not type checked itself
        // but could someone do: `type Color = red | green | blue; let some_var = Color` ?
        // what would be the type of some_var ? Or we treat it like a namespace which isn't typed?
        // Or introduce the concept of a namespace type?
        // Or treat it as a record of `( red: Color, green: Color, blue: Color )` when records are implemented?
        Expr::UnionNamespace(_) => todo!(),

        // TODO - this should give a function that accepts parameter(s) and gives back an instance of the union
        Expr::UnionVariant(variant) => {
            todo!("function that takes param(s) and gives an instance of the union type")
        }
        // TODO - Does the UnionUnitVariant get the `Color` type or should that be the Path?
        // this appears to work fine but maybe that's a lie
        Expr::UnionUnitVariant(unit_variant) => {
            let union_namespace = context.expr(unit_variant.union_namespace);
            let union_namespace = assert_matches!(union_namespace, Expr::UnionNamespace);
            result.chain(infer_type_expr(union_namespace.type_expr, context));
        }
        Expr::IndexInt(index_expr) => result.chain(infer_index_int_expr(&index_expr, context)),
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
    if let Some(ty) = context.type_database.type_expr_types.get(idx) {
        return ty.into();
    }
    // TODO: need `range`?
    let type_expr = context.database.type_expr(idx).0.clone();
    use TypeExpr as TE;
    let result: TypeResult = match type_expr {
        TE::FloatLiteral(f) => context
            .type_database
            .alloc_type(Type::FloatLiteral(f))
            .into(),
        TE::IntLiteral(i) => context.type_database.alloc_type(Type::IntLiteral(i)).into(),
        TE::StringLiteral(s) => context
            .type_database
            .alloc_type(Type::StringLiteral(s))
            .into(),

        TE::VarRef(TypeRefExpr { symbol, .. }) => {
            context.type_database.get_type_symbol(&symbol).into()
        }
        TE::UnresolvedVarRef { key } => {
            println!("unresolved type key '{}'", context.lookup(key));
            todo!()
        }
        TE::Unit => context.type_database.core.unit.into(),

        TE::VarDef(type_var_def) => infer_type_var_def(&type_var_def, context),
        TE::Call(_) => todo!(),
        TE::Union(union) => {
            let mut result = TypeResult::new(&context.type_database);

            let mut variants: Vec<(Key, Idx<Type>)> = Vec::with_capacity(union.variants.len());
            for (key, type_expr) in &union.variants {
                let inferred = infer_type_expr(*type_expr, context);
                let ty = inferred.ty;
                result.chain(inferred);
                variants.push((*key, ty));
            }

            result.ty = context.type_database.alloc_type(Type::sum(variants));

            result
        }
        TE::Binary(_) => todo!(),
        TE::Unary(_) => todo!(),

        TE::Empty => todo!(),
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

fn infer_function(function: &FunctionExprGroup, context: &mut Context) -> TypeResult {
    let mut signatures = vec![];

    let mut result = TypeResult::new(&context.type_database);
    for overload in function.overloads.iter() {
        let FunctionExpr {
            params,
            body,
            return_type_annotation,
            captures: _,
        } = overload;

        let param_tys: Box<[Idx<Type>]> = params
            .iter()
            .map(|param| {
                result.chain(infer_function_param(param, context));
                result.ty
            })
            .collect();

        // infer return type from function body
        result.chain(infer_expr(*body, context));

        if let Some(annotation) = return_type_annotation {
            let expected = infer_type_expr(*annotation, context);

            // if we couldn't determine the type for the annotation we can't check
            if expected.is_ok() {
                match check_expr(*body, expected.ty, context) {
                    Ok(()) => result.ty = expected.ty,
                    Err(mut diagnostics) => result.diagnostics.append(&mut diagnostics),
                }
            }
        }

        let return_ty = result.ty;

        signatures.push(FuncSignature {
            params: param_tys,
            return_ty,
        });
    }

    result.ty = context.type_database.alloc_type(Type::func(signatures));
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
        context.core_types().unknown.into()
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
        symbol,
        value,
        type_annotation,
        ..
    } = var_def;

    let mut result = TypeResult::new(&context.type_database);

    if let Some(annotation_idx) = type_annotation {
        let expected = infer_type_expr(*annotation_idx, context);

        // FIXME: doesn't work when `expected` itself errors
        match check_expr(*value, expected.ty, context) {
            Ok(_) => {
                context
                    .type_database
                    .value_symbols
                    .insert(*symbol, expected.ty);
            }
            Err(mut diagnostics) => result.diagnostics.append(&mut diagnostics),
        }
    } else {
        let mut infer_result = infer_expr(*value, context);
        if var_def.is_mutable(context) {
            infer_result.ty = widen_to_scalar(infer_result.ty, context);
            context.type_database.set_expr_type(*value, infer_result.ty);
        }
        result.chain(infer_result);
        if result.is_ok() {
            context
                .type_database
                .value_symbols
                .insert(*symbol, result.ty);
        }
    }
    // The full definition expression still returns Unit no matter what
    result.ty = context.core_types().unit;

    result
}

fn infer_type_var_def(type_var_def: &TypeVarDefExpr, context: &mut Context) -> TypeResult {
    let TypeVarDefExpr { symbol, type_expr } = type_var_def;

    let inferred = infer_type_expr(*type_expr, context);
    context
        .type_database
        .type_symbols
        .insert(*symbol, inferred.ty);

    // TODO for the "type statement" itself, unit or never?
    TypeResult::from_ty(context.core_types().unit)
}

fn infer_union(union: &UnionTypeExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);

    let variants = union
        .variants
        .iter()
        .map(|(key, type_expr)| (*key, infer_type_expr(*type_expr, context)))
        .map(|(key, type_result)| {
            let ty = type_result.ty;
            result.chain(type_result);
            (key, ty)
        })
        .collect_vec();

    result.ty = context.type_database.alloc_type(Type::sum(variants));

    result
}

fn infer_reassignment(reassignment: &ReAssignment, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);

    let place_result = infer_expr(reassignment.place, context);
    let place_ty = place_result.ty;
    result.chain(place_result);

    // FIXME: need to check for if the assign place is mutable
    //

    // FIXME: doesn't work when `expected` itself errors
    match check_expr(reassignment.value, place_ty, context) {
        Ok(_) => {}
        Err(mut diagnostics) => result.diagnostics.append(&mut diagnostics),
    }

    // Reassignment returns Unit no matter what
    result.ty = context.core_types().unit;

    result
}

fn infer_block(block: &BlockExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    match block {
        BlockExpr::Empty => result.ty = context.core_types().unit,
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
                Expr::VarDef(_) => context.core_types().unit,
                Expr::TypeStatement(_) => context.core_types().unit,
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

    let result_type = context.type_(result.ty).clone();
    if let Type::Function(FunctionType { signatures }) = result_type {
        if signatures.len() == 1 {
            let FuncSignature { params, return_ty } = &signatures[0];
            if args.len() != params.len() {
                result.push_diag(TypeDiagnostic::num_args_mismatch(
                    params.len(),
                    args.len(),
                    range,
                ));
            }

            for (arg, param) in args.iter().zip(params.iter()) {
                result.check(*arg, *param, context)
            }

            // regardless of whether the args are correct or not, when there is a single signature
            // it is used so that these type errors remain localized
            result.ty = *return_ty;

            // TODO: the `&CallExpr` passed in here is a clone so mutating does nothing
            // which is why the expr is being pulled out by idx here
            // would we rather hold the resolved signature in a separate arena/structure?
            // could be an ArenaMap of Idx<Expr> -> ResolvedSignature
            let expr_mut = context.expr_mut(expr_idx);
            let call_mut = assert_matches!(expr_mut, Expr::Call);

            call_mut.signature_index = Some(0);

            return result;
        }

        // resolve signature if there are multiple possibilities
        for (i, signature) in signatures.iter().enumerate() {
            let FuncSignature { params, return_ty } = signature;
            // 1. compare number of args vs. params
            if params.len() != args.len() {
                continue;
            };
            // 2. compare the type of each param
            let mut signature_result = TypeResult::new(&context.type_database);
            for (arg, param) in args.iter().zip(params.iter()) {
                signature_result.check(*arg, *param, context)
            }

            if signature_result.is_ok() {
                result.chain(signature_result);
                result.ty = *return_ty;

                // TODO: the `&CallExpr` passed in here is a clone so mutating does nothing
                // which is why the expr is being pulled out by idx here
                // would we rather hold the resolved signature in a separate arena/structure?
                // could be an ArenaMap of Idx<Expr> -> Idx<FuncSignature>
                let expr_mut = context.expr_mut(expr_idx);
                let expr_mut = assert_matches!(expr_mut, Expr::Call);
                expr_mut.signature_index = Some(i as u32);

                return result;
            }
            // type judgements are discarded if not ok, and try the next signature
        }

        result.push_diag(TypeDiagnostic::no_matching_signature(range));

        // TODO: set result.ty to Error? or leave it as Unknown?
    } else {
        result.push_diag(TypeDiagnostic::expected_function(result.ty, range));
        result.ty = context.core_types().error
    }
    result
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

/// Infers the type for an ListLiteral
///
/// Loops through the items and infers the types for each of those.
/// The first (non-error) inferred type becomes the expected type for
/// the rest of the elements.
fn infer_list_literal(array_expr: &ListLiteralExpr, context: &mut Context) -> TypeResult {
    match array_expr {
        ListLiteralExpr::Empty => context
            .type_database
            .alloc_type(Type::array_of(context.core_types().bottom))
            .into(),
        ListLiteralExpr::NonEmpty { elements } => {
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

            let inner = inner_type.unwrap_or(context.core_types().error);

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
        match context.type_(result.ty) {
            // TODO: the index expr needs to be an Int also
            Type::Array(ArrayType { of }) => result.ty = *of,

            _ => {
                // TODO: add diagnostic
                // TODO: mutate result.ty to error
                todo!()
            }
        }
    }
    result
}

fn infer_match_expr(match_expr: &MatchExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);

    result.chain(infer_expr(match_expr.scrutinee, context));

    let mut overall_ty: Option<Idx<Type>> = None;
    for arm in &match_expr.arms {
        let arm_ty = infer_expr(arm.expr, context);
        if arm_ty.is_ok() && overall_ty.is_none() {
            overall_ty = Some(arm_ty.ty);
        }
        result.chain(arm_ty);
    }

    match overall_ty {
        Some(ty) => result.ty = ty,
        None => result.ty = context.core_types().error,
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

    result.check(*condition, context.core_types().bool, context);

    result.chain(infer_expr_widened(*then_branch, context));

    if let Some(else_branch) = else_branch {
        result.check(*else_branch, result.ty, context);
    } else {
        result.ty = context.core_types().unit;
    }
    result
}

fn infer_loop_expr(loop_expr: &LoopExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    result.chain(infer_expr(loop_expr.body, context));

    // the *body* of a loop is Unit regardless of what happens inside. The overall
    // loop type is determined by the `break` expression(s)
    context
        .type_database
        .set_expr_type(loop_expr.body, context.core_types().unit);

    if loop_expr.breaks.is_empty() {
        result.ty = context.core_types().bottom;
        return result;
    }

    let mut first_successful: Option<Idx<Type>> = None;

    for break_expr in loop_expr.breaks.iter() {
        match first_successful {
            Some(expected) => result.check(*break_expr, expected, context),

            None => {
                let infer_result = infer_expr(*break_expr, context);
                match infer_result.is_ok() {
                    true => first_successful = Some(infer_result.ty),
                    false => result.chain(infer_result),
                }
            }
        }
    }

    match first_successful {
        Some(ty) => result.ty = ty,
        None => result.ty = context.core_types().error,
    }

    result
}

fn infer_unary(expr_idx: Idx<Expr>, expr: &UnaryExpr, context: &mut Context) -> TypeResult {
    let UnaryExpr {
        op,
        expr: inner_idx,
    } = expr;

    let mut result = infer_expr(*inner_idx, context);
    let inner_type = context.type_(result.ty);

    match op {
        UnaryOp::Neg => todo!(),
        UnaryOp::Not => {
            let bool_ty = context.core_types().bool;
            result.chain(match result.ty == bool_ty {
                true => bool_ty.into(),
                false => TypeResult::from_diag(
                    TypeDiagnostic::mismatch(bool_ty, result.ty, TextRange::default()),
                    context.core_types().error,
                ),
            })
        }

        UnaryOp::IntoString => match inner_type {
            Type::Unit
            | Type::FloatLiteral(_)
            | Type::IntLiteral(_)
            | Type::StringLiteral(_)
            | Type::Float
            | Type::Int
            // | Type::Function(_)
            // | Type::Array(_) 
            | Type::String=> result.ty = context.core_types().string,

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

    fn check(input: &str, context: &mut Context) -> TypeResult {
        let parse = parser::parse(input);
        if !parse.errors().is_empty() {
            dbg!(parse.errors());
            assert!(parse.errors().is_empty());
        }

        let syntax = parse.syntax();
        let root = ast::Root::cast(syntax).expect("valid Root node");

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

        infer_expr(root, context)
    }

    fn check_infer_type(input: &str, expected: &Type) {
        let mut context = Context::new(Interner::default());

        let result = check(input, &mut context);

        assert!(result.is_ok());
        let actual = context.type_(result.ty);

        assert_eq!(actual, expected);
    }

    fn check_with_context(input: &str, expected: &Type, context: &mut Context) {
        let result = check(input, context);

        assert!(result.is_ok());
        let actual = context.type_(result.ty);

        assert_eq!(actual, expected);
    }

    #[test]
    fn infer_int_literal() {
        let input = "1";
        let expected = Type::IntLiteral(1);

        check_infer_type(input, &expected);
    }

    #[test]
    fn infer_int_addition() {
        let input = "2 + 3";
        let expected = Type::Int;

        check_infer_type(input, &expected);
    }

    #[test]
    fn infer_unit() {
        let input = "()";
        let expected = Type::Unit;

        check_infer_type(input, &expected);
    }

    #[test]
    fn infer_let_binding() {
        let input = "let a = 1";
        let expected = Type::Unit; // the let binding itself is Unit

        check_infer_type(input, &expected);
    }

    #[test]
    fn infer_block() {
        let input = "{
    let a = 1
    a
}";
        let expected = Type::IntLiteral(1);

        check_infer_type(input, &expected);
    }

    #[test]
    fn infer_union() {
        let mut context = Context::new(Interner::default());
        let red = context.interner.intern("red");
        let green = context.interner.intern("green");
        let blue = context.interner.intern("blue");

        let input = "{
        type Color = (red | green | blue)

        Color.green
}";

        let expected = Type::sum(vec![
            (red, context.core_types().unit),
            (green, context.core_types().unit),
            (blue, context.core_types().unit),
        ]);

        check_with_context(input, &expected, &mut context);
    }
}
