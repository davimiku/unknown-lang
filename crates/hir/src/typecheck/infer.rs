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
    LoopExpr, MatchExpr, PathExpr, ReAssignment, RecordLiteralExpr, UnaryExpr, UnaryOp, VarDefExpr,
    VarRefExpr,
};
use crate::interner::Key;
use crate::intrinsics::{FLOAT, INT, LIST, STRING};
use crate::type_expr::{TypeExpr, TypeRefExpr, TypeVarDefExpr};
use crate::typecheck::types::UnionNamespaceType;
use crate::{CallExpr, Context, FunctionType, ListType, Module, Pattern};

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
        Expr::Intrinsic(symbol) => {
            let builtin = context
                .database
                .value_builtins
                .get(&symbol)
                .copied()
                .expect("intrinsic expression to have a builtin id");
            result.ty = context
                .type_database
                .alloc_type(crate::builtins::signature(builtin, context.core_types()));
        }

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
        Expr::Path(path_expr) => result.chain(infer_path_expr(expr_idx, &path_expr, context)),
        Expr::PathSegment(path_segment) => {
            todo!("determine if this should never be typed-checked on its own (only within a Path)")
        }
        // TODO - this should be just a part of a Path, ex. `>Color<.green` and not type checked itself
        // but could someone do: `type Color = red | green | blue; let some_var = Color` ?
        // what would be the type of some_var ? Or we treat it like a namespace which isn't typed?
        // Or introduce the concept of a namespace type?
        // Or treat it as a record of `[ red: Color, green: Color, blue: Color ]` when records are implemented?
        Expr::UnionNamespace(union_namespace) => {
            result.ty = context
                .type_database
                .get_type_from_valuesymbol(&union_namespace.name);
        }

        Expr::UnionVariant(variant) => {
            let union_namespace =
                assert_matches!(context.expr(variant.union_namespace), Expr::UnionNamespace)
                    .clone();
            let (variant_key, variant_ty_expr) = union_namespace.variants[variant.index as usize];
            let variant_key = {
                let union_namespace_key = context.database.value_names[&union_namespace.name];
                let union_name = context.lookup(union_namespace_key);
                let variant_name = context.lookup(variant_key);
                context
                    .interner
                    .intern(&format!("{union_name}.{variant_name}"))
            };
            let union_type_expr = union_namespace.type_expr;
            result.chain(infer_type_expr(variant_ty_expr, context));
            let variant_ty = result.ty;
            result.chain(infer_type_expr(union_type_expr, context));
            let union_ty = result.ty;

            let signature = FuncSignature {
                params: Box::new([variant_ty]),
                return_ty: union_ty,
            };

            result.ty = context.type_database.alloc_type(Type::union_variant_func(
                signature,
                (variant_ty, variant.index, variant_key),
            ));
        }
        Expr::UnionUnitVariant(unit_variant) => {
            let union_namespace = context.expr(unit_variant.union_namespace);
            let union_namespace = assert_matches!(union_namespace, Expr::UnionNamespace);
            result.chain(infer_type_expr(union_namespace.type_expr, context));
        }
        Expr::RecordLiteral(record_expr) => {
            let fields = record_expr
                .fields
                .iter()
                .map(|(key, expr)| {
                    let field_ty_result = infer_expr(*expr, context);
                    let field_ty = field_ty_result.ty;
                    result.chain(field_ty_result);
                    (*key, field_ty)
                })
                .collect_vec();
            let ty = context
                .type_database
                .alloc_type(Type::product(fields.into(), None, context));
            result.ty = ty;
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

        TE::VarRef(TypeRefExpr { symbol, .. }) => context
            .type_database
            .get_type_from_typesymbol(&symbol)
            .into(),
        TE::UnresolvedVarRef { key } => {
            // TODO: Generic declaration parameters currently have no substitution environment
            // Keep them type-checkable as top until generic aliases are implemented
            if context.lookup(key).starts_with('\'') {
                context.type_database.core.top.into()
            } else {
                println!("unresolved type key '{}'", context.lookup(key));
                todo!()
            }
        }
        TE::Intrinsic(symbol) => {
            let name = context.lookup(context.database.type_names[&symbol]);
            let ty = match name {
                INT => context.core_types().int,
                FLOAT => context.core_types().float,
                STRING => context.core_types().string,
                LIST => context.type_database.alloc_type(Type::List(ListType {
                    of: context.core_types().unknown,
                })),
                _ => context.core_types().top,
            };
            ty.into()
        }
        TE::Unit => context.type_database.core.unit.into(),

        TE::VarDef(type_var_def) => infer_type_var_def(&type_var_def, context),
        TE::Call(call) => infer_type_call(&call, context),
        TE::Union(union) => {
            let mut result = TypeResult::new(&context.type_database);

            let mut variants: Vec<(Key, Idx<Type>)> = Vec::with_capacity(union.variants.len());
            for (key, type_expr) in &union.variants {
                let inferred = infer_type_expr(*type_expr, context);
                let ty = inferred.ty;
                result.chain(inferred);
                variants.push((*key, ty));
            }

            let sum_type = context.type_database.alloc_type(Type::sum(
                variants.clone().into(),
                union.name,
                None,
            ));
            result.ty = sum_type;

            if let Some(value_symbol) = union.namespace_symbol {
                let variant_constructors = variants
                    .iter()
                    .enumerate()
                    .map(|(i, (key, variant_ty))| {
                        let constructor_ty = if *variant_ty == context.core_types().unit {
                            sum_type
                        } else {
                            let func = FunctionType {
                                signatures: vec![FuncSignature {
                                    params: Box::new([*variant_ty]),
                                    return_ty: sum_type,
                                }],
                                variant: Some((*variant_ty, i.into(), *key)),
                            };
                            context.type_database.alloc_type(Type::Function(func))
                        };
                        (*key, (i as u32, constructor_ty))
                    })
                    .collect();
                let ty = UnionNamespaceType {
                    name: value_symbol,
                    variant_constructors,
                    associated_sum_type: sum_type,
                };
                let ty_idx = context.type_database.alloc_type(Type::UnionNamespace(ty));
                context
                    .type_database
                    .insert_value_symbol(value_symbol, ty_idx);
            }

            result
        }
        TE::Record(record) => {
            let mut result = TypeResult::new(&context.type_database);

            let mut fields: Vec<(Key, Idx<Type>)> = Vec::with_capacity(record.fields.len());
            for (key, type_expr) in &record.fields {
                let inferred = infer_type_expr(*type_expr, context);
                let ty = inferred.ty;
                result.chain(inferred);
                fields.push((*key, ty));
            }

            result.ty = context.type_database.alloc_type(Type::product(
                fields.into(),
                record.name,
                context,
            ));

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

        // Bind the annotated type before checking the initializer. This is
        // important for compiler-provided constructors such as `list_new`,
        // whose result starts as `List Unknown` and is refined by context.
        context
            .type_database
            .value_symbols
            .insert(*symbol, expected.ty);

        // FIXME: doesn't work when `expected` itself errors
        match check_expr(*value, expected.ty, context) {
            Ok(_) => context.type_database.set_expr_type(*value, expected.ty),
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
            if matches!(context.type_(result.ty), Type::List(list) if list.of == context.core_types().unknown)
            {
                result.push_diag(TypeDiagnostic::list_element_type_unknown(
                    context.database.range_of_expr(*value),
                ));
                result.ty = context.core_types().error;
            }
        }
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
                Expr::Statement(inner) => infer_expr(*inner, context).ty,
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
    let builtin_id = match context.expr(*callee) {
        Expr::VarRef(var_ref) => context.builtin_id(var_ref.symbol),
        Expr::Intrinsic(symbol) => context.builtin_id(*symbol),
        _ => None,
    };
    if builtin_id.is_none() || matches!(context.expr(*callee), Expr::Intrinsic(_)) {
        result.chain(infer_expr(*callee, context));
    } else {
        result.ty = context.core_types().top;
    }

    // These are compiler-privileged functions rather than ordinary
    // polymorphic functions for now. Their element type relation is checked
    // here until the language has generic function types.
    if let Expr::VarRef(var_ref) = context.expr(*callee) {
        let name = context.value_symbol_str(var_ref.symbol).to_owned();
        if matches!(name.as_str(), "list_new" | "list_push")
            || builtin_id == Some(crate::BuiltinId::ListLen)
        {
            let builtin_result = infer_list_builtin(expr_idx, &name, args, result, context);
            if builtin_result.is_ok() {
                let call = assert_matches!(context.expr_mut(expr_idx), Expr::Call);
                call.signature_index = Some(0);
            }
            return builtin_result;
        }
    }
    if builtin_id == Some(crate::BuiltinId::ListLen) {
        let builtin_result = infer_list_builtin(expr_idx, "list_len", args, result, context);
        if builtin_result.is_ok() {
            let call = assert_matches!(context.expr_mut(expr_idx), Expr::Call);
            call.signature_index = Some(0);
        }
        return builtin_result;
    }

    let result_type = context.type_(result.ty).clone();
    if let Type::Function(FunctionType { signatures, .. }) = result_type {
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

fn infer_list_builtin(
    expr_idx: Idx<Expr>,
    name: &str,
    args: &[Idx<Expr>],
    mut result: TypeResult,
    context: &mut Context,
) -> TypeResult {
    let range = context.database.range_of_expr(expr_idx);
    let expected_args = match name {
        "list_new" => 0,
        "list_push" => 2,
        "list_len" => 1,
        _ => unreachable!(),
    };
    if args.len() != expected_args {
        result.push_diag(TypeDiagnostic::num_args_mismatch(
            expected_args,
            args.len(),
            range,
        ));
        result.ty = context.core_types().error;
        return result;
    }

    if name == "list_new" {
        result.ty = context.type_database.alloc_type(Type::List(ListType {
            of: context.core_types().unknown,
        }));
        return result;
    }

    let list_result = infer_expr(args[0], context);
    result.chain(list_result);
    let list_ty = result.ty;
    let element_ty = match context.type_(list_ty) {
        Type::List(list) => list.of,
        _ => {
            result.push_diag(TypeDiagnostic::mismatch(
                context.type_database.alloc_type(Type::List(ListType {
                    of: context.core_types().unknown,
                })),
                list_ty,
                range,
            ));
            result.ty = context.core_types().error;
            return result;
        }
    };

    if name == "list_push" {
        if element_ty == context.core_types().unknown {
            result.chain(infer_expr(args[1], context));
        } else {
            result.check(args[1], element_ty, context);
        }
        result.ty = context.core_types().unit;
    } else {
        result.ty = context.core_types().int;
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

fn infer_path_expr(expr_idx: Idx<Expr>, path_expr: &PathExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);

    // Infer the subject first (left side of the dot)
    result.chain(infer_expr(path_expr.subject, context));
    if result.is_err() {
        return result;
    }
    let subject_ty_idx = result.ty;

    let Some(member_idx) = path_expr.member else {
        result.ty = subject_ty_idx;
        return result;
    };

    // Handle the member (right side of the dot).
    //
    // `Expr::PathSegment` is not a meaningful standalone expression, it's only valid as
    // part of a `PathExpr`, so we interpret the segment structurally based on the subject type
    // rather than recurse out to the main `infer_expr`.
    let member_expr = context.expr(member_idx).clone();

    match member_expr {
        // Recursive case: `a.(b.c)` — the member is another PathExpr.
        // We infer the inner path and use its resulting type.
        Expr::Path(inner_path) => {
            result.chain(infer_path_expr(expr_idx, &inner_path, context));
        }

        // Normal segment access: `a.b`
        Expr::PathSegment(segment_expr) => {
            let subject_ty = context.type_(subject_ty_idx).clone();
            match subject_ty {
                // `Status.success` where `Status` has type UnionNamespaceType
                Type::UnionNamespace(union_namespace) => {
                    match union_namespace.variant_constructors.get(&segment_expr.key) {
                        Some((_, variant_constructor_ty)) => {
                            result.ty = *variant_constructor_ty;
                        }
                        None => {
                            let range = context.range_of_expr(expr_idx);
                            result.push_diag(TypeDiagnostic {
                                variant: TypeDiagnosticVariant::UnresolvedSumVariant {
                                    variant: segment_expr.key,
                                    ty: subject_ty_idx,
                                },
                                range,
                            });
                            result.ty = context.core_types().error;
                        }
                    }
                }

                // Record / tuple-like product field access: `record.field`
                Type::Product(product_type) => match product_type.fields.get(&segment_expr.key) {
                    Some((_, field_ty)) => {
                        result.ty = *field_ty;
                    }
                    None => {
                        let range = context.range_of_expr(expr_idx);
                        result.push_diag(TypeDiagnostic {
                            variant: TypeDiagnosticVariant::UnresolvedProductField {
                                field: segment_expr.key,
                                ty: subject_ty_idx,
                            },
                            range,
                        });
                        result.ty = context.core_types().error;
                    }
                },

                t => todo!("any other possibilities like {t:?}"),
            }
        }

        // Method/field call as member: `a.b()`
        // Here the member *is* a real expression, so we infer it and use the call return type.
        Expr::Call(_call_expr) => {
            result.chain(infer_expr(member_idx, context));
        }

        // Other member syntaxes are not yet decided/implemented.
        Expr::StringLiteral(_) => todo!("string literal member access not implemented yet"),
        Expr::IntLiteral(_) => todo!("tuple index member access not implemented yet"),

        other => unreachable!(
            "Internal Compiler Error (HIR/typecheck): invalid member of PathExpr: {other:?}"
        ),
    }

    result
}

fn infer_index_int_expr(index_expr: &IndexIntExpr, context: &mut Context) -> TypeResult {
    let IndexIntExpr { subject, .. } = index_expr;

    let mut result = infer_expr(*subject, context);

    if result.is_ok() {
        match context.type_(result.ty) {
            // TODO: the index expr needs to be an Int also
            _ => {
                // TODO: add diagnostic
                // TODO: mutate result.ty to error
                todo!()
            }
        }
    }
    result
}

fn infer_type_call(call: &crate::type_expr::CallExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    if call.path == LIST && call.args.len() == 1 {
        let element = infer_type_expr(call.args[0], context);
        result.chain(element);
        if result.is_ok() {
            result.ty = context
                .type_database
                .alloc_type(Type::List(ListType { of: result.ty }));
        }
    } else {
        result.ty = context.core_types().error;
        // The unresolved-type diagnostic infrastructure is not yet expressive
        // enough for type applications; keep this localized until it is.
    }
    result
}

fn infer_match_expr(match_expr: &MatchExpr, context: &mut Context) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);

    // TODO - should be 'check' instead of 'infer'? scrutinee must be SumType, IntLiteral, FloatLiteral, StringLiteral, etc.
    result.chain(infer_expr(match_expr.scrutinee, context));
    let scrutinee_ty = result.ty;

    let mut overall_ty: Option<Idx<Type>> = None;
    let mut saw_catch_all = false;
    for arm in &match_expr.arms {
        result.chain(infer_pattern(
            arm.pattern,
            scrutinee_ty,
            &mut saw_catch_all,
            context,
        ));

        let arm_ty = infer_expr_widened(arm.expr, context);
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

fn infer_pattern(
    pattern: Idx<Pattern>,
    scrutinee_ty: Idx<Type>,
    saw_catch_all: &mut bool,
    context: &mut Context,
) -> TypeResult {
    let mut result = TypeResult::new(&context.type_database);
    let int_ty = context.core_types().int;
    let bottom_ty = context.core_types().bottom;
    let unknown_ty = context.core_types().unknown;

    let pattern = context.pattern(pattern);
    match pattern {
        Pattern::Wild { meta } => {
            // TODO - store a Key/ValueSymbol for the '_' itself and associate a type for LSP
            if *saw_catch_all {
                result.ty = context.core_types().bottom;
            } else {
                *saw_catch_all = true;
                result.ty = scrutinee_ty;
            }
        }
        Pattern::IdentBinding { meta, binding } => {
            // TODO - `context.type_database.set_expr_type(expr_idx, scrutinee_ty);`
            // if we need to set the type for the Idx<Expr> - probably do for LSP when someone
            // mouses over that expression
            // Need to store Idx<Expr> on IdentPatternBinding

            let pattern_ty = if *saw_catch_all {
                bottom_ty
            } else {
                *saw_catch_all = true;
                if let Some(parent) = meta.parent {
                    let parent = context.pattern(parent);
                    match parent {
                        Pattern::Wild { meta } => todo!("compiler error: can't do `_ ident`"),
                        Pattern::IdentBinding { meta, binding } => {
                            todo!("compiler error: can't do `ident ident`")
                        }
                        Pattern::Variant { meta, pattern } => {
                            let scrutinee_ty =
                                assert_matches!(context.type_(scrutinee_ty), Type::Sum);

                            scrutinee_ty
                                .type_of_key(pattern.variant)
                                .unwrap_or(unknown_ty)
                        }
                        Pattern::IntLiteral { meta, literal } => {
                            todo!("compiler error: can't do `16 ident`")
                        }
                        Pattern::FloatLiteral { meta, literal } => {
                            todo!("compiler error: can't do `1.2 ident`")
                        }
                        Pattern::StringLiteral { meta, literal } => {
                            todo!("compiler error: can't do `\"s\" ident`")
                        }
                    }
                } else {
                    scrutinee_ty
                }
            };
            result.ty = pattern_ty;
            context
                .type_database
                .insert_value_symbol(binding.symbol, pattern_ty);
        }
        Pattern::Variant { meta, pattern } => {
            // TODO - this needs to check for a parent pattern as well, because nested variants are valid
            // i.e. `.variant .inner_variant ident` is valid
            let scrutinee = context.type_(scrutinee_ty);
            match scrutinee {
                Type::Sum(sum_type) => {
                    // TODO - check variant key against sum type here? Or should be done prior in lowering?
                }
                t => todo!("type diagnostic for only can use variant patterns on sum types"),
            }

            // if there's a variable, like `.variant data`, then need to infer type for `data` by backtracking
            // through the scrutinee to the original union definition
            if let Some(inner_pattern) = &pattern.inner_pattern {
                result.chain(infer_pattern(
                    *inner_pattern,
                    scrutinee_ty,
                    &mut false,
                    context,
                ));
            }
        }
        Pattern::IntLiteral { meta, literal } => {
            if scrutinee_ty != int_ty {
                result.push_diag(TypeDiagnostic::mismatch(scrutinee_ty, int_ty, meta.range));
            }
        }
        Pattern::FloatLiteral { meta, literal } => todo!(),
        Pattern::StringLiteral { meta, literal } => todo!(),
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
                false => {
                    let range = context.range_of_expr(expr_idx);
                    TypeResult::from_diag(
                        TypeDiagnostic::mismatch(bool_ty, result.ty, range),
                        context.core_types().error,
                    )
                }
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
            // List indexing will be added with the List MIR operations.
            | Type::String=> result.ty = context.core_types().string,

            _ => result.push_diag(TypeDiagnostic {
                variant: TypeDiagnosticVariant::CannotConvertIntoString { actual: result.ty },
                range: TextRange::default(),
        }),
        },
    }
    result
}

fn infer_record(
    expr_idx: Idx<Expr>,
    expr: &RecordLiteralExpr,
    context: &mut Context,
) -> TypeResult {
    todo!()
}
