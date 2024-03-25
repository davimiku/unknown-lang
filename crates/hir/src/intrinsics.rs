use la_arena::Idx;

use crate::database::Database;
use crate::scope::ModuleScopes;
use crate::typecheck::{CoreTypes, TypeDatabase};
use crate::{FuncSignature, Interner, IntrinsicExpr, Key, Type};

pub(crate) fn insert_core_values(
    database: &mut Database,
    type_database: &mut TypeDatabase,
    scopes: &mut ModuleScopes,
    interner: &Interner,
) {
    use IntrinsicExpr as IE;
    let t = type_database.core.clone();
    let keys = interner.core_keys();

    let mut insert_type = |key: Key, ty: Idx<Type>| {
        let symbol = scopes.insert_type(key);
        database.type_names.insert(symbol, key);
        type_database.insert_type_symbol(symbol, ty);
    };

    insert_type(keys.bool, t.bool);
    insert_type(keys.int, t.int);
    insert_type(keys.float, t.float);
    insert_type(keys.string, t.string);

    let mut insert_value = |key: Key, fn_type: Type| {
        let symbol = scopes.insert_value(key);
        database.value_names.insert(symbol, key);
        let ty = type_database.alloc_type(fn_type);
        type_database.insert_value_symbol(symbol, ty);

        symbol
    };

    insert_value(keys.print, Type::func(print_signatures(&t)));

    let mut insert_intrinsic = |key: Key, fn_type: Type, intrinsic: IntrinsicExpr| {
        let symbol = scopes.insert_value(key);
        database.value_names.insert(symbol, key);
        let ty = type_database.alloc_type(fn_type);
        type_database.insert_value_symbol(symbol, ty);

        database.operators.insert(symbol, intrinsic);
    };

    insert_intrinsic(keys.add, Type::func(add_signatures(&t)), IE::Add);
    insert_intrinsic(keys.sub, Type::func(sub_signatures(&t)), IE::Sub);
    insert_intrinsic(keys.mul, Type::func(mul_signatures(&t)), IE::Mul);
    insert_intrinsic(keys.div, Type::func(div_signatures(&t)), IE::Div);
    insert_intrinsic(
        keys.rem,
        Type::func(vec![((t.int, t.int), t.int).into()]),
        IE::Rem,
    );
    insert_intrinsic(
        keys.concat,
        Type::func(vec![((t.string, t.string), t.string).into()]),
        IE::Concat,
    );

    insert_intrinsic(keys.eq, Type::func(eq_cmp_signatures(&t)), IE::Eq);
    insert_intrinsic(keys.ne, Type::func(eq_cmp_signatures(&t)), IE::Ne);

    insert_intrinsic(keys.lt, Type::func(num_cmp_signatures(&t)), IE::Lt);
    insert_intrinsic(keys.le, Type::func(num_cmp_signatures(&t)), IE::Le);
    insert_intrinsic(keys.gt, Type::func(num_cmp_signatures(&t)), IE::Gt);
    insert_intrinsic(keys.ge, Type::func(num_cmp_signatures(&t)), IE::Ge);
}

fn print_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    // TODO: this order might be important, make a mechanism to synchronize
    // this across HIR, MIR, and clif_codegen
    // since we're resolving signature by index?
    vec![
        ((types.string,), types.unit).into(),
        ((types.int,), types.unit).into(),
        ((types.float,), types.unit).into(),
        ((types.bool,), types.unit).into(),
    ]
}

fn add_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    vec![
        ((types.int, types.int), types.int).into(),
        ((types.int, types.float), types.float).into(),
        ((types.float, types.int), types.float).into(),
        ((types.float, types.float), types.float).into(),
    ]
}

fn sub_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    add_signatures(types)
}

fn mul_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    add_signatures(types)
}

fn div_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    vec![
        ((types.int, types.int), types.float).into(),
        ((types.int, types.float), types.float).into(),
        ((types.float, types.int), types.float).into(),
        ((types.float, types.float), types.float).into(),
    ]
}

fn eq_cmp_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    vec![
        ((types.int, types.int), types.bool).into(),
        ((types.float, types.float), types.bool).into(),
        ((types.bool, types.bool), types.bool).into(),
        ((types.string, types.string), types.bool).into(),
    ]
}

fn num_cmp_signatures(types: &CoreTypes) -> Vec<FuncSignature> {
    vec![
        ((types.int, types.int), types.bool).into(),
        ((types.float, types.float), types.bool).into(),
    ]
}