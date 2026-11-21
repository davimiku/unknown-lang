use crate::construct;

use super::check_module;
use hir::{Context, ContextDisplay, Type};
use itertools::Itertools as _;

#[test]
fn main_call_is_even() {
    let input = "
let is_even = fun (a: Int) -> { a % 2 == 0 }

let main = fun (a: Int) -> {
    is_even a
}
";

    let expected = "
fun is_even:
    params: _1
    mut _0: Bool~0.3
    _1: Int
    _2: Int

    BB0():
        _2 := Rem(copy _1, const 2)
        _0 := Eq(copy _2, const 0)
        Return _0 ->

fun main:
    params: _1
    mut _0: Bool~0.3
    _1: Int

    BB0():
        _0 = is_even (copy _1) -> [return: BB1(), unwind -> TODO]
    BB1():
        Return _0 ->";

    check_module(input, expected);
}

#[test]
fn main_call_is_even_conditional() {
    let input = "
let is_even = fun (a: Int) -> Bool { a % 2 == 0 }

let main = fun (a: Int) -> Int {
    if is_even a {
        16
    } else {
        7
    }
}

";

    let expected = "
fun is_even:
    params: _1
    mut _0: Bool~0.3
    _1: Int
    _2: Int

    BB0():
        _2 := Rem(copy _1, const 2)
        _0 := Eq(copy _2, const 0)
        Return _0 ->

fun main:
    params: _1
    mut _0: Int
    _1: Int
    _2: Bool~20.0

    BB0():
        _2 = is_even (copy _1) -> [return: BB1(), unwind -> TODO]
    BB1():
        BranchInt(copy _2): [0 -> BB3(), else -> BB2()]
    BB2():
        _0 := const 16
        Jump -> BB4(_0)
    BB3():
        _0 := const 7
        Jump -> BB4(_0)
    BB4(_0):
        Return _0 ->
";

    check_module(input, expected);
}

#[test]
fn generic_function_calls_create_concrete_mir_instances() {
    let input = r#"
        type Box 'A = [ value: 'A ]
        type Pair ('A, 'B) = [ first: 'A, second: 'B ]

        let identity = fun ['A](a: 'A) -> 'A {
            a
        }
        let make_box = fun ['A](a: 'A) -> Box 'A {
            [ value = identity a ]
        }
        let make_pair = fun ['A, 'B](a: 'A, b: 'B) -> Pair ('A, 'B) {
            [ first = a, second = b ]
        }

        let main = fun () -> {
            let int_box = make_box 123
            let string_box = make_box "hello"
            make_pair (42, "answer")
        }
    "#;
    let (module, context) = construct(input);

    let names = module
        .functions
        .iter()
        .filter_map(|(_, function)| function.name.as_deref())
        .collect_vec();
    assert!(names.contains(&"make_box$Int"));
    assert!(names.contains(&"make_box$String"));
    assert!(names.contains(&"make_pair$Int$String"));
    assert!(names.contains(&"identity$Int"));
    assert!(names.contains(&"identity$String"));

    fn contains_type_parameter(ty: la_arena::Idx<Type>, context: &Context) -> bool {
        match context.type_(ty) {
            Type::TypeParameter(_) => true,
            Type::Product(product) => product
                .fields
                .values()
                .any(|(_, field)| contains_type_parameter(*field, context)),
            Type::Sum(sum) => sum
                .variants
                .iter()
                .any(|(_, variant)| contains_type_parameter(*variant, context)),
            Type::List(list) => contains_type_parameter(list.of, context),
            Type::Function(function) => function.signatures.iter().any(|signature| {
                signature
                    .params
                    .iter()
                    .any(|param| contains_type_parameter(*param, context))
                    || contains_type_parameter(signature.return_ty, context)
            }),
            _ => false,
        }
    }

    for (_, function) in module.functions.iter() {
        for param in &function.params {
            assert!(
                !contains_type_parameter(*param, &context),
                "MIR parameter retained an unresolved type parameter: {}",
                param.display(&context)
            );
        }
        for (_, local) in function.locals.iter() {
            assert!(
                !contains_type_parameter(local.type_idx(), &context),
                "MIR local retained an unresolved type parameter: {}",
                local.type_idx().display(&context)
            );
        }
    }
}

#[test]
fn list_builtins_lower_to_list_mir_operations() {
    let input = "
    let main = fun () -> {
        let values: List Int = list_new ()
        list_push (values, 42)
        list_len values
    }
    ";

    let expected = "
fun main:
    params: {none}
    mut _0: Int
    _1: List Int
    _2: ()

    BB0():
        _1 := list_new ()
        list_push copy _1, const 42
        _0 := list_len copy _1
        Return _0 ->
";

    check_module(input, expected);
}
