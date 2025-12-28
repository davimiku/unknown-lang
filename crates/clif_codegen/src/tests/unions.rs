use crate::builtins::{XInt, XTag};
use crate::tests::{compile_main, to_fn, to_fn_one_param_sret, to_fn_zero_param_sret};

#[test]
fn define_and_pass_through_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(XInt,), XInt>(code_ptr) };

    assert_eq!(code_fn((0,)), 0);
    assert_eq!(code_fn((1,)), 1);
    assert_eq!(code_fn((2,)), 2);
}

#[test]
fn define_and_use_sum_type() {
    let input = "
type Color = red | green | blue

let main = fun () -> { Color.green }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 1);
}

#[test]
fn unit_union_first_variant() {
    let input = "
type Color = red | green | blue

let main = fun () -> { Color.red }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 0);
}

#[test]
fn unit_union_last_variant() {
    let input = "
type Color = red | green | blue

let main = fun () -> { Color.blue }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 2);
}

#[test]
fn unit_union_with_many_variants() {
    let input = "
type DaysOfWeek = monday | tuesday | wednesday | thursday | friday | saturday | sunday

let main = fun () -> { DaysOfWeek.saturday }";

    let code_ptr = compile_main(input);

    let code_fn = unsafe { to_fn::<(), XInt>(code_ptr) };

    assert_eq!(code_fn(()), 5);
}

/// Helper struct for unions with tag + one i64 payload (16 bytes)
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
struct Union2Words {
    tag: i64,
    payload: i64,
}

/// Helper struct for unions with tag + one f64 payload (16 bytes)
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
struct Union2WordsFloat {
    tag: i64,
    payload: f64,
}

/// Helper struct for nested unions (24 bytes)
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
struct Union3Words {
    tag: i64,
    payload1: i64,
    payload2: i64,
}

#[test]
fn construct_union_with_int_data() {
    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_c 32 }
";

    let code_ptr = compile_main(input);

    // Function signature: fn(i64, *mut Union2Words) -> ()
    let code_fn = unsafe { to_fn_one_param_sret::<XTag, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(100_000, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 2,
            payload: 32
        }
    );

    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_c i }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<XTag, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(16, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 2,
            payload: 16
        }
    );
}

#[test]
fn construct_union_with_int_data_first_variant() {
    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_a i }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<i64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(42, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 0,
            payload: 42
        }
    );
}

#[test]
fn construct_union_with_int_data_negative() {
    let input = "
type CoolInt = (int_a: Int | int_b: Int)

let main = fun (i: Int) -> { CoolInt.int_b i }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<i64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(-999, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 1,
            payload: -999
        }
    );
}

#[test]
fn construct_union_with_float_data() {
    let input = "
type CoolFloat = (float_a: Float | float_b: Float | float_c: Float)

let main = fun (f: Float) -> { CoolFloat.float_c f }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<f64, Union2WordsFloat>(code_ptr) };

    let mut result = Union2WordsFloat {
        tag: 0,
        payload: 0.0,
    };
    code_fn(1.23, &mut result);
    assert_eq!(
        result,
        Union2WordsFloat {
            tag: 2,
            payload: 1.23
        }
    );
}

#[test]
fn construct_union_with_float_data_first_variant() {
    let input = "
type CoolFloat = (float_a: Float | float_b: Float)

let main = fun (f: Float) -> { CoolFloat.float_a f }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<f64, Union2WordsFloat>(code_ptr) };

    let mut result = Union2WordsFloat {
        tag: 0,
        payload: 0.0,
    };
    code_fn(3.14159, &mut result);
    assert_eq!(
        result,
        Union2WordsFloat {
            tag: 0,
            payload: 3.14159
        }
    );
}

#[test]
fn construct_union_with_float_literal() {
    let input = "
type CoolFloat = (float_a: Float | float_b: Float)

let main = fun () -> { CoolFloat.float_b 2.718 }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_zero_param_sret::<Union2WordsFloat>(code_ptr) };

    let mut result = Union2WordsFloat {
        tag: 0,
        payload: 0.0,
    };
    code_fn(&mut result);
    assert_eq!(
        result,
        Union2WordsFloat {
            tag: 1,
            payload: 2.718
        }
    );
}

#[test]
fn construct_union_with_negative_float() {
    let input = "
type CoolFloat = (pos: Float | neg: Float)

let main = fun (f: Float) -> { CoolFloat.neg f }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<f64, Union2WordsFloat>(code_ptr) };

    let mut result = Union2WordsFloat {
        tag: 0,
        payload: 0.0,
    };
    code_fn(-99.5, &mut result);
    assert_eq!(
        result,
        Union2WordsFloat {
            tag: 1,
            payload: -99.5
        }
    );
}

#[test]
fn construct_union_with_int_float_data_literal() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun () -> { Number.float 1.23 }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_zero_param_sret::<Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(&mut result);

    assert_eq!(result.tag, 1);
    // Float is stored as bitcast i64
    assert_eq!(result.payload, bytemuck::cast::<f64, i64>(1.23));
}

#[test]
fn construct_union_with_int_float_data() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (f: Float) -> { Number.float f }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<f64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(1.23, &mut result);

    assert_eq!(result.tag, 1);
    assert_eq!(result.payload, bytemuck::cast::<f64, i64>(1.23));
}

#[test]
fn mixed_union_int_variant() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (i: Int) -> { Number.int i }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<i64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(42, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 0,
            payload: 42
        }
    );

    code_fn(-100, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 0,
            payload: -100
        }
    );

    code_fn(0, &mut result);
    assert_eq!(result, Union2Words { tag: 0, payload: 0 });
}

#[test]
fn mixed_union_float_variant() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (f: Float) -> { Number.float f }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<f64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };

    code_fn(0.0, &mut result);
    assert_eq!(result.tag, 1);
    assert_eq!(result.payload, bytemuck::cast::<f64, i64>(0.0));

    code_fn(-1.5, &mut result);
    assert_eq!(result.tag, 1);
    assert_eq!(result.payload, bytemuck::cast::<f64, i64>(-1.5));
}

#[test]
fn mixed_union_with_int_literal() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun () -> { Number.int 999 }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_zero_param_sret::<Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(&mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 0,
            payload: 999
        }
    );
}

#[test]
fn mixed_union_three_variants() {
    let input = "
type Value = (int: Int | float: Float | other: Int)

let main = fun (i: Int) -> { Value.other i }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<i64, Union2Words>(code_ptr) };

    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(123, &mut result);
    assert_eq!(
        result,
        Union2Words {
            tag: 2,
            payload: 123
        }
    );
}

#[test]
fn pass_through_union_with_int_payload() {
    let input = "
type CoolInt = (a: Int | b: Int)

let main = fun (c: CoolInt) -> { c }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<*const Union2Words, Union2Words>(code_ptr) };

    let input = Union2Words {
        tag: 0,
        payload: 42,
    };
    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(&input, &mut result);
    assert_eq!(result, input);

    let input = Union2Words {
        tag: 1,
        payload: 100,
    };
    code_fn(&input, &mut result);
    assert_eq!(result, input);
}

#[test]
fn pass_through_union_with_float_payload() {
    let input = "
type CoolFloat = (a: Float | b: Float)

let main = fun (c: CoolFloat) -> { c }
";

    let code_ptr = compile_main(input);
    let code_fn =
        unsafe { to_fn_one_param_sret::<*const Union2WordsFloat, Union2WordsFloat>(code_ptr) };

    let input = Union2WordsFloat {
        tag: 0,
        payload: 1.5,
    };
    let mut result = Union2WordsFloat {
        tag: 0,
        payload: 0.0,
    };
    code_fn(&input, &mut result);
    assert_eq!(result, input);

    let input = Union2WordsFloat {
        tag: 1,
        payload: 2.5,
    };
    code_fn(&input, &mut result);
    assert_eq!(result, input);
}

#[test]
fn pass_through_mixed_union() {
    let input = "
type Number = (int: Int | float: Float)

let main = fun (n: Number) -> { n }
";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<*const Union2Words, Union2Words>(code_ptr) };

    // Int payload
    let input = Union2Words {
        tag: 0,
        payload: 42,
    };
    let mut result = Union2Words { tag: 0, payload: 0 };
    code_fn(&input, &mut result);
    assert_eq!(result, input);

    // Float payload (stored as bitcast i64)
    let float_bits = bytemuck::cast::<f64, i64>(3.14);
    let input = Union2Words {
        tag: 1,
        payload: float_bits,
    };
    code_fn(&input, &mut result);
    assert_eq!(result, input);
}

#[test]
fn pass_through_stack_allocated_union() {
    let input = "
type InnerUnion = (int_a: Int | int_b: Int)
type OuterUnion = (a | b: Int | c: InnerUnion)

let main = fun (u: OuterUnion) -> { u }
        ";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<*const Union3Words, Union3Words>(code_ptr) };

    // case a: tag=0, payloads are arbitrary
    let case_a = Union3Words {
        tag: 0,
        payload1: 0,
        payload2: 0,
    };
    let mut result = Union3Words {
        tag: 0,
        payload1: 0,
        payload2: 0,
    };
    code_fn(&case_a, &mut result);
    assert_eq!(result, case_a);

    // case b: tag=1, payload1=Int value
    let case_b = Union3Words {
        tag: 1,
        payload1: 16,
        payload2: 0,
    };
    code_fn(&case_b, &mut result);
    assert_eq!(result, case_b);

    // case c: tag=2, payload1=inner_tag, payload2=inner_payload
    let case_c = Union3Words {
        tag: 2,
        payload1: 1,  // inner tag for int_b
        payload2: 16, // inner payload
    };
    code_fn(&case_c, &mut result);
    assert_eq!(result, case_c);
}

#[test]
fn unwrap_add_and_rewrap() {
    let input = "type Number = (int: Int | float: Float)

let main = fun (n: Number) -> {
    match n {
        .int i -> { Number.int (i + 16) }
        .float f -> { Number.float (f + 16.0) }
    }
}";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn_one_param_sret::<*const Union2Words, Union2Words>(code_ptr) };

    let case_a = Union2Words { tag: 0, payload: 1 };
    let expected_a = Union2Words {
        tag: 0,
        payload: 17,
    };
    let mut output_a = Union2Words { tag: 0, payload: 0 }; // overwritten
    code_fn(&case_a, &mut output_a);
    assert_eq!(output_a, expected_a);

    let case_b = Union2Words {
        tag: 1,
        payload: bytemuck::cast(2.0),
    };
    let mut output_b = Union2Words { tag: 0, payload: 0 }; // overwritten
    code_fn(&case_b, &mut output_b);

    assert_eq!(1, output_b.tag);
    assert_eq!(18.0, bytemuck::cast::<i64, f64>(output_b.payload));
}

#[test]
fn unwrap_nested_to_int() {
    let input = "type InnerUnion = (int_a: Int | int_b: Int)
type OuterUnion = (a | b: Int | c: InnerUnion)

let main = fun (u: OuterUnion) -> Int {
    match u {
        .a -> { 42 }
        .b b_int -> { b_int }
        .c inner -> {
            match inner {
                .int_a a_int -> { a_int }
                .int_b b_int -> { b_int }
            }
        }
    }
}";

    let code_ptr = compile_main(input);
    let code_fn = unsafe { to_fn::<*const Union2Words, XInt>(code_ptr) };

    // tag=a, other fields arbitrary
    let case_a = Union3Words {
        tag: 0,
        payload1: 0,
        payload2: 0,
    };
}
