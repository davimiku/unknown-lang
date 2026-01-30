use super::check;

#[test]
fn define_union() {
    let input = "
    type Color = red | green | blue
";

    let expected_content = "Color~1.0 := red: () | green: () | blue: ()";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_union_explicit_unit() {
    let input = "
    type Color = red: () | green: () | blue: ()
";

    let expected_content = "Color~1.0 := red: () | green: () | blue: ()";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_and_pass_through_union() {
    let input = "
type Color = red | green | blue

let main = fun (c: Color) -> { c }";

    let expected_content = r#"Color~1.0 := red: () | green: () | blue: ()
main~1.1 : (Color~1.0) -> Color~1.0 = fun "main"(c~1.2 : Color~1.0) -> Color~1.0 { c~1.2; };"#;
    let expected_vars = &[
        ("c~1.2", "Color~1.0"),
        ("main~1.1", "(Color~1.0) -> Color~1.0"),
    ];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_and_pass_through_complex_union() {
    let input = "
type InnerUnion = (int_a: Int | int_b: Int)
type OuterUnion = (a | b: Int | c: InnerUnion)

let main = fun (u: OuterUnion) -> { u }
        ";

    let expected_content = r#"InnerUnion~1.0 := int_a: Int~0.0 | int_b: Int~0.0
OuterUnion~1.1 := a: () | b: Int~0.0 | c: InnerUnion~1.0
main~1.2 : (OuterUnion~1.1) -> OuterUnion~1.1 = fun "main"(u~1.3 : OuterUnion~1.1) -> OuterUnion~1.1 { u~1.3; };"#;
    let expected_vars = &[
        ("main~1.2", "(OuterUnion~1.1) -> OuterUnion~1.1"),
        ("u~1.3", "OuterUnion~1.1"),
    ];

    check(input, expected_content, expected_vars);
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

    let expected_content = r#"Number~1.0 := int: Int~0.0 | float: Float~0.1
main~1.1 : (Number~1.0) -> Number~1.0 = fun "main"(n~1.2 : Number~1.0) -> Number~1.0 { match n~1.2 {
    .int i -> { Number~1.0.int$0 (`+`~0.3$0 (i~1.3,16,),); }
    .float f -> { Number~1.0.float$0 (`+`~0.3$3 (f~1.4,16.0,),); }
}; };"#;
    let expected_vars = &[
        ("f~1.4", "Float"),
        ("i~1.3", "Int"),
        ("main~1.1", "(Number~1.0) -> Number~1.0"),
        ("n~1.2", "Number~1.0"),
    ];

    check(input, expected_content, expected_vars);
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
                .int_a a_int -> { a_int + 1 }
                .int_b b_int -> { b_int + 2 }
            }
        }
    }
}";

    let expected_content = "InnerUnion~1.0 := int_a: Int~0.0 | int_b: Int~0.0
OuterUnion~1.1 := a: () | b: Int~0.0 | c: InnerUnion~1.0
main~1.2 : (OuterUnion~1.1) -> Int = fun \"main\"(u~1.3 : OuterUnion~1.1) -> Int { match u~1.3 {
    .a -> { 42; }
    .b b_int -> { b_int~1.4; }
    .c inner -> { match inner~1.5 {
        .int_a a_int -> { `+`~0.3$0 (a_int~1.6,1,); }
        .int_b b_int -> { `+`~0.3$0 (b_int~1.7,2,); }
    }; }
}; };";
    let expected_vars = &[
        ("a_int~1.6", "Int"),
        ("b_int~1.4", "Int"),
        ("b_int~1.7", "Int"),
        ("inner~1.5", "InnerUnion~1.0"),
        ("main~1.2", "(OuterUnion~1.1) -> Int"),
        ("u~1.3", "OuterUnion~1.1"),
    ];

    check(input, expected_content, expected_vars);
}

#[test]
fn construct_union_with_int_data() {
    let input = "
type CoolInt = (int_a: Int | int_b: Int | int_c: Int)

let main = fun (i: Int) -> { CoolInt.int_c 32 }
";

    let expected_content = r#"
CoolInt~1.0 := int_a: Int~0.0 | int_b: Int~0.0 | int_c: Int~0.0
main~1.1 : (Int) -> CoolInt~1.0 = fun "main"(i~1.2 : Int) -> CoolInt~1.0 { CoolInt~1.0.int_c$0 (32,); };"#;
    let expected_vars = &[
        ("CoolInt~1.0", "CoolInt~1.0"),
        ("i~1.2", "Int"),
        ("main~1.1", "(Int) -> CoolInt~1.0"),
    ];

    check(input, expected_content, expected_vars);
}
