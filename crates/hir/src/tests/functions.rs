use indoc::indoc;

use super::check;

#[test]
fn nullary_function() {
    let input = "fun () -> {}";
    let expected_expr = indoc! {"
    fun () -> {};"};

    check(input, expected_expr, &[]);
}

#[test]
fn nullary_function_assignment() {
    let input = "let f = fun () -> {}";
    let expected_expr = indoc! {"
    f~1.0 : () -> () = fun \"f\"() -> {};"};

    check(input, expected_expr, &[("f~1.0", "() -> ()")]);
}

#[test]
fn unary_function() {
    let input = "fun (a: Int) -> {}";
    let expected_expr = indoc! {"
    fun (a~1.0 : Int) -> {};"};

    check(input, expected_expr, &[("a~1.0", "Int")]);
}

#[test]
fn unary_function_assignment() {
    let input = "let f = fun (a: Int) -> {}";

    let expected_expr = indoc! {"
    f~1.0 : (Int) -> () = fun \"f\"(a~1.1 : Int) -> {};"};
    let expected_vars = &[("a~1.1", "Int"), ("f~1.0", "(Int) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_string() {
    let input = "print \"Hello\"";

    let expected_expr = "print~0.0$0 (\"Hello\",);";

    check(input, expected_expr, &[])
}

#[test]
fn print_int() {
    let input = "print 16";

    let expected_expr = "print~0.0$1 (16,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_float() {
    let input = "print 16.0";

    let expected_expr = "print~0.0$2 (16.0,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_bool() {
    let input = "print true";

    let expected_expr = "print~0.0$3 (true~0.2,);";

    check(input, expected_expr, &[])
}

#[test]
fn print_param_function() {
    let input = "fun (a: String) -> { print a }";

    let expected_expr = "fun (a~1.0 : String) -> { print~0.0$0 (a~1.0,); };";
    let expected_vars = &[("a~1.0", "String")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_function_assignment() {
    let input = "let f = fun (a: String) -> { print a }";

    let expected_expr =
        "f~1.0 : (String) -> () = fun \"f\"(a~1.1 : String) -> { print~0.0$0 (a~1.1,); };";
    let expected_vars = &[("a~1.1", "String"), ("f~1.0", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn print_param_with_call() {
    let input = r#"
let print_param = fun (a: String) -> { print a }
print_param "Hello!"
"#;

    let expected_expr = indoc! {"
        print_param~1.0 : (String) -> () = fun \"print_param\"(a~1.1 : String) -> { print~0.0$0 (a~1.1,); };
        print_param~1.0$0 (\"Hello!\",);"};

    let expected_vars = &[("a~1.1", "String"), ("print_param~1.0", "(String) -> ()")];

    check(input, expected_expr, expected_vars);
}

#[test]
fn function_call_function() {
    let input = "
let is_even = fun (a: Int) -> { a % 2 == 0 }

let main = fun (a: Int) -> {
    is_even a
}
";

    let expected_expr = indoc! {"
        is_even~1.0 : (Int) -> (false | true) = fun \"is_even\"(a~1.1 : Int) -> (false | true) { `==`~0.9$0 (%~0.7$0 (a~1.1,2,),0,); };
        main~1.2 : (Int) -> (false | true) = fun \"main\"(a~1.3 : Int) -> (false | true) { is_even~1.0$0 (a~1.3,); };"};

    let expected_vars = &[
        ("a~1.1", "Int"),
        ("a~1.3", "Int"),
        ("is_even~1.0", "(Int) -> (false | true)"),
        ("main~1.2", "(Int) -> (false | true)"),
    ];

    check(input, expected_expr, expected_vars);
}
