use crate::tests::check;

#[test]
fn define_empty_record_type() {
    let input = "
    type Empty = []
";

    let expected_content = "Empty~1.0 := [  ]";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_one_field_record_type() {
    let input = "
    type OneField = [ a: Float ]
";

    let expected_content = "OneField~1.0 := [ a : Float~0.1 ]";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn define_two_field_record_type() {
    let input = "
    type Point = [ x: Float, y: Float ]
";

    let expected_content = "Point~1.0 := [ x : Float~0.1, y : Float~0.1 ]";
    let expected_vars = &[];

    check(input, expected_content, expected_vars);
}

#[test]
fn inferred_empty_record() {
    let input = "let empty = []";

    let expected_content = "empty~1.0 : [  ] = [ ];";
    let expected_vars = &[("empty~1.0", "[  ]")];

    check(input, expected_content, expected_vars);
}

#[test]
fn inferred_one_int_field() {
    let input = "let one_int = [ i = 123 ]";

    let expected_content = "one_int~1.0 : [ i : 123 ] = [ i = 123 ];";
    let expected_vars = &[("one_int~1.0", "[ i : 123 ]")];

    check(input, expected_content, expected_vars);
}

#[test]
fn access_field_of_record() {
    let input = "let main = fun (i: Int) -> {
    let record = [ int = i ]
    record.int
    }";

    let expected_content = r#"main~1.0 : (Int) -> Int = fun "main"(i~1.1 : Int) -> Int {
    record~1.2 : [ int : Int ] = [ int = i~1.1 ];
    record~1.2.int;
};"#;
    let expected_vars = &[
        ("i~1.1", "Int"),
        ("main~1.0", "(Int) -> Int"),
        ("record~1.2", "[ int : Int ]"),
    ];

    check(input, expected_content, expected_vars);
}
