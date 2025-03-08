use super::check;

#[test]
fn empty_loop() {
    let input = "loop {}";

    let expected = "loop {};";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_immediate_break() {
    let input = "loop { break }";

    let expected = "loop { break; };";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_break_later() {
    let input = "loop { 
if true { break }
}";

    let expected = "loop { if (true~0.2) { break; }; };";

    let expected_vars = &[];

    check(input, expected, expected_vars);
}

#[test]
fn loop_full_example() {
    let input = "
let main = fun () -> {
    let mut i = 0
    loop {
        if i > 5 { break }
        i = i + 1
    }
    i
}";

    let expected = "main~1.0 : () -> Int = fun \"main\"() -> Int {
    i~1.1 : mut Int = 0;
    loop {
        if (`>`~0.13$0 (i~1.1,5,)) { break; };
        i~1.1 <- `+`~0.3$0 (i~1.1,1,);
    };
    i~1.1;
};";

    let expected_vars = &[("i~1.1", "Int"), ("main~1.0", "() -> Int")];

    check(input, expected, expected_vars);
}
