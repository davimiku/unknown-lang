use expect_test::expect;

use crate::test_parse_type_expr;

fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = test_parse_type_expr(input);

    expected_tree.assert_eq(&parse.debug_tree());
}

#[test]
fn plain_type() {
    let input = "A";
    check(
        input,
        expect![[r#"
TypeExpr@0..1
  Ident@0..1
    Ident@0..1 "A""#]],
    )
}

#[test]
fn path_type() {
    let input = "A.B";
    check(
        input,
        expect![[r#"
            TypeExpr@0..3
              PathExpr@0..3
                Ident@0..1
                  Ident@0..1 "A"
                Dot@1..2 "."
                Ident@2..3
                  Ident@2..3 "B""#]],
    )
}

#[test]
fn array_type() {
    let input = "[]A";
    check(
        input,
        expect![[r#"
TypeExpr@0..3
  ArrayType@0..3
    LBracket@0..1 "["
    RBracket@1..2 "]"
    Ident@2..3
      Ident@2..3 "A""#]],
    )
}

#[test]
fn parameterized_type() {
    let input = "Option Int";
    check(
        input,
        expect![[r#"
            TypeExpr@0..10
              Call@0..10
                Ident@0..7
                  Ident@0..6 "Option"
                  Emptyspace@6..7 " "
                CallArgs@7..10
                  PathExpr@7..10
                    Ident@7..10
                      Ident@7..10 "Int""#]],
    )
}

#[test]
fn type_function_no_params() {
    let input = "() -> A";
    check(
        input,
        expect![[r#"
            TypeExpr@0..7
              FunExpr@0..7
                ParenExpr@0..3
                  LParen@0..1 "("
                  RParen@1..2 ")"
                  Emptyspace@2..3 " "
                Arrow@3..5 "->"
                Emptyspace@5..6 " "
                Ident@6..7
                  Ident@6..7 "A""#]],
    );
}

#[test]
fn type_function_one_param() {
    let input = "A -> B";
    check(
        input,
        expect![[r#"
            TypeExpr@0..6
              FunExpr@0..6
                Ident@0..2
                  Ident@0..1 "A"
                  Emptyspace@1..2 " "
                Arrow@2..4 "->"
                Emptyspace@4..5 " "
                Ident@5..6
                  Ident@5..6 "B""#]],
    );
}

#[test]
fn type_function_one_param_with_paren() {
    let input = "(A) -> B";
    check(
        input,
        expect![[r#"
            TypeExpr@0..8
              FunExpr@0..8
                ParenExpr@0..4
                  LParen@0..1 "("
                  Ident@1..2
                    Ident@1..2 "A"
                  RParen@2..3 ")"
                  Emptyspace@3..4 " "
                Arrow@4..6 "->"
                Emptyspace@6..7 " "
                Ident@7..8
                  Ident@7..8 "B""#]],
    );
}

#[test]
fn type_function_two_params() {
    let input = "(A, B) -> C";
    check(
        input,
        expect![[r#"
            TypeExpr@0..11
              FunExpr@0..11
                ParenExpr@0..7
                  LParen@0..1 "("
                  Ident@1..2
                    Ident@1..2 "A"
                  Comma@2..3 ","
                  Emptyspace@3..4 " "
                  Ident@4..5
                    Ident@4..5 "B"
                  RParen@5..6 ")"
                  Emptyspace@6..7 " "
                Arrow@7..9 "->"
                Emptyspace@9..10 " "
                Ident@10..11
                  Ident@10..11 "C""#]],
    );
}
