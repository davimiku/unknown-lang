mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::ParseEntryPoint;
use crate::parser::Parser;

use crate::syntax::SyntaxKind;

// TODO: Remove this and all tests from AST that relies on these internal details
pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    match p.entry_point {
        ParseEntryPoint::Root => parse_root(p),
        ParseEntryPoint::ExprTest => expr::parse_expr(p).expect("successfully parsed an Expr"),
        ParseEntryPoint::TypeTest => {
            expr::test_parse_type_expr(p).expect("successfully parsed a type Expr")
        }
    }
}

fn parse_root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    while !p.at_end() {
        expr::parse_expr(p);
    }
    m.complete(p, SyntaxKind::Root)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_empty() {
        check("", expect![[r#"Root@0..0"#]])
    }

    #[test]
    fn parse_multiple_string_literals() {
        check(
            r#""a1"
"b2"
"c3"
"d4""#,
            expect![[r#"
Root@0..19
  StringLiteralExpr@0..4
    StringLiteralExpr@0..4 "\"a1\""
  Newline@4..5
    Newline@4..5 "\n"
  StringLiteralExpr@5..9
    StringLiteralExpr@5..9 "\"b2\""
  Newline@9..10
    Newline@9..10 "\n"
  StringLiteralExpr@10..14
    StringLiteralExpr@10..14 "\"c3\""
  Newline@14..15
    Newline@14..15 "\n"
  StringLiteralExpr@15..19
    StringLiteralExpr@15..19 "\"d4\"""#]],
        )
    }

    #[test]
    fn parse_function_def_and_call() {
        let input = r#"let print_param = (a: String) -> print a
        print_param "Hello!""#;
        let expected = expect![[r#"
Root@0..69
  LetBinding@0..40
    Let@0..3 "let"
    Emptyspace@3..4 " "
    Ident@4..16
      Ident@4..15 "print_param"
      Emptyspace@15..16 " "
    Equals@16..17 "="
    Emptyspace@17..18 " "
    FunExpr@18..40
      ParenExpr@18..30
        LParen@18..19 "("
        ParenExprItem@19..28
          Path@19..20
            Ident@19..20
              Ident@19..20 "a"
          Colon@20..21 ":"
          Emptyspace@21..22 " "
          TypeExpr@22..28
            Path@22..28
              Ident@22..28
                Ident@22..28 "String"
        RParen@28..29 ")"
        Emptyspace@29..30 " "
      Arrow@30..32 "->"
      Emptyspace@32..33 " "
      Call@33..40
        Path@33..39
          Ident@33..39
            Ident@33..38 "print"
            Emptyspace@38..39 " "
        CallArgs@39..40
          Path@39..40
            Ident@39..40
              Ident@39..40 "a"
  Newline@40..49
    Newline@40..41 "\n"
    Emptyspace@41..49 "        "
  Call@49..69
    Path@49..61
      Ident@49..61
        Ident@49..60 "print_param"
        Emptyspace@60..61 " "
    CallArgs@61..69
      StringLiteralExpr@61..69
        StringLiteralExpr@61..69 "\"Hello!\"""#]];
        check(input, expected);
    }

    #[test]
    fn parse_plain_return() {
        let input = r#"return 1"#;
        let expected = expect![[r#"
Root@0..8
  ReturnStatement@0..8
    Return@0..6 "return"
    Emptyspace@6..7 " "
    IntLiteralExpr@7..8
      IntLiteral@7..8 "1""#]];

        check(input, expected)
    }
}
