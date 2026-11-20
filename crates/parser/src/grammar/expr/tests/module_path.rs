use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_single_module_name() {
    check_expr(
        "@core",
        expect![[r#"
        ModulePathExpr@0..5
          ModuleName@0..5 "@core""#]],
    )
}

#[test]
fn parse_module_path() {
    check_expr(
        "@core/collections",
        expect![[r#"
        ModulePathExpr@0..17
          ModuleName@0..5 "@core"
          Slash@5..6 "/"
          Ident@6..17
            Ident@6..17 "collections""#]],
    )
}

#[test]
fn parse_module_path_with_symbol() {
    check_expr(
        "@core/collections/List",
        expect![[r#"
        ModulePathExpr@0..22
          ModuleName@0..5 "@core"
          Slash@5..6 "/"
          Ident@6..17
            Ident@6..17 "collections"
          Slash@17..18 "/"
          Ident@18..22
            Ident@18..22 "List""#]],
    )
}

#[test]
fn parse_module_import_binding() {
    check_expr(
        "module Collections = @core/collections",
        expect![[r#"
        ImportBinding@0..38
          ModuleKw@0..6 "module"
          Emptyspace@6..7 " "
          Ident@7..19
            Ident@7..18 "Collections"
            Emptyspace@18..19 " "
          Equals@19..20 "="
          Emptyspace@20..21 " "
          ModulePathExpr@21..38
            ModuleName@21..26 "@core"
            Slash@26..27 "/"
            Ident@27..38
              Ident@27..38 "collections""#]],
    )
}
