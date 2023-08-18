use la_arena::Idx;

use crate::{lowering_context::ContextDisplay, Context, Expr, Type};

/// Formats an expression into a String representation
///
/// This format is not stable and should not be used in machine parsing. It is
/// meant to be read and understood by humans, and may be used in some non-permanent test cases.
pub fn display_root(root: Idx<Expr>, context: &Context) -> String {
    let mut s = String::new();
    s.push_str(&root.display(context));

    fmt_local_types(&mut s, context);

    s
}

pub(crate) fn fmt_local_types(s: &mut String, context: &Context) {
    s.push('\n');
    let mut locals: Vec<(String, Idx<Type>)> = context
        .type_database
        .value_symbols
        .iter()
        .map(|(key, ty)| (key.display(context), *ty))
        .collect();
    locals.sort_by(|(a, ..), (b, ..)| a.cmp(b));

    for (name, ty) in locals {
        s.push_str(&name);
        s.push_str(" : ");
        s.push_str(&format!("{}\n", ty.display(context)));
    }
}
