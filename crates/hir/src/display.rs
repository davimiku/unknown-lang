use la_arena::Idx;

use crate::{Context, Expr, Module, Type};

/// Types implementing this trait can be processed into string messages
/// with the information available in a Context
pub trait ContextDisplay {
    /// Display `self` with the information available in a `Context`
    #[must_use]
    fn display(&self, context: &Context) -> String;
}

pub fn display_module(module: &Module, context: &Context) -> (String, String) {
    let mut content = String::new();
    for expr in module.exprs.iter() {
        content.push_str(&expr.display(context));
        content.push('\n');
    }
    let mut locals = String::new();
    fmt_local_types(&mut locals, context);
    (content, locals)
}

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
        .filter(|(symbol, _)| !symbol.in_core_module())
        .map(|(symbol, ty)| (symbol.display(context), *ty))
        .collect();
    locals.sort_by(|(a, ..), (b, ..)| a.cmp(b));

    for (name, ty) in locals {
        s.push_str(&format!(".    {} : {}\n", &name, ty.display(context)));
    }
}
