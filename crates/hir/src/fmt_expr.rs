use itertools::Itertools;
use la_arena::Idx;

use crate::expr::{
    BinaryExpr, BlockExpr, CallExpr, FunctionExpr, LocalDefExpr, LocalRefExpr, LocalRefName,
    UnaryExpr,
};
use crate::interner::Interner;
use crate::type_expr::{LocalTypeRefExpr, LocalTypeRefName, TypeExpr};
use crate::typecheck::fmt_local_types;
use crate::{Context, Expr, Type};

const DEFAULT_INDENT: usize = 4;

/// Formats an expression into a String representation
///
/// This format is not stable and should not be used in machine parsing. It is
/// meant to be read and understood by humans, and may be used in some test cases.
pub(crate) fn fmt_root(idx: Idx<Expr>, context: &Context) -> String {
    let mut s = String::new();
    fmt_expr(&mut s, idx, context, 0);

    fmt_local_types(&mut s, &context.type_database, &context.interner);

    s
}

pub fn fmt_expr(s: &mut String, idx: Idx<Expr>, context: &Context, indent: usize) {
    let mut indent = indent;
    let expr = context.expr(idx);
    match expr {
        Expr::Empty => s.push_str("{{empty}}"),

        Expr::BoolLiteral(b) => s.push_str(&b.to_string()),
        Expr::FloatLiteral(f) => s.push_str(&f.to_string()),
        Expr::IntLiteral(i) => s.push_str(&i.to_string()),
        Expr::StringLiteral(key) => s.push_str(&format!(r#""{}""#, context.interner.lookup(*key))),

        Expr::Call(call) => fmt_call_expr(s, call, context, indent),
        Expr::Binary(binary) => fmt_binary_expr(s, binary, context, indent),
        Expr::Unary(unary) => fmt_unary_expr(s, unary, context, indent),
        Expr::Block(block_expr) => fmt_block_expr(s, block_expr, context, &mut indent),

        Expr::LocalRef(LocalRefExpr { name }) => match name {
            LocalRefName::Resolved(key) => s.push_str(&key.display(&context.interner)),
            LocalRefName::Unresolved(name) => s.push_str(context.interner.lookup(*name)),
        },

        Expr::Function(function) => fmt_function_expr(s, function, context, indent),
        Expr::LocalDef(local_def) => fmt_local_def(s, local_def, context, indent),

        Expr::If(if_expr) => {
            todo!()
        }
    }
}

fn fmt_call_expr(s: &mut String, call: &CallExpr, context: &Context, indent: usize) {
    let CallExpr { path, args } = call;
    s.push_str(&format!("{path} "));
    s.push('(');
    for arg in args {
        fmt_expr(s, *arg, context, indent);
        s.push(',');
    }
    s.push(')');
}

fn fmt_binary_expr(s: &mut String, binary: &BinaryExpr, context: &Context, indent: usize) {
    let BinaryExpr { op, lhs, rhs, .. } = binary;
    fmt_expr(s, *lhs, context, indent);
    s.push_str(&format!(" {op} "));
    fmt_expr(s, *rhs, context, indent)
}

fn fmt_unary_expr(s: &mut String, unary: &UnaryExpr, context: &Context, indent: usize) {
    let UnaryExpr { op, expr, .. } = unary;
    s.push_str(&format!("{op}"));
    fmt_expr(s, *expr, context, indent)
}

fn fmt_block_expr(s: &mut String, block: &BlockExpr, context: &Context, indent: &mut usize) {
    let BlockExpr { exprs } = block;
    s.push_str("{\n");
    *indent += DEFAULT_INDENT;
    for idx in exprs {
        s.push_str(&" ".repeat(*indent));
        fmt_expr(s, *idx, context, *indent);
        s.push('\n');
    }
    *indent -= DEFAULT_INDENT;
    s.push_str(&format!("{}}}", " ".repeat(*indent)));
}

fn fmt_function_expr(s: &mut String, function: &FunctionExpr, context: &Context, indent: usize) {
    let FunctionExpr { params, body } = function;
    s.push_str("fun (");
    for param in params {
        s.push_str(&param.name.display(context.interner));
        s.push_str(" : ");
        match param.ty {
            Some(ty) => {
                let ty = context
                    .type_database
                    .get_type_expr_type(ty)
                    .expect("TODO: is this possible?");
                s.push_str(&fmt_type(ty, context.interner));
            }
            None => s.push_str("~empty~"),
        }
    }
    s.push_str(") -> ");

    fmt_expr(s, *body, context, indent)
}

fn fmt_local_def(s: &mut String, local_def: &LocalDefExpr, context: &Context, indent: usize) {
    let LocalDefExpr {
        key,
        value,
        type_annotation,
    } = local_def;
    let mut type_buffer = String::new();
    if let Some(type_annotation) = type_annotation {
        fmt_type_expr(&mut type_buffer, *type_annotation, context, indent);
    } else {
        let formatted_type = fmt_type(context.type_of_expr(*value), &context.interner);
        type_buffer.push_str(&formatted_type);
    }
    s.push_str(&format!(
        "{} : {} = ",
        &key.display(&context.interner),
        type_buffer
    ));
    fmt_expr(s, *value, context, indent);
}

pub(crate) fn fmt_type_expr(s: &mut String, idx: Idx<TypeExpr>, context: &Context, indent: usize) {
    let mut indent = indent;
    let expr = context.type_expr(idx);
    match expr {
        TypeExpr::Empty => s.push_str("{{empty}}"),

        TypeExpr::BoolLiteral(b) => s.push_str(&b.to_string()),
        TypeExpr::FloatLiteral(f) => s.push_str(&f.to_string()),
        TypeExpr::IntLiteral(i) => s.push_str(&i.to_string()),
        TypeExpr::StringLiteral(key) => {
            s.push_str(&format!(r#""{}""#, context.interner.lookup(*key)))
        }
        TypeExpr::Call(_) => todo!(),

        TypeExpr::LocalDef(_) => todo!(),
        TypeExpr::LocalRef(local_ref) => {
            let LocalTypeRefExpr { name } = local_ref;
            let type_name = match name {
                LocalTypeRefName::Resolved(name) => name.display(context.interner),
                LocalTypeRefName::Unresolved(name) => context.interner.lookup(*name).to_owned(),
            };

            s.push_str(&type_name);
        }
        TypeExpr::Binary(_) => todo!(),
        TypeExpr::Unary(_) => todo!(),
    }
}

pub(crate) fn fmt_type(ty: &Type, interner: &Interner) -> String {
    match ty {
        Type::Bool => "Bool".to_string(),
        Type::BoolLiteral(b) => b.to_string(),
        Type::Float => "Float".to_string(),
        Type::FloatLiteral(f) => f.to_string(),
        Type::Int => "Int".to_string(),
        Type::IntLiteral(i) => i.to_string(),
        Type::String => "String".to_string(),
        Type::StringLiteral(s) => format!("\"{}\"", interner.lookup(*s)),

        Type::Function { params, return_ty } => {
            let mut s = String::new();

            s.push('(');
            let params = params
                .iter()
                .map(|param| fmt_type(param, interner))
                .join(", ");
            s.push_str(&params);
            s.push_str(") -> ");
            s.push_str(&fmt_type(return_ty, interner));

            s
        }
        Type::Named(name) => interner.lookup(*name).to_string(),

        Type::Unit => "Unit".to_string(),
        Type::Top => "Top".to_string(),
        Type::Bottom => "Bottom".to_string(),

        Type::Undetermined => "Undetermined".to_string(),
        Type::Error => "Error".to_string(),
    }
}
