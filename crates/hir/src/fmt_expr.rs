use la_arena::Idx;

use crate::{
    interner::Interner, typecheck::fmt_local_types, BinaryExpr, BlockExpr, CallExpr, Context, Expr,
    FunctionExpr, LocalDef, LocalRef, LocalRefName, Type, UnaryExpr,
};

const DEFAULT_INDENT: usize = 4;

/// Formats an expression into a String representation
///
/// This format is not stable and should not be used in machine parsing. It is
/// meant to be read and understood by humans, and may be used in some test cases.
pub(crate) fn fmt_root(idx: Idx<Expr>, context: &Context) -> String {
    let mut s = String::new();
    fmt_expr(&mut s, idx, context, 0);

    fmt_local_types(&mut s, &context.typecheck_results, &context.interner);

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

        Expr::Call(CallExpr { path, args }) => {
            s.push_str(&format!("{path} "));

            s.push('(');
            for arg in args {
                fmt_expr(s, *arg, context, indent);
                s.push(',');
            }
            s.push(')');
        }

        Expr::Binary(BinaryExpr { op, lhs, rhs, .. }) => {
            fmt_expr(s, *lhs, context, indent);

            s.push_str(&format!(" {op} "));

            fmt_expr(s, *rhs, context, indent)
        }

        Expr::Unary(UnaryExpr { op, expr, .. }) => {
            s.push_str(&format!("{op}"));

            fmt_expr(s, *expr, context, indent)
        }

        Expr::Block(BlockExpr { exprs, .. }) => {
            s.push_str("{\n");

            indent += DEFAULT_INDENT;

            for idx in exprs {
                s.push_str(&" ".repeat(indent));
                fmt_expr(s, *idx, context, indent);
                s.push('\n');
            }

            indent -= DEFAULT_INDENT;

            s.push_str(&format!("{}}}", " ".repeat(indent)));
        }

        Expr::LocalRef(LocalRef { name }) => match name {
            LocalRefName::Resolved(key) => s.push_str(&key.display(&context.interner)),
            LocalRefName::Unresolved(name) => s.push_str(context.interner.lookup(*name)),
        },

        Expr::Function(FunctionExpr {
            params,
            body,
            return_type_annotation,
        }) => {
            s.push_str("fun (");

            for param in params {
                fmt_expr(s, *param, context, indent);
                s.push(' ');
            }

            s.push_str(") -> ");

            if let Some(return_type) = return_type_annotation {
                fmt_expr(s, *return_type, context, indent);
            }

            fmt_expr(s, *body, context, indent)
        }

        Expr::LocalDef(LocalDef {
            key,
            value,
            type_annotation,
        }) => {
            let mut type_buffer = String::new();
            if let Some(type_annotation) = type_annotation {
                fmt_expr(&mut type_buffer, *type_annotation, context, indent);
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

        Expr::If(if_expr) => {
            todo!()
        }
    }
}

pub(crate) fn fmt_type(ty: &Type, interner: &Interner) -> String {
    match ty {
        Type::Undetermined => "~Undetermined~".to_owned(),
        Type::Error => "~Error~".to_owned(),

        Type::Unit => "Unit".to_owned(),

        Type::BoolLiteral(b) => format!("{b}"),
        Type::FloatLiteral(f) => format!("{f}"),
        Type::IntLiteral(i) => format!("{i}"),
        Type::StringLiteral(key) => interner.lookup(*key).to_owned(),

        Type::Bool => "Bool".to_owned(),
        Type::Float => "Float".to_owned(),
        Type::Int => "Int".to_owned(),
        Type::String => "String".to_owned(),

        Type::Named(name) => interner.lookup(*name).to_owned(),
    }
}
