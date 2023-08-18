use la_arena::Idx;

use crate::lowering_context::{Context, ContextDisplay};
use crate::{
    ArrayLiteralExpr, BinaryExpr, BlockExpr, CallExpr, Expr, FunctionExpr, IfExpr, IndexIntExpr,
    UnaryExpr, ValueSymbol, VarDefExpr, VarRefExpr, COMPILER_BRAND,
};

const DEFAULT_INDENT: usize = 4;

impl ContextDisplay for Idx<Expr> {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        let indent = 0;
        fmt_expr(&mut s, *self, context, indent);

        s
    }
}

/// Formats a given expression into a String representation
///
/// This format is not stable and should not be used in machine parsing. It is
/// meant to be read and understood by humans, and may be used in some non-permanent test cases.
///
/// This function is often called recursively for expressions that nest other expressions.
fn fmt_expr(s: &mut String, idx: Idx<Expr>, context: &Context, indent: usize) {
    let mut indent = indent;
    let expr = context.expr(idx);
    match expr {
        Expr::Empty => s.push_str("{{empty}}"),
        Expr::Statement(expr_idx) => {
            fmt_expr(s, *expr_idx, context, indent);
            s.push(';');
        }
        Expr::ReturnStatement(return_value) => {
            s.push_str("return ");
            fmt_expr(s, *return_value, context, indent);
        }

        Expr::BoolLiteral(b) => s.push_str(&b.to_string()),
        Expr::FloatLiteral(f) => s.push_str(&f.to_string()),
        Expr::IntLiteral(i) => s.push_str(&i.to_string()),
        Expr::StringLiteral(key) => s.push_str(&format!(r#""{}""#, context.lookup(*key))),
        Expr::ArrayLiteral(array_expr) => fmt_array_literal(s, array_expr, context, indent),

        Expr::Call(call) => fmt_call_expr(s, call, context, indent),
        Expr::Binary(binary) => fmt_binary_expr(s, binary, context, indent),
        Expr::Unary(unary) => fmt_unary_expr(s, unary, context, indent),
        Expr::EmptyBlock => s.push_str("{}"),
        Expr::Block(block_expr) => fmt_block_expr(s, block_expr, context, &mut indent),

        Expr::VarRef(var_ref) => s.push_str(&var_ref.display(context)),

        Expr::UnresolvedVarRef { key } => {
            s.push_str(&format!("<undefined {}>", context.lookup(*key)))
        }

        Expr::Function(function) => fmt_function_expr(s, function, context, indent),
        Expr::VarDef(local_def) => fmt_var_def(s, local_def, context, indent),

        Expr::If(if_expr) => fmt_if_expr(s, if_expr, context, indent),
        Expr::Path(_) => todo!(),
        Expr::IndexInt(index_expr) => fmt_index_int_expr(s, index_expr, context, indent),
    }
}

fn fmt_array_literal(s: &mut String, array: &ArrayLiteralExpr, context: &Context, indent: usize) {
    match array {
        ArrayLiteralExpr::Empty => s.push_str("[]"),
        ArrayLiteralExpr::NonEmpty { elements } => {
            s.push('[');
            for element in elements {
                fmt_expr(s, *element, context, indent);
                s.push(',');
            }
            s.push(']');
        }
    }
}

fn fmt_call_expr(s: &mut String, call: &CallExpr, context: &Context, indent: usize) {
    let CallExpr { callee, args, .. } = call;
    fmt_expr(s, *callee, context, indent);
    s.push(' ');
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
    let FunctionExpr { params, body, name } = function;
    s.push_str("fun");
    if let Some(key) = name {
        let name = context.lookup(*key);
        s.push('<');
        s.push_str(name);
        s.push('>');
    }
    s.push_str(" (");
    for param in params {
        s.push_str(&param.name.display(context));
        s.push_str(" : ");
        match param.annotation {
            Some(ty) => {
                let ty = context.type_database.get_type_expr_type(ty);
                s.push_str(&ty.display(context));
            }
            None => s.push_str("~empty~"),
        }
    }
    s.push_str(") -> ");
    fmt_expr(s, *body, context, indent);
}

fn fmt_var_def(s: &mut String, local_def: &VarDefExpr, context: &Context, indent: usize) {
    let VarDefExpr {
        symbol: key,
        value,
        type_annotation,
    } = local_def;
    let type_buffer = if let Some(type_annotation) = type_annotation {
        type_annotation.display(context)
    } else {
        context.borrow_expr_type(*value).display(context)
    };
    s.push_str(&format!("{} : {} = ", &key.display(context), type_buffer));
    fmt_expr(s, *value, context, indent);
    s.push(';');
}

fn fmt_if_expr(s: &mut String, if_expr: &IfExpr, context: &Context, indent: usize) {
    let IfExpr {
        condition,
        then_branch,
        else_branch,
    } = if_expr;

    s.push_str("if (");
    fmt_expr(s, *condition, context, indent);
    s.push_str(") ");
    fmt_expr(s, *then_branch, context, indent);
    if let Some(else_branch) = else_branch {
        s.push_str(" else ");
        fmt_expr(s, *else_branch, context, indent)
    }
}

fn fmt_index_int_expr(s: &mut String, index_expr: &IndexIntExpr, context: &Context, indent: usize) {
    let IndexIntExpr { subject, index } = index_expr;
    fmt_expr(s, *subject, context, indent);
    s.push('.');
    fmt_expr(s, *index, context, indent);
}

impl ContextDisplay for ValueSymbol {
    fn display(&self, context: &Context) -> String {
        let name = context.lookup(context.database.value_names[self]);
        let ValueSymbol {
            symbol_id,
            module_id,
        } = self;
        format!("{name}{COMPILER_BRAND}{module_id}.{symbol_id}")
    }
}

impl ContextDisplay for VarRefExpr {
    fn display(&self, context: &Context) -> String {
        self.symbol.display(context)
    }
}
