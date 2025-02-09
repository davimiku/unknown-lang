use la_arena::Idx;
use util_macros::assert_matches;

use crate::type_expr::{TypeExpr, TypeSymbol};
use crate::{
    ArrayLiteralExpr, BlockExpr, CallExpr, Context, ContextDisplay, Expr, FunctionExpr, IfExpr,
    IndexIntExpr, Type, UnaryExpr, ValueSymbol, VarDefExpr, VarRefExpr, COMPILER_BRAND,
};

use super::{FunctionExprGroup, FunctionParam, LoopExpr, ReAssignment};

const DEFAULT_INDENT: usize = 4;

impl ContextDisplay for Idx<Expr> {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        let indent = 0;
        fmt_idx_expr(&mut s, *self, context, indent);

        s
    }
}

impl ContextDisplay for Expr {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        let indent = 0;
        fmt_expr(&mut s, self, context, indent);

        s
    }
}

fn fmt_idx_expr(s: &mut String, idx: Idx<Expr>, context: &Context, indent: usize) {
    let expr = context.expr(idx);
    fmt_expr(s, expr, context, indent);
}

/// Formats a given expression into a String representation
///
/// This format is not stable and should not be used in machine parsing. It is
/// meant to be read and understood by humans, and may be used in some internal test cases.
///
/// This function is often called recursively for expressions that nest other expressions.
fn fmt_expr(s: &mut String, expr: &Expr, context: &Context, indent: usize) {
    let mut indent = indent;
    match expr {
        Expr::Empty => {}
        Expr::Statement(expr_idx) => {
            fmt_idx_expr(s, *expr_idx, context, indent);
            s.push(';');
        }
        Expr::BreakStatement(break_value) => {
            s.push_str("break");
            if !matches!(context.expr(*break_value), Expr::Empty) {
                s.push(' ');
            }
            fmt_idx_expr(s, *break_value, context, indent);
        }
        Expr::ReturnStatement(return_value) => {
            s.push_str("return ");
            fmt_idx_expr(s, *return_value, context, indent);
        }

        Expr::FloatLiteral(f) => {
            let mut buffer = ryu::Buffer::new();
            s.push_str(buffer.format_finite(*f))
        }
        Expr::IntLiteral(i) => s.push_str(&i.to_string()),
        Expr::StringLiteral(key) => s.push_str(&format!(r#""{}""#, context.lookup(*key))),
        Expr::ArrayLiteral(array_expr) => fmt_array_literal(s, array_expr, context, indent),

        Expr::Call(call) => fmt_call_expr(s, call, context, indent),
        Expr::Unary(unary) => fmt_unary_expr(s, unary, context, indent),
        Expr::Block(block_expr) => fmt_block_expr(s, block_expr, context, &mut indent),

        Expr::VarRef(var_ref) => s.push_str(&var_ref.display(context)),

        Expr::UnresolvedVarRef { key } => {
            s.push_str(&format!("<undefined {}>", context.lookup(*key)))
        }

        Expr::Function(function) => fmt_function_expr_group(s, function, context, indent),
        Expr::VarDef(local_def) => fmt_var_def(s, local_def, context, indent),
        Expr::ReAssignment(reassignment) => fmt_reassignment(s, reassignment, context, indent),

        Expr::Match(match_expr) => todo!(),
        Expr::If(if_expr) => fmt_if_expr(s, if_expr, context, indent),
        Expr::Loop(loop_expr) => fmt_loop_expr(s, loop_expr, context, &mut indent),
        Expr::Path(_) => todo!(),
        Expr::IndexInt(index_expr) => fmt_index_int_expr(s, index_expr, context, indent),
        Expr::TypeStatement(.., type_expr) => {
            s.push_str(&type_expr.display(context).to_string());
        }
    }
}

fn fmt_array_literal(s: &mut String, array: &ArrayLiteralExpr, context: &Context, indent: usize) {
    match array {
        ArrayLiteralExpr::Empty => s.push_str("[]"),
        ArrayLiteralExpr::NonEmpty { elements } => {
            s.push('[');
            for element in elements {
                fmt_idx_expr(s, *element, context, indent);
                s.push(',');
            }
            s.push(']');
        }
    }
}

fn fmt_call_expr(s: &mut String, call: &CallExpr, context: &Context, indent: usize) {
    let CallExpr {
        callee,
        args,
        signature_index: resolved_signature,
        ..
    } = call;
    fmt_idx_expr(s, *callee, context, indent);
    if let Some(sig) = resolved_signature {
        s.push_str(&format!("${sig}"));
    }

    s.push(' ');
    s.push('(');
    for arg in args.iter() {
        fmt_idx_expr(s, *arg, context, indent);
        s.push(',');
    }
    s.push(')');
}

fn fmt_unary_expr(s: &mut String, unary: &UnaryExpr, context: &Context, indent: usize) {
    let UnaryExpr { op, expr, .. } = unary;
    s.push_str(&format!("{op}"));
    fmt_idx_expr(s, *expr, context, indent)
}

fn fmt_block_expr(s: &mut String, block: &BlockExpr, context: &Context, indent: &mut usize) {
    match block {
        BlockExpr::Empty => s.push_str("{}"),
        BlockExpr::NonEmpty { exprs } => {
            if exprs.len() == 1 {
                s.push_str("{ ");
                fmt_idx_expr(s, exprs[0], context, *indent);
                s.push_str(" }");
            } else {
                s.push_str("{\n");
                *indent += DEFAULT_INDENT;
                for idx in exprs {
                    s.push_str(&" ".repeat(*indent));
                    fmt_idx_expr(s, *idx, context, *indent);
                    s.push('\n');
                }
                *indent -= DEFAULT_INDENT;
                s.push_str(&format!("{}}}", " ".repeat(*indent)));
            }
        }
    }
}

fn fmt_function_expr_group(
    s: &mut String,
    function_group: &FunctionExprGroup,
    context: &Context,
    _: usize,
) {
    let FunctionExprGroup {
        overloads,
        name,
        entry_point: _, // TODO: display this?
    } = function_group;
    s.push_str("fun ");
    if let Some((key, ..)) = name {
        s.push('"');
        s.push_str(context.lookup(*key));
        s.push('"');
    }

    let is_overloaded = overloads.len() > 1;
    if is_overloaded {
        s.push('\n');
    }
    for (i, function) in overloads.iter().enumerate() {
        if is_overloaded {
            s.push_str(&i.to_string());
            s.push_str("| ");
        }
        s.push_str(&function.display(context));
    }
}

fn fmt_function_expr(s: &mut String, function: &FunctionExpr, context: &Context, indent: usize) {
    let FunctionExpr { params, body, .. } = function;

    s.push('(');
    for param in params.iter() {
        s.push_str(&param.symbol.display(context));
        s.push_str(" : ");
        match param.annotation {
            Some(type_expr) => {
                let ty = context.type_of_type_expr(type_expr);
                s.push_str(&ty.display(context));
            }
            None => s.push_str("{empty}"),
        }
    }
    s.push_str(") -> ");
    let body_ty = context.expr_type(*body);
    if matches!(body_ty, Type::Unit) {
        // write nothing for Unit return
    } else {
        let body_ty = body_ty.display(context);
        s.push_str(&body_ty);
        s.push(' ');
    }
    fmt_idx_expr(s, *body, context, indent);
}

fn fmt_var_def(s: &mut String, local_def: &VarDefExpr, context: &Context, indent: usize) {
    let VarDefExpr {
        symbol,
        value,
        type_annotation,
    } = local_def;
    let mutability = context.mutability_of(symbol);
    let type_buffer = if let Some(type_annotation) = type_annotation {
        type_annotation.display(context)
    } else {
        context.expr_type(*value).display(context)
    };
    s.push_str(&format!(
        "{} : {}{} = ",
        &symbol.display(context),
        mutability,
        type_buffer
    ));
    fmt_idx_expr(s, *value, context, indent);
    s.push(';');
}

fn fmt_reassignment(s: &mut String, reassignment: &ReAssignment, context: &Context, indent: usize) {
    fmt_idx_expr(s, reassignment.place, context, indent);
    s.push_str(" <- ");
    fmt_idx_expr(s, reassignment.value, context, indent);
}

fn fmt_if_expr(s: &mut String, if_expr: &IfExpr, context: &Context, indent: usize) {
    let IfExpr {
        condition,
        then_branch,
        else_branch,
    } = if_expr;

    s.push_str("if (");
    fmt_idx_expr(s, *condition, context, indent);
    s.push_str(") ");
    fmt_idx_expr(s, *then_branch, context, indent);
    if let Some(else_branch) = else_branch {
        s.push_str(" else ");
        fmt_idx_expr(s, *else_branch, context, indent)
    }
}

fn fmt_loop_expr(s: &mut String, loop_expr: &LoopExpr, context: &Context, indent: &mut usize) {
    s.push_str("loop ");
    let body = context.expr(loop_expr.body);
    let block = assert_matches!(body, Expr::Block);
    fmt_block_expr(s, block, context, indent);
}

fn fmt_index_int_expr(s: &mut String, index_expr: &IndexIntExpr, context: &Context, indent: usize) {
    let IndexIntExpr { subject, index } = index_expr;
    fmt_idx_expr(s, *subject, context, indent);
    s.push('.');
    fmt_idx_expr(s, *index, context, indent);
}

fn fmt_type_statement(
    s: &mut String,
    symbol: TypeSymbol,
    type_expr: Idx<TypeExpr>,
    context: &Context,
    indent: usize,
) {
    s.push_str(&type_expr.display(context).to_string());
}

// impl ContextDisplay for CallExprSignature {
//     fn display(&self, _: &Context) -> String {
//         match self {
//             CallExprSignature::Unresolved => String::from("<?>"),
//             CallExprSignature::ResolvedOnly => String::new(),
//             CallExprSignature::Resolved(i) => format!("<{i}>"),
//         }
//     }
// }

impl ContextDisplay for ValueSymbol {
    fn display(&self, context: &Context) -> String {
        let name = context.lookup(context.database.value_names[self]);
        let name = match name {
            s @ ("+" | "-" | "*" | "/" | "++" | "==" | "!=" | "<" | "<=" | ">" | ">=") => {
                format!("`{s}`")
            }
            s => s.to_owned(),
        };
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

impl ContextDisplay for FunctionExprGroup {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        fmt_function_expr_group(&mut s, self, context, 0);
        s
    }
}

impl ContextDisplay for FunctionExpr {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        fmt_function_expr(&mut s, self, context, 0);
        s
    }
}

impl ContextDisplay for FunctionParam {
    fn display(&self, context: &Context) -> String {
        let mut s = String::new();
        // s.push_str(context.lookup(self.name));
        s.push_str(&self.symbol.display(context));

        // s.push_str(": ");
        // TODO: print annotation type expression

        s
    }
}
