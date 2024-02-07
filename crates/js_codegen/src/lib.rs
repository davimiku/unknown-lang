// TODO: are there any libraries that can codegen JavaScript?
// SWC? something called Oxc Lint or something like that?

mod function;
#[cfg(test)]
mod tests;

use hir::{
    ArrayLiteralExpr, BlockExpr, CallExpr, Context, Expr, FunctionExpr, IfExpr, Type, VarDefExpr,
    VarRefExpr,
};

const INDENT_SIZE: usize = 4;

pub fn codegen(expr: &Expr, context: &Context) -> String {
    let mut code = Codegen::default();
    code.write_expr(expr, None, context);

    code.into()
}

#[derive(Debug, Default)]
struct Codegen {
    code: String,
    indent: usize,
}

impl From<Codegen> for String {
    fn from(val: Codegen) -> Self {
        val.code
    }
}

impl Codegen {
    #[inline(always)]
    fn push<S: AsRef<str>>(&mut self, s: S) {
        self.code.push_str(s.as_ref());
    }

    #[inline(always)]
    fn push_ch(&mut self, ch: char) {
        self.code.push(ch);
    }

    fn write_indent(&mut self) {
        // TODO: pre-compute strings for 1, 2, 3 indents to cover most cases
        // and avoid allocation?
        self.push(" ".repeat(self.indent * INDENT_SIZE));
    }

    fn write_expr(&mut self, expr: &Expr, assign_to: Option<&str>, context: &Context) {
        match expr {
            Expr::Empty => todo!(),
            Expr::BoolLiteral(b) => self.push(b.to_string()),
            Expr::FloatLiteral(f) => self.push(f.to_string()),
            Expr::IntLiteral(i) => self.push(i.to_string()),
            Expr::StringLiteral(key) => self.push(&format!("\"{}\"", context.lookup(*key))),
            Expr::ArrayLiteral(arr) => self.write_array_literal(arr, context),
            Expr::Unary(_) => unreachable!("will be removed, in favor of Call"),
            Expr::Block(block) => self.write_block(block, assign_to, context),
            Expr::Call(call) => self.write_call_expr(call, context),
            Expr::VarRef(var_ref) => self.write_var_ref(var_ref, context),
            Expr::UnresolvedVarRef { .. } => unreachable!(),
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(func) => self.write_function_literal(func, context),
            Expr::VarDef(var_def) => self.write_var_def(var_def, context),
            Expr::If(if_expr) => self.write_if_expr(if_expr, assign_to, context),
            Expr::Statement(inner) => {
                self.write_indent();
                let inner = context.expr(*inner);
                self.write_expr(inner, None, context);
                self.push(";\n");
            }
            Expr::ReturnStatement(inner) => todo!(),
            Expr::Module(exprs) => {
                for idx in exprs {
                    self.write_expr(context.expr(*idx), None, context);
                }
            }
            Expr::Intrinsic(_) => unreachable!("intrinsics won't be part of codegen"),
        };
    }

    fn write_array_literal(&mut self, arr: &ArrayLiteralExpr, context: &Context) {
        self.push_ch('[');
        for element in arr.elements() {
            let expr = context.expr(*element);
            self.write_expr(expr, None, context);
            self.push_ch(',');
        }
        self.push_ch(']');
    }

    fn write_block(&mut self, block: &BlockExpr, assign_to: Option<&str>, context: &Context) {
        self.push("{\n");
        self.indent += 1;
        let last_idx = block.exprs().len().saturating_sub(1);
        for (i, expr) in block.exprs().iter().enumerate() {
            let is_last = last_idx == i;
            let expr = context.expr(*expr);

            if is_last {
                // TODO: use if-let chain #53667
                if let Some(assign_to) = assign_to {
                    self.write_indent();
                    self.push(assign_to);
                    self.push(" = ");
                }
            }
            // FIXME: this writes the indent, which is 2x indent when there
            // is an assign_to
            self.write_expr(expr, None, context);
        }
        self.indent -= 1;
        self.push_ch('}');
    }

    fn write_call_expr(&mut self, call: &CallExpr, context: &Context) {
        let CallExpr { callee, args, .. } = call;

        let callee = context.expr(*callee);
        if let Expr::VarRef(var_ref) = callee {
            match context.lookup_value_symbol(var_ref.symbol) {
                op @ ("+" | "-" | "*" | "/") => {
                    let args: Vec<&Expr> = args.iter().map(|arg| context.expr(*arg)).collect();

                    // TODO: check for Array type and call write_concat
                    self.write_binary(op, &args, context)
                }

                "++" => {
                    let lhs_type = context.expr_type(args[0]);
                    let args: Vec<_> = args.iter().map(|arg| context.expr(*arg)).collect();

                    match lhs_type {
                        Type::StringLiteral(_) | Type::String => {
                            self.write_binary("+", &args, context)
                        }
                        Type::Array(_) => {
                            let subject = args[0];
                            self.write_method_call(
                                subject,
                                "concat",
                                args.into_iter().skip(1),
                                context,
                            )
                        }
                        _ => panic!(
                            "compiler error! expected String or Array types for concat operator"
                        ),
                    }
                }

                _ => {
                    let args = args.iter().map(|arg| context.expr(*arg));
                    self.write_function_call(callee, args, context);
                }
            }
        } else {
            let args = args.iter().map(|a| context.expr(*a));
            self.write_function_call(callee, args, context);
        }
    }

    fn write_function_call<'a>(
        &mut self,
        callee: &Expr,
        args: impl Iterator<Item = &'a Expr>,
        context: &Context,
    ) {
        self.write_expr(callee, None, context);
        self.write_args(args, context);
    }

    fn write_method_call<'a>(
        &mut self,
        subject: &Expr,
        method: &str, // TODO ? could method be a variable/expression?
        args: impl Iterator<Item = &'a Expr>,
        context: &Context,
    ) {
        self.write_expr(subject, None, context);
        self.push_ch('.');
        self.push(method);
        self.write_args(args, context);
    }

    fn write_args<'a>(&mut self, args: impl Iterator<Item = &'a Expr>, context: &Context) {
        self.push_ch('(');
        for arg in args {
            self.write_expr(arg, None, context);
            self.push_ch(',');
        }
        self.push_ch(')');
    }

    /// Binary expressions are represented as function calls in the HIR
    ///
    /// Write these "calls" to the corresponding JavaScript binary expressions
    fn write_binary(&mut self, op: &str, args: &[&Expr], context: &Context) {
        let lhs = args[0];
        let rhs = args[1];

        self.write_expr(lhs, None, context);
        self.push_ch(' ');
        self.push(op);
        self.push_ch(' ');
        self.write_expr(rhs, None, context);
    }

    fn write_concat(&mut self, concat_call: &CallExpr, context: &Context) {
        let CallExpr { callee, args, .. } = concat_call;
        let lhs = context.expr(args[0]);
        let rhs = context.expr(args[1]);

        self.write_expr(lhs, None, context);
        self.push(".concat(");
        self.write_expr(rhs, None, context);
        self.push_ch(')');
    }

    /// Writes a variable reference to the code, which is just the variable name
    fn write_var_ref(&mut self, var_ref: &VarRefExpr, context: &Context) {
        let name = context.lookup_value_symbol(var_ref.symbol);
        self.push(name);
    }

    /// Writes a variable declaration, used often to declare a variable
    /// before a block which gets assigned at the end of the block.
    fn write_var_declaration(&mut self, name: &str) {
        self.push("let ");
        self.push(name);
        self.push(";\n");
    }

    /// Writes a variable definition, which includes both the declaration and
    /// the initial assignment
    fn write_var_def(&mut self, var_def: &VarDefExpr, context: &Context) {
        let name = context.lookup_value_symbol(var_def.symbol);
        self.push("let ");
        self.push(name);

        let value = context.expr(var_def.value);

        // if the value is something that is not an expression in JS, need
        // to do the dance of declaring a variable first, then assigning it
        match value {
            // TODO: is Empty possible?
            Expr::Empty => todo!(),

            // TODO: needs to be parenthesized?
            // Expr::Function(_) => todo!(),

            // TODO: need to check
            // Expr::VarDef(_) => todo!(),
            Expr::Block(_) | Expr::If(_) => {
                self.push(";\n");
                self.write_expr(value, Some(name), context);
            }

            _ => {
                self.push(" = ");
                self.write_expr(value, None, context);
            }
        }
        self.push(";\n");
    }

    fn write_if_expr(&mut self, if_expr: &IfExpr, assign_to: Option<&str>, context: &Context) {
        let IfExpr {
            condition,
            then_branch,
            else_branch,
        } = if_expr;

        // condition
        self.push("if (");
        self.write_expr(context.expr(*condition), None, context);
        self.push(") ");

        // then Block
        self.write_expr(context.expr(*then_branch), assign_to, context);

        // else Block
        if let Some(else_branch) = else_branch {
            self.push(" else ");
            self.write_expr(context.expr(*else_branch), assign_to, context)
        }
    }

    fn write_return_stmt(&mut self, ret: &Expr, context: &Context) {
        self.push("return ");
        self.write_expr(ret, None, context);
    }
}
