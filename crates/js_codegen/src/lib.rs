#[cfg(test)]
mod tests;

use hir::{
    ArrayLiteralExpr, BlockExpr, CallExpr, Context, Expr, FunctionExpr, IfExpr, Type, VarDefExpr,
    VarRefExpr,
};

pub fn codegen(expr: &Expr, context: &Context) -> String {
    let mut code = Code::default();
    code.write_expr(expr, None, context);

    code.into()
}

#[derive(Debug, Default)]
struct Code(String);

impl From<Code> for String {
    fn from(val: Code) -> Self {
        val.0
    }
}

impl Code {
    #[inline(always)]
    fn push(&mut self, s: &str) {
        self.0.push_str(s);
    }

    #[inline(always)]
    fn push_ch(&mut self, ch: char) {
        self.0.push(ch);
    }

    fn write_expr(&mut self, expr: &Expr, assign_to: Option<&str>, context: &Context) {
        match expr {
            Expr::Empty => todo!(),
            Expr::BoolLiteral(b) => self.push(&b.to_string()),
            Expr::FloatLiteral(f) => self.push(&f.to_string()),
            Expr::IntLiteral(i) => self.push(&i.to_string()),
            Expr::StringLiteral(key) => self.push(&format!("\"{}\"", context.lookup(*key))),
            Expr::ArrayLiteral(arr) => self.write_array_literal(arr, context),
            Expr::Unary(_) => unreachable!("will be removed, in favor of Call"),
            Expr::Block(block) => self.write_block(block, None, context),
            Expr::Call(call) => self.write_call_expr(call, context),
            Expr::VarRef(var_ref) => self.write_var_ref(var_ref, context),
            Expr::UnresolvedVarRef { .. } => unreachable!(),
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(func) => self.write_function_literal(func, context),
            Expr::VarDef(var_def) => self.write_var_def(var_def, context),
            Expr::If(if_expr) => self.write_if_expr(if_expr, assign_to, context),
            Expr::Statement(inner) => {
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
        self.push_ch('{');
        let last_idx = block.exprs().len() - 1;
        for (i, expr) in block.exprs().iter().enumerate() {
            let expr = context.expr(*expr);

            if last_idx == i {
                // TODO: Rust issue tracker #53667
                if let Some(assign_to) = assign_to {
                    self.push(assign_to);
                    self.push(" = ");
                }
            }
            self.write_expr(expr, None, context);
            self.push(";\n");
        }
        self.push_ch('}');
    }

    fn write_call_expr(&mut self, call: &CallExpr, context: &Context) {
        let CallExpr { callee, args, .. } = call;

        let callee = context.expr(*callee);
        if let Expr::VarRef(var_ref) = callee {
            match context.lookup_value_symbol(var_ref.symbol) {
                mut op @ ("+" | "++" | "-" | "*" | "/") => {
                    let arg_type = context.expr_type(args[0]);
                    let args: Vec<&Expr> = args.iter().map(|arg| context.expr(*arg)).collect();

                    if op == "++" && matches!(arg_type, Type::String | Type::StringLiteral(_)) {
                        // JS uses `+` for string concatenation
                        op = "+";
                    }
                    // TODO: check for Array type and call write_concat
                    self.write_binary(op, &args, context)
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
        self.push_ch(' ');
        self.push(name);
        self.push_ch(' ');
    }

    /// Writes a function literal expression
    ///
    /// Often this is called via the "variable definition" procedure but function expressions
    /// can also exist on their own (can be immediately invoked - IIFE)
    fn write_function_literal(&mut self, func: &FunctionExpr, context: &Context) {
        let FunctionExpr { params, body, name } = func;
        self.push("(function ");
        if let Some(key) = name {
            let name = context.lookup(*key);
            self.push(name);
        }

        // params
        self.push_ch('(');
        for param in params {
            let name = context.lookup(param.name);
            self.push(name);
            self.push_ch(',');
        }
        self.push_ch(')');

        // body
        self.write_function_body(context.expr(*body), context);
        self.push_ch('\n');
    }

    fn write_function_body(&mut self, body: &Expr, context: &Context) {
        match body {
            Expr::Empty => self.push("{}"), // unreachable?
            Expr::UnresolvedVarRef { .. } => unreachable!(),
            Expr::Block(block) => self.write_function_body_block(block, context),
            Expr::ReturnStatement(ret) => self.write_return_stmt(context.expr(*ret), context),
            Expr::VarDef(_) => self.push("{}"), // "optimization" TODO: should we write this out anyways?
            Expr::Unary(_) => unreachable!("replacing with Call"),

            expr => {
                self.push("return ");
                self.write_expr(body, None, context);
            }
            Expr::IndexInt(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Statement(_) => todo!(),
        }
    }

    fn write_function_body_block(&mut self, block: &BlockExpr, context: &Context) {
        self.push_ch('{');
        let last = block.exprs().len() - 1;
        for (i, expr) in block.exprs().iter().enumerate() {
            if i == last {
                self.push("return ");
            }

            let expr = context.expr(*expr);
            self.write_expr(expr, None, context);
            self.push(";\n");
        }
        self.push_ch('}');
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
        self.push(" = ");

        let value = context.expr(var_def.value);
        self.write_expr(value, Some(name), context);
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
        self.push_ch(')');

        // then
        // TODO: pass through "assign_to"
        self.write_expr(context.expr(*then_branch), assign_to, context);

        // else
        if let Some(else_branch) = else_branch {
            self.write_expr(context.expr(*else_branch), assign_to, context)
        }
    }

    fn write_return_stmt(&mut self, ret: &Expr, context: &Context) {
        self.push("return ");
        self.write_expr(ret, None, context);
    }
}
