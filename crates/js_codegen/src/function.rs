use hir::{BlockExpr, Context, Expr, FunctionExpr, FunctionExprGroup};

use crate::Codegen;

impl Codegen {
    /// Writes a function literal expression
    ///
    /// Often this is called via the "variable definition" procedure but function expressions
    /// can also exist on their own (can be immediately invoked - IIFE)
    pub(super) fn write_function_literal(
        &mut self,
        func_group: &FunctionExprGroup,
        context: &Context,
    ) {
        let func = &func_group.overloads[0];
        let FunctionExpr { params, body, .. } = func;
        self.push("(function ");
        if let Some((key, ..)) = func_group.name {
            let name = context.lookup(key);
            self.push(name);
        }

        // params
        self.push_ch('(');
        for param in params.iter() {
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

            Expr::IndexInt(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Statement(_) => todo!(),
            expr => {
                panic!("unexpected expr {expr:?}");
            }
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
}
