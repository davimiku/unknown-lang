//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{CallExpr, Context, Expr, FunctionParam, Type, ValueSymbol, VarDefExpr};
use la_arena::Idx;

use crate::syntax::{
    BasicBlock, BinOp, Constant, Local, Mutability, Operand, Place, Rvalue, Statement, Terminator,
};
use crate::{Builder, Function};
use util_macros::assert_matches;

// TODO: need a naming convention for mutating vs. non-mutating functions

impl Builder {
    // construct into existing allocated Function
    pub(crate) fn construct_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
        context: &Context,
    ) -> Idx<Function> {
        let func = self.current_function_mut();
        func.name = function_expr.name;

        let return_ty = context.expr_type_idx(function_expr.body);
        self.make_local(return_ty, None, Mutability::Mut);

        for param in function_expr.params.iter() {
            self.construct_param(param, context);
        }

        let body = assert_matches!(context.expr(function_expr.body), Expr::Block);
        let statements = body.exprs();

        let return_place = Some(self.current_function().return_place());
        self.construct_block(statements, return_place, context);

        self.current_block_mut().terminator = Terminator::Return;

        self.current_function
    }

    fn construct_param(&mut self, param: &FunctionParam, context: &Context) {
        let ty_idx = context.type_idx_of_value(&param.symbol);
        self.current_function_mut().params.push(ty_idx);
        let ty = context.type_idx_of_value(&param.symbol);
        self.make_local(ty, Some(param.symbol), Mutability::Not); // TODO: depends on the param
    }

    fn new_block(&mut self) -> Idx<BasicBlock> {
        self.current_block = self.current_function_mut().new_block();
        self.current_block
    }

    /// Constructs the provided statements into the current Basic Block
    // FIXME: how do we capture the `place` of `let place = { ... }`
    fn construct_block(
        &mut self,
        statements: &[Idx<Expr>],
        assign_place: Option<Place>,
        context: &Context,
    ) {
        let statements_iter = statements
            .iter()
            .enumerate()
            .map(|(i, el)| (el, i == statements.len() - 1));

        for (hir_statement, is_last) in statements_iter {
            let return_place = if is_last {
                if self.scope_depth == 0 {
                    Some(self.current_function().return_place())
                } else {
                    todo!("capture the assignplace of `let a = {{ ... }}`")
                }
            } else {
                None
            };
            self.construct_statement_like(*hir_statement, context, return_place);
        }
    }

    // TODO: Statements that boil down to side-effect free stuff can be
    // removed.
    // Do it as an optimization pass to set up the infrastructure for such passes
    // and have a good / straightforward use case to start with
    //
    // example: statement `1 + 2` can be optimized out because it's not being assigned
    // and makes no side-effects. TODO: can purity be inferred? for builtins at least
    // ```
    // 1 + 2 // --> call: `+`(1, 2)
    // ```
    fn construct_statement_like(
        &mut self,
        statement: Idx<Expr>,
        context: &Context,
        assign_place: Option<Place>,
    ) {
        let statement = context.expr(statement);

        if let Expr::VarDef(var_def) = statement {
            self.construct_var_def(var_def, context);
        } else {
            let statement = assert_matches!(statement, Expr::Statement);
            self.construct_statement(*statement, context, assign_place)
        }
    }

    fn construct_var_def(&mut self, var_def: &VarDefExpr, context: &Context) {
        // TODO: capture mutability in parser, AST, HIR, and through here
        let ty = context.expr_type_idx(var_def.value);
        let symbol = Some(var_def.symbol);

        // FIXME: var_def.value could be a call
        // which would be constructed as a Terminator, not Assign
        let rvalue = self.construct_rvalue(context.expr(var_def.value), context);
        self.construct_assign(rvalue, ty, symbol);
    }

    fn construct_statement(
        &mut self,
        statement_expr: Idx<Expr>,
        context: &Context,
        // TODO: decide if this is the best way to indicate when the statement is the "return value" of the function/block
        assign_place: Option<Place>,
    ) {
        let statement_expr = context.expr(statement_expr);

        match statement_expr {
            Expr::BoolLiteral(b) => {
                self.construct_statement_constant(assign_place, Constant::bool(*b))
            }

            Expr::FloatLiteral(f) => {
                self.construct_statement_constant(assign_place, Constant::Float(*f))
            }

            Expr::StringLiteral(s) => {
                self.construct_statement_constant(assign_place, Constant::String(*s))
            }

            Expr::IntLiteral(i) => {
                self.construct_statement_constant(assign_place, Constant::Int(*i))
            }

            // can't ignore because elements may cause side-effects
            // ex. [do_thing (), do_thing (), do_thing ()]
            // couldn't be optimized away unless do_thing could be proven to
            // be side-effect free
            // TODO: optimization pass to remove if elements are literals
            Expr::ArrayLiteral(_) => todo!(),

            // TODO: will be removed in favor of Call
            Expr::Unary(_) => todo!(),

            Expr::Block(block) => {
                // FIXME: new_block terminator needs to be set to the next block
                self.construct_block(block.exprs(), assign_place, context);
                // TODO: there could be a return value here
                // fun () => {
                //     // statements...
                //     {
                //        // statements...
                //        return_val
                //     }
                // }
                // i.e. it is a "statement block" but it is the last statement of another
                // block therefore it should assign the _0 return. This can possibly occur to
                // any arbitrary depth of nested blocks
                todo!()
            }

            Expr::Call(call) => {
                // FIXME: this should return Option<Rvalue> or needs to be re-thought completely
                // a Call will normally construct a terminator, the Rvalue is a special case for
                // builtin operators
                let rvalue = self.construct_call_rvalue(call, context);

                if let Some(place) = assign_place {
                    let assign = Statement::assign(place, rvalue);

                    self.current_block_mut().statements.push(assign);
                }
            }

            // TODO: a lot of these are going to repeat this same code of
            // "if there's a place to assign to, make the operand/rvalue/assign"
            Expr::VarRef(var_ref) => {
                if let Some(assign_place) = assign_place {
                    let operand = self.construct_var_ref_operand(var_ref);
                    let rvalue = Rvalue::Use(operand);

                    let assign = Statement::assign(assign_place, rvalue);
                    self.current_block_mut().statements.push(assign);
                };
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::If(_) => todo!(),

            Expr::VarDef(var_def) => {
                todo!()
            }
            Expr::Statement(_) => {
                // HIR should have just 1 expression nested inside a statement
                // so the expression should have already been pulled out
                unreachable!("Compiler bug (MIR): Found hir::Expr::Statement directly nested inside hir::Expr::Statement")
            }
            Expr::ReturnStatement(ret) => todo!(),

            _ => {
                eprintln!("Compiler Bug (MIR): Unknown expression {statement_expr:?}");
                unreachable!()
            }
        }
    }

    fn construct_statement_constant(&mut self, assign_place: Option<Place>, constant: Constant) {
        if let Some(assign_place) = assign_place {
            let operand = Operand::Constant(constant);
            let rvalue = Rvalue::Use(operand);
            let assign = Statement::assign(assign_place, rvalue);
            self.current_block_mut().statements.push(assign);
        }
    }

    fn construct_call_rvalue(&mut self, call: &hir::CallExpr, context: &Context) -> Rvalue {
        // FIXME: pull return_place only if is last statement of the *function*
        // otherwise make a new place/local for assigning
        let place = self.current_function().return_place();

        // FIXME: create try_make_binop_rvalue or whatever and use that here
        if let Some(binop) = try_get_binop(call, context) {
            return self.construct_binop(context, call, binop);
        }

        // FIXME: otherwise, create a Call terminator
        // direct Call for non-capturing functions
        // indirect Call for capturing/closures (not implemented for a while)
        todo!()
    }

    fn construct_call_operand(
        &mut self,
        call: &CallExpr,
        context: &Context,
        ty: Idx<Type>,
    ) -> Operand {
        if let Some(binop) = try_get_binop(call, context) {
            // When a BinOp is encountered here, we need to create a new (temp) local hold the Rvalue
            let local = self.make_local(ty, None, Mutability::Not);
            let rvalue = self.construct_binop(context, call, binop);

            let assign = Statement::assign(local.into(), rvalue);
            self.current_block_mut().statements.push(assign);

            return Operand::Copy(local.into());
        };

        // FIXME: otherwise, create a Call terminator
        // direct Call for non-capturing functions
        // indirect Call for capturing/closures (not implemented for a while)
        // possibly construct a temporary since this is in the context of another operand
        todo!()
    }

    fn construct_binop(&mut self, context: &Context, call: &CallExpr, binop: BinOp) -> Rvalue {
        // FIXME: where do we construct the Call terminator for user-defined operations?
        // such as Point + Point
        let operand1 = self.construct_operand(call.args[0], context);
        let operand2 = self.construct_operand(call.args[1], context);

        Rvalue::BinaryOp(binop, Box::new((operand1, operand2)))
    }

    /// Constructs an Assign statement for a *new* Local and adds
    /// it to the current block.
    ///
    /// See also `Statement::assign` for a lower-level function.
    fn construct_assign(
        &mut self,
        rvalue: Rvalue,
        ty: Idx<hir::Type>,
        symbol: Option<ValueSymbol>,
    ) {
        let local = self.make_local(ty, symbol, Mutability::Not);
        let assign = Statement::assign(local.into(), rvalue);
        self.current_block_mut().statements.push(assign);
    }

    fn find_symbol(&self, symbol: ValueSymbol) -> Idx<Local> {
        self.current_function()
            .locals_map
            .iter()
            .find(|(_, s)| **s == Some(symbol))
            .expect("this symbol to have a corresponding Local")
            .0
    }

    fn construct_rvalue(&mut self, expr: &Expr, context: &Context) -> Rvalue {
        match expr {
            Expr::BoolLiteral(_) => todo!(),
            Expr::FloatLiteral(_) => todo!(),
            Expr::IntLiteral(i) => todo!(),
            Expr::StringLiteral(_) => todo!(),
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Call(call) => self.construct_call_rvalue(call, context),
            Expr::VarRef(var_ref) => {
                let operand = self.construct_var_ref_operand(var_ref);
                Rvalue::Use(operand)
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::VarDef(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Statement(_) => todo!(),
            Expr::ReturnStatement(_) => todo!(),
            Expr::Intrinsic(_) => todo!(),

            Expr::Empty | Expr::UnresolvedVarRef { .. } | Expr::Module(_) => unreachable!(),
        }
    }

    fn construct_operand(&mut self, expr: Idx<Expr>, context: &Context) -> Operand {
        if let Some(constant) = self.construct_constant(context.expr(expr)) {
            return Operand::Constant(constant);
        }
        let ty = context.expr_type_idx(expr);

        match context.expr(expr) {
            Expr::VarRef(var_ref) => self.construct_var_ref_operand(var_ref),
            Expr::Call(call) => self.construct_call_operand(call, context, ty),
            _ => panic!(
                "unexpected Expr variant when constructing operand: {:?}",
                expr
            ),
        }
    }

    fn construct_var_ref_operand(&mut self, var_ref: &hir::VarRefExpr) -> Operand {
        let local = self.find_symbol(var_ref.symbol);
        let place = local.into();

        // TODO: resource types need to be moved rather than copied
        // and "share" types, like strings maybe would have a different Operand::Share?
        Operand::Copy(place)
    }

    fn construct_constant(&self, expr: &Expr) -> Option<Constant> {
        match expr {
            Expr::BoolLiteral(b) => Some(Constant::Int(*b as i64)),
            Expr::FloatLiteral(f) => Some(Constant::Float(*f)),
            Expr::IntLiteral(i) => Some(Constant::Int(*i)),

            _ => None,
        }
    }

    fn make_local(
        &mut self,
        ty: Idx<hir::Type>,
        symbol: Option<ValueSymbol>,
        mutability: Mutability,
    ) -> Idx<Local> {
        let local = Local { mutability, ty };
        let func = self.current_function_mut();
        let local = func.locals.alloc(local);
        func.locals_map.insert(local, symbol);

        self.local_count += 1;

        local
    }
}

fn try_get_binop(call: &hir::CallExpr, context: &Context) -> Option<BinOp> {
    if let Some(symbol) = call.symbol {
        if let Some(f) = context.lookup_function_symbol(symbol) {
            if let Expr::Intrinsic(intrinsic) = context.expr(f) {
                return Some(intrinsic.into());
            }
        }
    }
    None
}
