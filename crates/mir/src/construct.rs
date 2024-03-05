//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{CallExpr, Context, Expr, FunctionParam, IfExpr, Type, ValueSymbol, VarDefExpr};
use la_arena::Idx;

use crate::syntax::{
    BasicBlock, BinOp, Constant, Local, Mutability, Operand, Place, Rvalue, Statement,
    SwitchIntTargets, Terminator,
};
use crate::{BlockQueueItem, Builder, Function};
use util_macros::assert_matches;

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

        // initial block
        let return_place = Some(self.current_function().return_place());
        self.block_queue.push_back(BlockQueueItem {
            to_build: self.current_block,
            block_expr: function_expr.body,
            assign_to: return_place,
            jump_to: None,
        });

        // main loop - pop from the queue and construct
        while let Some(next) = self.block_queue.pop_front() {
            let BlockQueueItem {
                to_build,
                block_expr,
                assign_to,
                jump_to,
            } = next;
            self.current_block = to_build;
            self.jump_to = jump_to;
            self.assign_to = assign_to;

            self.construct_block(block_expr, context);
        }

        self.current_function
    }

    fn construct_param(&mut self, param: &FunctionParam, context: &Context) {
        let ty_idx = context.type_idx_of_value(&param.symbol);
        self.current_function_mut().params.push(ty_idx);
        let ty = context.type_idx_of_value(&param.symbol);
        let local = self.make_local(ty, Some(param.symbol), Mutability::Not); // TODO: depends on the param
        self.block_var_defs.insert(local, self.current_block);
    }

    /// Creates a new block in the current function
    ///
    /// Only a pass-through to Function::new_block()
    /// Does not change what the current block is
    fn new_block(&mut self) -> Idx<BasicBlock> {
        self.current_function_mut().new_block()
    }

    /// Constructs the statements of the provided HIR Block into the
    /// current MIR BasicBlock
    ///
    /// The slice of statements used to construct the BasicBlock are determined
    /// by the current builder state of `statement_counter`.
    ///
    /// This allows for multiple MIR BasicBlock to be constructed from a single
    /// HIR Block. For example, before and after an if-else expression that converges
    /// is the same HIR Block but must be different MIR BasicBlocks.
    fn construct_block(&mut self, block_expr: Idx<Expr>, context: &Context) {
        let statements = assert_matches!(context.expr(block_expr), Expr::Block).exprs();
        let start = self.scopes.peek_statement_counter();
        let statements = &statements[start..];

        let statements_iter = statements
            .iter()
            .enumerate()
            .map(|(i, el)| (el, i == statements.len() - 1));

        for (statement, is_last) in statements_iter {
            self.construct_statement_like(*statement, block_expr, context, is_last);
            self.scopes.increment_statement_counter();
        }

        if let Some(target) = self.jump_to {
            self.current_block_mut().terminator = Terminator::Jump { target };
            self.jump_to = None;
        } else if matches!(self.current_block_mut().terminator, Terminator::Unreachable) {
            self.current_block_mut().terminator = Terminator::Return;
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
        block_expr: Idx<Expr>,
        context: &Context,
        is_last: bool,
    ) {
        let statement = context.expr(statement);
        if let Expr::VarDef(var_def) = statement {
            self.construct_var_def(var_def, context)
        } else {
            let statement = assert_matches!(statement, Expr::Statement);
            self.construct_statement(*statement, block_expr, context, is_last)
        }
    }

    fn construct_var_def(&mut self, var_def: &VarDefExpr, context: &Context) {
        // TODO: capture mutability in parser, AST, HIR, and through here
        let VarDefExpr { symbol, value, .. } = var_def;
        let symbol = Some(*symbol);
        let ty = context.expr_type_idx(*value);

        // FIXME: var_def.value could be a call
        // which would be constructed as a Terminator, not Assign
        let rvalue = self.construct_rvalue(context.expr(var_def.value), context);
        let local = self.construct_assign(rvalue, ty, symbol);
        self.scopes.insert_local(local, symbol);
        self.block_var_defs.insert(local, self.current_block);
    }

    fn construct_statement(
        &mut self,
        statement_idx: Idx<Expr>,
        // the hir::Expr::Block that contains this statement
        block_expr: Idx<Expr>,
        context: &Context,
        is_last: bool,
    ) {
        let statement_expr = context.expr(statement_idx);

        match statement_expr {
            Expr::BoolLiteral(b) => self.construct_statement_constant(Constant::bool(*b), is_last),
            Expr::FloatLiteral(f) => {
                self.construct_statement_constant(Constant::Float(*f), is_last)
            }
            Expr::StringLiteral(s) => {
                self.construct_statement_constant(Constant::String(*s), is_last)
            }
            Expr::IntLiteral(i) => self.construct_statement_constant(Constant::Int(*i), is_last),

            // can't ignore because elements may cause side-effects
            // ex. [do_thing (), do_thing (), do_thing ()]
            // couldn't be optimized away unless do_thing could be proven to
            // be side-effect free
            // TODO: optimization pass to remove if elements are literals
            Expr::ArrayLiteral(_) => todo!(),

            // TODO: will be removed in favor of Call
            Expr::Unary(_) => todo!(),

            Expr::Block(..) => {
                // when a new block is encountered as a statement, there isn't actually any
                // control flow, the new block is immediately jumped into
                // Therefore here we don't create a new BasicBlock, and instead construct the
                // next statements into the existing Basic Block
                // If these statements end without any explicit terminators, then
                // construction just continues on in the current block.
                self.scopes.push(statement_idx);
                self.construct_block(statement_idx, context);
                self.scopes.pop();
            }

            Expr::Call(call) => {
                // FIXME: this should return Option<Rvalue> or needs to be re-thought completely
                // a Call will normally construct a terminator, the Rvalue is a special case for
                // builtin operators
                let rvalue = self.construct_call_rvalue(call, context);

                if is_last {
                    if let Some(place) = self.assign_to.take() {
                        let assign = Statement::assign(place, rvalue);

                        self.current_block_mut().statements.push(assign);
                    }
                }
                // FIXME: Call may create a Terminator which should return Break
                // for the control flow so that the next block is constructed
            }

            // TODO: a lot of these are going to repeat this same code of
            // "if there's a place to assign to, make the operand/rvalue/assign"
            Expr::VarRef(var_ref) => {
                if is_last {
                    if let Some(assign_place) = self.assign_to.take() {
                        let operand = self.construct_var_ref_operand(var_ref);
                        let rvalue = Rvalue::Use(operand);

                        let assign = Statement::assign(assign_place, rvalue);
                        self.current_block_mut().statements.push(assign);
                    };
                }
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::If(if_expr) => self.construct_if_else(if_expr, block_expr, context),

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

    fn construct_statement_constant(&mut self, constant: Constant, is_last: bool) {
        if is_last {
            if let Some(assign_place) = self.assign_to.take() {
                let operand = Operand::Constant(constant);
                let rvalue = Rvalue::Use(operand);
                let assign = Statement::assign(assign_place, rvalue);
                self.current_block_mut().statements.push(assign);
            }
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

    /// Constructs an Assign statement for a *new* `Local`` and adds
    /// it to the current block.
    ///
    /// If this assignment corresponds to a variable that appears in the
    /// source code, then that symbol must be passed in. Otherwise, for a
    /// temporary/synthetic variable this would be `None`.
    ///
    /// See also `Statement::assign` for a lower-level function.
    fn construct_assign(
        &mut self,
        rvalue: Rvalue,
        ty: Idx<hir::Type>,
        symbol: Option<ValueSymbol>,
    ) -> Idx<Local> {
        let local = self.make_local(ty, symbol, Mutability::Not);
        let assign = Statement::assign(local.into(), rvalue);
        self.current_block_mut().statements.push(assign);

        local
    }

    fn construct_if_else(&mut self, if_expr: &IfExpr, block_expr: Idx<Expr>, context: &Context) {
        todo!();
        // let IfExpr {
        //     condition,
        //     then_branch,
        //     else_branch,
        // } = if_expr;
        // // TODO: if the condition expr is a BoolLiteral, mark one of the blocks unreachable?
        // let rvalue = self.construct_rvalue(context.expr(*condition), context);
        // let local = self.construct_assign(rvalue, context.core_types().bool, None);
        // let discriminant = Operand::Copy(Place::from(local));

        // let then_block = self.new_block();
        // let mut then_queue_item = BlockQueueItem {
        //     to_build: then_block,
        //     block_expr: *then_branch,
        //     assign_to: self.assign_to.clone(),
        //     jump_to: None,
        // };

        // let else_stuff = else_branch.map(|branch| {
        //     let else_block = self.new_block();

        //     let else_queue_item = BlockQueueItem {
        //         to_build: else_block,
        //         block_expr: branch,
        //         assign_to: self.assign_to.clone(),
        //         jump_to: None,
        //     };
        //     (else_block, else_queue_item)
        // });

        // let join_block = self.new_block();

        // then_queue_item.jump_to = Some(join_block);
        // self.block_queue.push_back(then_queue_item);

        // let targets = if let Some((else_block, mut else_queue_item)) = else_stuff {
        //     else_queue_item.jump_to = Some(join_block);
        //     self.block_queue.push_back(else_queue_item);
        //     SwitchIntTargets {
        //         branches: Box::new([(0, else_block)]),
        //         otherwise: Some(then_block),
        //     }
        // } else {
        //     SwitchIntTargets {
        //         branches: Box::new([(0, join_block)]),
        //         otherwise: Some(then_block),
        //     }
        // };

        // let join_queue_item = BlockQueueItem {
        //     to_build: join_block,
        //     block_expr,
        //     assign_to: None,
        //     jump_to: None,
        // };
        // self.block_queue.push_back(join_queue_item);

        // self.current_block_mut().terminator = Terminator::SwitchInt {
        //     discriminant,
        //     targets,
        // };
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
            Expr::BoolLiteral(b) => Rvalue::Use(Operand::constant_bool(*b)),
            Expr::FloatLiteral(f) => Rvalue::Use(Operand::constant_float(*f)),
            Expr::IntLiteral(i) => Rvalue::Use(Operand::constant_int(*i)),
            Expr::StringLiteral(_) => todo!(),
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Call(call) => self.construct_call_rvalue(call, context),
            Expr::VarRef(var_ref) => Rvalue::Use(self.construct_var_ref_operand(var_ref)),
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

        match self.block_var_defs.get(local) {
            Some(bb) if *bb != self.current_block => {
                self.current_block_mut().parameters.push(local);
            }
            _ => {}
        }

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
