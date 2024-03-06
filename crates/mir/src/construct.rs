//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{CallExpr, Context, Expr, FunctionParam, IfExpr, Type, ValueSymbol, VarDefExpr};
use la_arena::Idx;

use crate::syntax::{
    BasicBlock, BinOp, Constant, Local, Mutability, Operand, Place, Rvalue, Statement,
    SwitchIntTargets, Terminator,
};
use crate::{Builder, Function};
use util_macros::assert_matches;

impl Builder {
    // construct into existing allocated Function
    pub(crate) fn construct_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
        context: &Context,
    ) -> Idx<Function> {
        let hir::FunctionExpr { params, body, name } = function_expr;
        let func = self.current_function_mut();
        func.name = *name;

        let return_ty = context.expr_type_idx(*body);
        self.make_local(return_ty, None, Mutability::Mut);

        for param in params.iter() {
            self.construct_param(param, context);
        }

        // initial block
        let return_place = Some(self.current_function().return_place());
        self.construct_block(*body, None, &return_place, context);

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

    /// Constructs the statements of the provided HIR Block
    /// into the current MIR BasicBlock
    fn construct_block(
        &mut self,
        block_expr: Idx<Expr>,
        jump_to: Option<Idx<BasicBlock>>,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let statements = assert_matches!(context.expr(block_expr), Expr::Block).exprs();
        let start = self.scopes.peek_statement_counter();
        let statements = &statements[start..];

        let statements_iter = statements
            .iter()
            .enumerate()
            .map(|(i, el)| (el, i == statements.len() - 1));

        for (statement, is_last) in statements_iter {
            self.scopes.increment_statement_counter();
            let assign_to = if is_last { assign_to } else { &None };
            self.construct_statement_like(*statement, block_expr, assign_to, context);
        }

        if self.current_block().terminator.is_none() {
            if let Some(target) = jump_to {
                self.current_block_mut().terminator = Some(Terminator::Jump { target });
            } else {
                self.construct_return();
            }
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
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let statement = context.expr(statement);
        if let Expr::VarDef(var_def) = statement {
            self.construct_var_def(var_def, context)
        } else {
            let statement = assert_matches!(statement, Expr::Statement);
            self.construct_statement(*statement, block_expr, assign_to, context)
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
        block_expr: Idx<Expr>, // the hir::Expr::Block that contains this statement
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let statement_expr = context.expr(statement_idx);

        match statement_expr {
            Expr::BoolLiteral(b) => {
                self.construct_statement_constant(Constant::bool(*b), assign_to)
            }
            Expr::FloatLiteral(f) => {
                self.construct_statement_constant(Constant::Float(*f), assign_to)
            }
            Expr::StringLiteral(s) => {
                self.construct_statement_constant(Constant::String(*s), assign_to)
            }
            Expr::IntLiteral(i) => self.construct_statement_constant(Constant::Int(*i), assign_to),

            // can't ignore because elements may cause side-effects
            // ex. [do_thing (), do_thing (), do_thing ()]
            // couldn't be optimized away unless do_thing could be proven to
            // be side-effect free
            // TODO: optimization pass to remove if elements are literals
            Expr::ArrayLiteral(_) => todo!(),

            // TODO: will be removed in favor of Call
            Expr::Unary(_) => todo!(),

            Expr::Block(_) => {
                // when a new block is encountered as a statement, there isn't actually any
                // control flow, the new block is immediately jumped into
                // Therefore here we don't create a new BasicBlock, and instead construct the
                // next statements into the existing Basic Block
                // If these statements end without any explicit terminators, then
                // construction just continues on in the current block.
                self.scopes.push(statement_idx);
                self.construct_block(statement_idx, None, assign_to, context);
                self.scopes.pop();
            }

            Expr::Call(call) => {
                // FIXME: this should return Option<Rvalue> or needs to be re-thought completely
                // a Call will normally construct a terminator, the Rvalue is a special case for
                // builtin operators
                let rvalue = self.construct_call_rvalue(call, context);

                if let Some(place) = assign_to {
                    let assign = Statement::assign(place.clone(), rvalue);

                    self.current_block_mut().statements.push(assign);
                }
                // FIXME: Call may create a Terminator which should return Break
                // for the control flow so that the next block is constructed
            }

            Expr::VarRef(var_ref) => {
                if let Some(assign_place) = assign_to {
                    let operand = self.construct_var_ref_operand(var_ref);
                    let rvalue = Rvalue::Use(operand);

                    let assign = Statement::assign(assign_place.clone(), rvalue);
                    self.current_block_mut().statements.push(assign);
                };
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::If(if_expr) => self.construct_if_expr(if_expr, block_expr, assign_to, context),

            Expr::VarDef(_) => {
                unreachable!("Compiler bug (MIR): Found hir::Expr::VarDefExpr inside of hir::Expr::Statement")
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

    fn construct_statement_constant(&mut self, constant: Constant, assign_to: &Option<Place>) {
        if let Some(assign_place) = assign_to {
            let operand = Operand::Constant(constant);
            let rvalue = Rvalue::Use(operand);
            let assign = Statement::assign(assign_place.clone(), rvalue);
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

    fn construct_return(&mut self) {
        let (return_local, _) = self.current_function().return_local();

        if self.current_block != self.current_function().entry_block() {
            self.current_block_mut().parameters.push(return_local);
        }
        self.current_block_mut().terminator = Some(Terminator::Return);
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

    fn construct_if_expr(
        &mut self,
        if_expr: &IfExpr,
        block_expr: Idx<Expr>,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let IfExpr {
            condition,
            then_branch,
            else_branch,
        } = if_expr;
        // TODO: if the condition expr is a BoolLiteral, mark one of the blocks unreachable?
        let rvalue = self.construct_rvalue(context.expr(*condition), context);
        let local = self.construct_assign(rvalue, context.core_types().bool, None);
        let discriminant = Operand::Copy(Place::from(local));

        let source_block = self.current_block;
        let then_block = self.new_block();
        let else_block = else_branch.map(|_| self.new_block());
        let join_block = self.new_block();

        self.current_block = then_block;
        self.scopes.push(*then_branch);
        self.construct_block(*then_branch, Some(join_block), assign_to, context);
        self.scopes.pop();

        let else_zipped = else_branch.zip(else_block);
        if let Some((else_branch, else_block)) = else_zipped {
            self.current_block = else_block;
            self.scopes.push(else_branch);
            self.construct_block(else_branch, Some(join_block), assign_to, context);
            self.scopes.pop();
        }

        let target_block = if let Some(else_block) = else_block {
            else_block
        } else {
            join_block
        };
        let targets = SwitchIntTargets {
            branches: Box::new([(0, target_block)]),
            otherwise: Some(then_block),
        };

        self.block_mut(source_block).terminator = Some(Terminator::SwitchInt {
            discriminant,
            targets,
        });

        // construct the block that joins the branches back together
        // the ScopesStack should be correct after popping the if/else scopes
        // which means the original hir::block should be able to continue
        // constructing the next BasicBlock(s) by starting at its current statement
        self.current_block = join_block;
        self.construct_block(block_expr, None, assign_to, context);
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
