//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{CallExpr, Context, Expr, FunctionParam, IfExpr, Type, ValueSymbol, VarDefExpr};
use itertools::Itertools;
use la_arena::Idx;

use crate::syntax::{
    BasicBlock, BinOp, Callee, Constant, FuncId, Local, Mutability, Operand, Place, Rvalue,
    Statement, SwitchIntTargets, Terminator,
};
use crate::{Builder, Function, Module};
use util_macros::assert_matches;

impl Builder {
    pub(crate) fn construct_module(mut self, module: &hir::Module, context: &Context) -> Module {
        let mut func_count: u32 = 0;
        for expr in module.exprs.iter() {
            let expr = context.expr(*expr);
            let var_def = assert_matches!(expr, hir::Expr::VarDef);

            let value = context.expr(var_def.value);
            let func_group = assert_matches!(value, hir::Expr::Function);

            let name = func_group.name.map(|(key, ..)| context.lookup(key));
            for (i, function_expr) in func_group.overloads.iter().enumerate() {
                let func_id: FuncId = (self.module_id, func_count).into();
                if let Some(entry_point) = func_group.entry_point {
                    // assumption: entry point functions cannot be overloaded
                    // enforced by HIR / type checking
                    self.entry_points.insert(entry_point, func_id);
                }

                self.functions_map
                    .entry(var_def.symbol)
                    .or_default()
                    .push(func_id);
                let func = Function::new(func_id);
                self.current_block = func.entry_block();
                self.current_function = self.functions.alloc(func);
                let name = name.map(|n| {
                    if i == 0 {
                        n.to_string()
                    } else {
                        format!("{n}${i}")
                    }
                });
                self.construct_function(function_expr, Some(var_def.symbol), name, context);
                func_count += 1;
            }
        }

        self.build()
    }

    // construct into existing allocated Function
    pub(crate) fn construct_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
        symbol: Option<ValueSymbol>,
        name: Option<String>,
        context: &Context,
    ) -> Idx<Function> {
        let hir::FunctionExpr { params, body, .. } = function_expr;
        self.scopes = Default::default();
        let func: &mut Function = self.current_function_mut();
        func.name = name;

        let return_ty = context.expr_type_idx(*body);
        self.construct_local(return_ty, None, Mutability::Mut);

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
        let local = self.construct_local(ty, Some(param.symbol), Mutability::Not); // TODO: depends on the param
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
        let ty = context.expr_type_idx(var_def.value);
        if context.type_is_function(ty) {
            // self.functions_map.insert(k, v)
            todo!("handle local functions")
        }
        let local = self.construct_local(ty, Some(var_def.symbol), Mutability::Not);
        self.scopes.insert_local(local, Some(var_def.symbol));
        self.block_var_defs.insert(local, self.current_block);

        let assign_place = Place::from(local);
        // assumption: all branches in here create an Assign when assign_to is Some
        self.construct_operand(var_def.value, &Some(assign_place), context);
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
            Expr::BoolLiteral(b) => self.construct_statement_constant((*b).into(), assign_to),
            Expr::FloatLiteral(f) => self.construct_statement_constant((*f).into(), assign_to),
            Expr::StringLiteral(s) => self.construct_statement_constant((*s).into(), assign_to),
            Expr::IntLiteral(i) => self.construct_statement_constant((*i).into(), assign_to),

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
                self.scopes.push();
                self.construct_block(statement_idx, None, assign_to, context);
                self.scopes.pop();
            }

            Expr::Call(call) => {
                if let Some(binop) = try_get_binop(call, context) {
                    // TODO: handle side-effects in non-assigned statement binops
                    let rvalue = self.construct_binop(&binop, context);
                    match assign_to {
                        Some(assign_place) => {
                            let assign = Statement::assign(assign_place.clone(), rvalue);
                            self.current_block_mut().statements.push(assign);
                        }
                        // In a statement context, binop with nothing to assign is ignored
                        None => {}
                    };
                } else {
                    self.construct_call_terminator(call, assign_to, context);
                };
            }

            Expr::VarRef(var_ref) => {
                self.construct_var_ref_operand(var_ref, assign_to, context);
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
            let assign = Statement::assign(assign_place.clone(), operand);
            self.current_block_mut().statements.push(assign);
        }
    }

    /*
    // from construct_statement
    {
        func(_3)
    }

    should make new block then

        _N = call func (copy _3) -> [return to BBN]

    // from construct_rvalue (from var_def or if_expr or eventually match)
    {
        ...
        let a = func(_3)
        // or
        if func(_3) { ... }
        ...
        // or
        let a = func1(func2(_3))
    }

    in the first case would be:
    (where a <--> _7)
        _7 = call func (copy _3) -> [return to BB12]

    second case is:

        _8 = call func (copy _3) -> [return to BB12]
    BB12:
        SwitchInt (_8): [0 -> BB13, else -> BB14]

    third case:
        _8 = call func2 (copy _3) -> [return to BB12]
    BB12:
        _9 = call func1 (copy _8) -> [return to BB13]
     */

    fn construct_call(
        &mut self,
        call: &hir::CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Option<Place> {
        match try_get_binop(call, context) {
            Some(binop) => {
                self.construct_binop(&binop, context);
                None
            }
            None => Some(self.construct_call_terminator(call, assign_to, context)),
        }
    }

    fn construct_call_terminator(
        &mut self,
        call: &hir::CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Place {
        let ty = call.return_ty_idx(context);
        let destination = match assign_to {
            Some(assign_place) => assign_place.clone(),
            None => self
                .construct_local(ty, call.symbol, Mutability::Not)
                .into(),
        };

        let args = call
            .args
            .iter()
            .map(|arg| Operand::from_result(self.construct_operand(*arg, &None, context), context))
            .collect_vec();

        let signature_index = call
            .signature_index
            .expect("signature to have been resolved in HIR");
        let callee = self.construct_callee(call.callee, signature_index, assign_to, context);
        let func = match callee {
            Callee::Direct(func_id) => Operand::Constant(func_id.into()),
            // TODO: confirm if Copy is fine? It should never be Move probably
            Callee::Indirect(local) => Operand::Copy(local.into()),
        };
        let next_block = self.new_block();

        let terminator = Terminator::Call {
            func,
            args: args.into(),
            destination: destination.clone(),
            target: Some(next_block),
        };
        self.current_block_mut().terminator = Some(terminator);

        self.current_block = next_block;

        destination
    }

    fn construct_call_operand(
        &mut self,
        call: &CallExpr,
        ty: Idx<Type>,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> OperandResult {
        let place = if let Some(binop) = try_get_binop(call, context) {
            let assign_place = match assign_to {
                Some(assign_place) => assign_place.clone(),
                // New / temp local to hold the Rvalue
                None => self.construct_local(ty, None, Mutability::Not).into(),
            };
            let rvalue = self.construct_binop(&binop, context);

            let assign = Statement::assign(assign_place.clone(), rvalue);
            self.current_block_mut().statements.push(assign);

            assign_place
        } else {
            self.construct_call_terminator(call, assign_to, context)
        };

        OperandResult::Place(place)
    }

    fn construct_callee(
        &mut self,
        callee: Idx<Expr>,
        signature_index: u32,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Callee {
        let callee_expr = context.expr(callee);
        match callee_expr {
            Expr::VarRef(var_ref) => self
                .functions_map
                .get(&var_ref.symbol)
                .and_then(|func_ids| func_ids.get(signature_index as usize))
                .map(Callee::from)
                .unwrap_or_else(|| todo!("grab a local (indirect call)")),

            _ => todo!("come up with a better abstraction to not duplicate all this code"),
        }
    }

    fn construct_binop(&mut self, binop: &BinOp, context: &Context) -> Rvalue {
        let lhs = self.construct_operand(binop.lhs, &None, context);
        let lhs = Operand::from_result(lhs, context);
        let rhs = self.construct_operand(binop.rhs, &None, context);
        let rhs = Operand::from_result(rhs, context);

        Rvalue::BinaryOp(binop.kind, Box::new((lhs, rhs)))
    }

    fn construct_return(&mut self) {
        let (return_local, _) = self.current_function().return_local();

        if self.current_block != self.current_function().entry_block() {
            self.current_block_mut().parameters.push(return_local);
        }
        self.current_block_mut().terminator = Some(Terminator::Return);
    }

    /// Constructs an Assign statement for a *new* `Local` and adds
    /// it to the current block.
    ///
    /// If this assignment corresponds to a variable that appears in the
    /// source code, then that symbol must be passed in. Otherwise, for a
    /// temporary/synthetic variable this would be `None`.
    ///
    /// See also `Statement::assign` for a lower-level function.
    // TODO: review all current uses of Statement::assign and replace with this, or delete this
    fn construct_assign(
        &mut self,
        rvalue: Rvalue,
        ty: Idx<hir::Type>,
        symbol: Option<ValueSymbol>,
    ) -> Idx<Local> {
        let local = self.construct_local(ty, symbol, Mutability::Not);
        let assign = Statement::assign(local, rvalue);
        self.current_block_mut().statements.push(assign);

        local
    }

    fn construct_operand_assign(
        &mut self,
        assign_to: &Option<Place>,
        operand: Operand,
    ) -> OperandResult {
        if let Some(place) = assign_to {
            let assign = Statement::assign(place.clone(), operand.clone());
            self.current_block_mut().statements.push(assign);
        }

        OperandResult::Operand(operand)
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
        let discriminant = self.construct_operand(*condition, &None, context);
        let discriminant = Operand::from_result(discriminant, context);
        // TODO: if the discriminant is a BoolLiteral, mark one of the blocks unreachable?

        let source_block = self.current_block;
        let then_block = self.new_block();
        let else_block = else_branch.map(|_| self.new_block());
        let join_block = self.new_block();

        self.current_block = then_block;
        self.scopes.push();
        self.construct_block(*then_branch, Some(join_block), assign_to, context);
        self.scopes.pop();

        let else_zipped = else_branch.zip(else_block);
        if let Some((else_branch, else_block)) = else_zipped {
            self.current_block = else_block;
            self.scopes.push();
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

    fn find_callee(&self, symbol: ValueSymbol) -> Callee {
        todo!()
    }

    fn find_symbol(&self, symbol: ValueSymbol) -> Idx<Local> {
        self.current_function()
            .locals_map
            .iter()
            .find(|(_, s)| **s == Some(symbol))
            .expect("this symbol to have a corresponding Local")
            .0
    }

    fn construct_operand(
        &mut self,
        expr: Idx<Expr>,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> OperandResult {
        let ty = context.expr_type_idx(expr);

        match context.expr(expr) {
            Expr::BoolLiteral(b) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*b).into()))
            }

            Expr::FloatLiteral(f) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*f).into()))
            }
            Expr::IntLiteral(i) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*i).into()))
            }
            Expr::StringLiteral(_) => todo!(),
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Call(call) => self.construct_call_operand(call, ty, assign_to, context),
            Expr::VarRef(var_ref) => {
                OperandResult::Operand(self.construct_var_ref_operand(var_ref, assign_to, context))
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::VarDef(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::ReturnStatement(_) => todo!(),

            _ => panic!(
                "unexpected Expr variant when constructing operand: {:?}",
                expr
            ),
        }
    }

    fn construct_callee_operand(&mut self, expr: Idx<Expr>, context: &Context) -> Operand {
        loop {}
    }

    fn construct_callee_operand_inner(&mut self, expr: Idx<Expr>, context: &Context) -> Operand {
        let ty = context.expr_type_idx(expr);

        match context.expr(expr) {
            Expr::Unary(_) => unreachable!("this should be folded into Call"),
            Expr::Block(_) => {
                // {
                //    expr
                //    expr
                //    f
                // } arg
                // in this case, find the tail expression of the block
                // and recursively find until the Expr::Function is found
                todo!()
            }
            Expr::Call(_) => {
                // (g arg1) arg2
                // ^^^^^^ this callee, is itself a Call
                todo!()
            }
            Expr::VarRef(_) => {
                // find the expression that was bound to this variable
                // recursively
                // until the Function::Expr is found OR
                // a builtin is found
                // in either case it should produce a Callee (?)
                todo!()
            }
            Expr::Path(_) => {
                // eventually VarRef would be folded into here
                todo!()
            }
            Expr::Function(_) => {
                // IIFE
                // should be able to return a Constant::Function here
                todo!()
            }
            Expr::If(_) => {
                // same as Block, it is possible for an If/Else to resolve to
                // a callable function
                // but since this is a branch, the function needs to be a runtime value
                todo!()
            }

            _ => unreachable!(),
        }
    }

    fn construct_var_ref_operand(
        &mut self,
        var_ref: &hir::VarRefExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Operand {
        let local = self.find_symbol(var_ref.symbol);
        let place = Place::from(local);

        match self.block_var_defs.get(local) {
            Some(bb) if *bb != self.current_block => {
                self.current_block_mut().parameters.push(local);
            }
            _ => {}
        }

        if let Some(assign_place) = assign_to {
            let assign_from = Operand::from_place(place.clone(), context);
            let assign = Statement::assign(assign_place.clone(), assign_from);
            self.current_block_mut().statements.push(assign);
        };

        Operand::from_place(place, context)
    }

    fn construct_local(
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
    call.symbol
        .and_then(|symbol| context.lookup_operator(symbol))
        .map(|intrinsic| BinOp {
            lhs: call.args[0],
            rhs: call.args[1],
            kind: intrinsic.into(),
        })
}

pub enum OperandResult {
    Operand(Operand),

    Place(Place),
}
