//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{
    CallExpr, Context, ContextDisplay, Expr, FunctionParam, IfExpr, LoopExpr, MatchExpr,
    Mutability, ReAssignment, Type, ValueSymbol, VarDefExpr,
};
use itertools::Itertools;
use la_arena::Idx;

use crate::syntax::{
    BasicBlock, BinOp, BlockTarget, BranchIntTargets, Callee, Constant, FuncId, Local, Operand,
    OperandOrPlace, Place, Rvalue, Statement, Terminator,
};
use crate::{Builder, Function, Module};
use util_macros::assert_matches;

impl Builder {
    pub(crate) fn construct_module(mut self, module: &hir::Module, context: &Context) -> Module {
        let mut func_count: u32 = 0;
        for expr in module.exprs.iter() {
            let expr = context.expr(*expr);

            if let Expr::TypeStatement(_) = expr {
                continue;
            }

            // match expr {
            //     // type definitions, others?
            //     Expr::Statement(statement) => {
            //         todo!()
            //     }
            //     // should be most things - `let main = () -> { ... }`
            //     // constants too - `let base_url = "..."`
            //     Expr::VarDef(var_def) => {
            //         todo!()
            //     }

            //     Expr::TypeStatement(type_statement) => {
            //         todo!()
            //     }
            // }
            if let Expr::Statement(x) = expr {
                dbg!(x.display(context));
                let x = context.expr(*x);
                dbg!(x);
            }
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
                self.function_names.insert(func_id, name.clone());
                self.construct_function(function_expr, name, context);
                func_count += 1;
            }
        }

        self.build()
    }

    // construct into existing allocated Function
    pub(crate) fn construct_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
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

        self.construct_terminator_arguments();

        self.current_function
    }

    fn construct_param(&mut self, param: &FunctionParam, context: &Context) {
        let ty_idx = context.type_idx_of_value(&param.symbol);
        self.current_function_mut().params.push(ty_idx);
        let ty = context.type_idx_of_value(&param.symbol);
        let local = self.construct_local(ty, Some(param.symbol), Mutability::Not);
        // TODO: depends on the param
    }

    /// Creates a new block in the current function
    ///
    /// Only a pass-through to Function::new_block()
    /// Does not change what the current block is
    fn new_block(&mut self) -> Idx<BasicBlock> {
        self.current_function_mut().new_block()
    }

    // TODO: move to a new module for post-construction passes
    fn args_of(&self, target_block: Idx<BasicBlock>) -> Vec<Idx<Local>> {
        self.block(target_block).parameters.clone().take_inner()
    }

    /// Visits each block and populates arguments for terminators with targets
    // TODO: move to a new module for post-construction passes
    fn construct_terminator_arguments(&mut self) {
        let block_indexes = self.block_indexes();
        for idx in block_indexes {
            let existing_terminator = self
                .block(idx)
                .terminator
                .as_ref()
                .expect(
                    "Compiler Bug (MIR): Missing terminator when constructing terminator arguments",
                )
                .clone();
            let new_terminator = match existing_terminator {
                Terminator::Jump(block_target) => {
                    let args = self.args_of(block_target.target);
                    Terminator::Jump(BlockTarget::with_args(block_target.target, args))
                }
                Terminator::BranchInt {
                    discriminant,
                    targets,
                } => {
                    let branches = targets
                        .branches
                        .iter()
                        .map(|(int, block_target)| {
                            let args = self.args_of(block_target.target);
                            let block_target = BlockTarget::with_args(block_target.target, args);
                            (*int, block_target)
                        })
                        .collect_vec()
                        .into();
                    let otherwise = targets.otherwise.map(|otherwise| {
                        let args = self.args_of(otherwise.target);
                        Box::new(BlockTarget::with_args(otherwise.target, args))
                    });
                    Terminator::branch_int(discriminant, branches, otherwise)
                }
                Terminator::Return => Terminator::Return,

                terminator @ Terminator::Call { .. } => terminator,
                Terminator::Drop { place, target } => todo!(),
                Terminator::Unreachable => Terminator::Unreachable,
            };
            self.block_mut(idx).terminator = Some(new_terminator);
        }
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
            self.construct_statement_like(*statement, assign_to, context);
        }

        if let Some(target) = jump_to {
            self.construct_jump_terminator(target);
        } else if self.current_block().terminator.is_none() {
            self.construct_return();
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
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let statement = context.expr(statement);
        if let Expr::VarDef(var_def) = statement {
            self.construct_var_def(var_def, context)
        } else {
            let statement = assert_matches!(statement, Expr::Statement);
            self.construct_statement(*statement, assign_to, context)
        }
    }

    fn construct_var_def(&mut self, var_def: &VarDefExpr, context: &Context) {
        // TODO: capture mutability in parser, AST, HIR, and through here
        let ty = context.expr_type_idx(var_def.value);
        if context.type_is_function(ty) {
            // self.functions_map.insert(k, v)
            todo!("handle local functions")
        }
        let mutability = context.mutability_of(&var_def.symbol);
        let local = self.construct_local(ty, Some(var_def.symbol), mutability);
        self.scopes.insert_local(local, Some(var_def.symbol));

        let assign_place = Place::from(local);
        // assumption: all branches in here create an Assign because assign_to is Some
        // which is why the return value here can be ignored
        let _ = self.construct_operand(var_def.value, &Some(assign_place), context);
    }

    fn construct_statement(
        &mut self,
        statement_idx: Idx<Expr>,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let statement_expr = context.expr(statement_idx);

        match statement_expr {
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
                // when a new block is encountered as a *statement*, there isn't actually any
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
                    if let Some(assign_place) = assign_to {
                        let assign = Statement::assign(assign_place.clone(), rvalue);
                        self.current_block_mut().statements.push(assign);
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
            Expr::Match(match_expr) => self.construct_match_expr(match_expr, assign_to, context),
            Expr::If(if_expr) => self.construct_if_expr(if_expr, assign_to, context),
            Expr::Loop(loop_expr) => self.construct_loop_expr(loop_expr, assign_to, context),

            Expr::ReAssignment(reassignment) => self.construct_reassign(reassignment, context),

            Expr::VarDef(_) => {
                unreachable!("Compiler bug (MIR): Found hir::Expr::VarDefExpr inside of hir::Expr::Statement")
            }
            Expr::Statement(_) => {
                // HIR should have just 1 expression nested inside a statement
                // so the expression should have already been pulled out
                unreachable!("Compiler bug (MIR): Found hir::Expr::Statement directly nested inside hir::Expr::Statement")
            }
            Expr::ReturnStatement(ret) => todo!(),
            Expr::BreakStatement(break_) => {
                // assumption: HIR checks that break statements are inside a loop
                let break_to = self
                    .breaks_stack
                    .last()
                    .expect("break statement to be inside a loop");
                self.construct_jump_terminator(*break_to);

                // If there are any statements left in the (HIR) block, they are unreachable now.
                // Create a dummy block, so if there are other statements left in the HIR, just let it cook
                // instead of trying to mutate / mess with the remaining statements to skip them.
                // This block has no predecessors and will be optimized out anyways.
                let dummy_block = self.new_block();
                self.current_block = dummy_block;
            }
            Expr::UnionUnitVariant(unit_variant) => self
                .construct_statement_constant(Constant::Int(unit_variant.index as i64), assign_to),

            _ => unreachable!("Compiler Bug (MIR): Unknown expression {statement_expr:?}"),
        }
    }

    fn construct_statement_constant(&mut self, constant: Constant, assign_to: &Option<Place>) {
        if let Some(assign_place) = assign_to {
            let operand = Operand::Constant(constant);
            let assign = Statement::assign(assign_place.clone(), operand);
            self.current_block_mut().statements.push(assign);
            self.def_local(assign_place.local);
        }
    }

    fn construct_jump_terminator(&mut self, target: Idx<BasicBlock>) {
        let predecessor = self.current_block;
        self.current_function_mut()
            .predecessors
            .add(predecessor, target);
        self.current_block_mut().terminator = Some(Terminator::jump(target, Vec::default()));
    }

    fn construct_branch_terminator(
        &mut self,
        predecessor: Idx<BasicBlock>,
        discriminant: Operand,
        targets: BranchIntTargets,
    ) {
        for (_, successor) in targets.branches.iter() {
            self.current_function_mut()
                .predecessors
                .add(predecessor, successor.target);
        }
        if let Some(successor) = &targets.otherwise {
            self.current_function_mut()
                .predecessors
                .add(predecessor, successor.target);
        }
        self.block_mut(predecessor).terminator = Some(Terminator::BranchInt {
            discriminant,
            targets,
        });
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
            .map(|arg| {
                Operand::from_op_or_place(self.construct_operand(*arg, &None, context), context)
            })
            .collect_vec();

        let signature_index = call
            .signature_index
            .expect("signature to have been resolved in HIR");
        let callee = self.construct_callee(call.callee, signature_index, context);
        let func = match callee {
            Callee::Direct(func_id) => Operand::Constant(func_id.into()),
            // TODO: confirm if Copy is fine? It should never be Move probably
            // unless its a closure that captures a Move type?
            Callee::Indirect(local) => Operand::Copy(local.into()),
        };
        let target_block = self.new_block();
        let predecessor = self.current_block;
        self.current_function_mut()
            .predecessors
            .add(predecessor, target_block);

        self.current_block_mut().terminator = Some(Terminator::Call {
            func,
            args: args.into(),
            destination: destination.clone(),
            target: Some(BlockTarget::with_empty_args(target_block)),
        });

        self.current_block = target_block;

        destination
    }

    fn construct_call_operand(
        &mut self,
        call: &CallExpr,
        ty: Idx<Type>,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> OperandOrPlace {
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

        OperandOrPlace::from(place)
    }

    fn construct_callee(
        &mut self,
        callee: Idx<Expr>,
        signature_index: u32,
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
        let lhs = Operand::from_op_or_place(lhs, context);
        let rhs = self.construct_operand(binop.rhs, &None, context);
        let rhs = Operand::from_op_or_place(rhs, context);

        Rvalue::BinaryOp(binop.kind, Box::new((lhs, rhs)))
    }

    fn construct_return(&mut self) {
        let (return_local, _) = self.current_function().return_local();

        // if self.current_block != self.current_function().entry_block() {
        //     self.zzzusezzz_blockzzz_paramzzz(return_local);
        // }
        // for block_idx in self.block_indexes().iter() {
        //     let block = &self.current_function().blocks[*block_idx];
        //     let var_defs = self
        //         .block_var_defs
        //         .iter()
        //         .filter_map(|(local, block)| (block == block_idx).then_some(local))
        //         .collect_vec();
        //     dbg!(block_idx, var_defs, &block.parameters);
        // }
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

    fn construct_reassign(&mut self, reassignment: &ReAssignment, context: &Context) {
        let op_or_place = self.construct_operand(reassignment.value, &None, context);
        let operand = Operand::from_op_or_place(op_or_place, context);
        let rvalue = Rvalue::from(operand);

        let place = self
            .construct_operand(reassignment.place, &None, context)
            .into_place()
            .expect("assign place to be a valid Place");
        let assign = Statement::reassign(place, rvalue);
        self.current_block_mut().statements.push(assign);
    }

    fn construct_operand_assign(
        &mut self,
        assign_to: &Option<Place>,
        operand: Operand,
    ) -> OperandOrPlace {
        if let Some(place) = assign_to {
            let assign = Statement::assign(place.clone(), operand.clone());
            self.current_block_mut().statements.push(assign);
        }

        OperandOrPlace::from(operand)
    }

    fn construct_match_expr(
        &mut self,
        match_expr: &MatchExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        // FIXME: assumes that scrutinee is an enum. eventually will have literal patterns
        // and more complex patterns like records
        // let scrutinee_op = self.construct_operand(match_expr.scrutinee, assign_to, context);
        // let scrutinee_place = scrutinee_op
        //     .as_place()
        //     .expect("FIXME: scrutinee was not a place");

        let scrutinee_ty = context.expr_type(match_expr.scrutinee);
        let scrutinee = self.construct_operand(match_expr.scrutinee, &None, context);
        let scrutinee = Operand::from_op_or_place(scrutinee, context);

        // let discriminant = Rvalue::Discriminant(scrutinee_place);

        // self.construct_assign(discriminant, context.core_types().int, None);

        let source_block = self.current_block;
        let branch_blocks = match_expr
            .arms
            .iter()
            .map(|_| self.new_block())
            .collect_vec();
        let join_block = self.new_block();

        for (i, branch_block) in branch_blocks.iter().enumerate() {
            self.current_block = *branch_block;
            self.scopes.push();
            self.construct_block(
                match_expr.arms[i].expr,
                Some(join_block),
                assign_to,
                context,
            );
            self.scopes.pop();
        }

        let mut branches = vec![];
        let mut otherwise = None;
        for (arm_index, arm) in match_expr.arms.iter().enumerate() {
            match &arm.pattern {
                hir::Pattern::Wild(_) => todo!(),
                hir::Pattern::Binding { meta, binding } => {
                    // need to get union variant index as int
                    let variant_index = match scrutinee_ty {
                        Type::Sum(s) => s.index_of(binding.ident),
                        _ => unreachable!(),
                    };
                    if variant_index.is_none() {
                        let s = context.lookup(binding.ident);
                        let ty_s = scrutinee_ty.display(context);
                        panic!("Found '{s}' binding, expected that to exist on type {ty_s}")
                    }
                    let variant_index = variant_index.unwrap();

                    branches.push((
                        variant_index,
                        BlockTarget::with_empty_args(branch_blocks[arm_index]),
                    ));
                }
                hir::Pattern::Path(_) => todo!(),
                hir::Pattern::Literal(_) => todo!(),
            }
        }

        let targets = BranchIntTargets {
            branches: branches.into(),
            otherwise,
        };

        self.construct_branch_terminator(source_block, scrutinee, targets);

        self.current_block = join_block;
    }

    fn construct_if_expr(
        &mut self,
        if_expr: &IfExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let IfExpr {
            condition,
            then_branch,
            else_branch,
        } = if_expr;
        let discriminant = self.construct_operand(*condition, &None, context);
        let discriminant = Operand::from_op_or_place(discriminant, context);

        let else_is_empty = match else_branch {
            Some(else_branch) => is_empty_block(*else_branch, context),
            None => true,
        };
        if is_empty_block(*then_branch, context) && else_is_empty {
            // No need to construct anything
            // The discriminant has already been constructed in case there were side effects
            // TODO: this needs to be tested for when the discriminant is a Call
            return;
        }
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

        let target = else_block.unwrap_or(join_block);

        let targets = BranchIntTargets {
            branches: Box::new([(0, BlockTarget::with_empty_args(target))]),
            otherwise: Some(Box::new(BlockTarget::with_empty_args(then_block))),
        };

        self.construct_branch_terminator(source_block, discriminant, targets);

        // The ScopesStack should be correct after popping the if/else scopes
        // which means the original hir::block should be able to continue
        // constructing the next BasicBlock(s) by starting at its current statement
        self.current_block = join_block;

        // TODO: is this needed?
        if let Some(assign_place) = assign_to {
            self.use_block_param(assign_place.local);
        }
    }

    fn construct_loop_expr(
        &mut self,
        loop_expr: &LoopExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
        let new_block = self.new_block();
        self.construct_jump_terminator(new_block);
        self.current_block = new_block;

        let break_to = self.new_block();
        self.breaks_stack.push(break_to);

        // FIXME: passing in the jump_to as the current block needs to create the loop
        self.scopes.push();
        // dbg!(self.current_block);
        // dbg!(new_block);
        self.construct_block(loop_expr.body, Some(self.current_block), assign_to, context);
        self.scopes.pop();

        let _ = self.breaks_stack.pop();
        self.current_block = break_to;
    }

    fn try_find_symbol(&self, symbol: ValueSymbol) -> Option<Idx<Local>> {
        self.current_function()
            .locals_map
            .iter()
            .find(|(_, s)| **s == Some(symbol))
            .map(|i| i.0)
    }

    fn find_symbol(&self, symbol: ValueSymbol, context: &Context) -> Idx<Local> {
        self.try_find_symbol(symbol).unwrap_or_else(|| {
            panic!(
                "expected symbol `{}` to have a corresponding Local",
                context.value_symbol_str(symbol)
            )
        })
    }

    #[must_use]
    fn construct_operand(
        &mut self,
        expr: Idx<Expr>,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> OperandOrPlace {
        let ty = context.expr_type_idx(expr);

        match context.expr(expr) {
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
                OperandOrPlace::from(self.construct_var_ref_operand(var_ref, assign_to, context))
            }
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::VarDef(_) => todo!(),
            Expr::Match(match_expr) => {
                self.construct_match_expr(match_expr, assign_to, context);

                let assign_place = assign_to.clone().expect(
                    "construct_operand with Expr::Match should only be called when assign_to is Some",
                );

                OperandOrPlace::from(assign_place)
            }
            Expr::If(if_expr) => {
                self.construct_if_expr(if_expr, assign_to, context);

                let assign_place = assign_to.clone().expect(
                    "construct_operand with Expr::If should only be called when assign_to is Some",
                );

                OperandOrPlace::from(assign_place)
            }
            Expr::ReturnStatement(_) => todo!(),

            _ => panic!(
                "unexpected Expr variant when constructing operand: {:?}",
                expr
            ),
        }
    }

    // fn construct_callee_operand(&mut self, expr: Idx<Expr>, context: &Context) -> Operand {
    //     loop {}
    // }

    // fn construct_callee_operand_inner(&mut self, expr: Idx<Expr>, context: &Context) -> Operand {
    //     let ty = context.expr_type_idx(expr);

    //     match context.expr(expr) {
    //         Expr::Unary(_) => unreachable!("this should be folded into Call"),
    //         Expr::Block(_) => {
    //             // {
    //             //    expr
    //             //    expr
    //             //    f
    //             // } arg
    //             // in this case, find the tail expression of the block
    //             // and recursively find until the Expr::Function is found
    //             todo!()
    //         }
    //         Expr::Call(_) => {
    //             // (g arg1) arg2
    //             // ^^^^^^ this callee, is itself a Call
    //             todo!()
    //         }
    //         Expr::VarRef(_) => {
    //             // find the expression that was bound to this variable
    //             // recursively
    //             // until the Function::Expr is found OR
    //             // a builtin is found
    //             // in either case it should produce a Callee (?)
    //             todo!()
    //         }
    //         Expr::Path(_) => {
    //             // eventually VarRef would be folded into here
    //             todo!()
    //         }
    //         Expr::Function(_) => {
    //             // IIFE
    //             // should be able to return a Constant::Function here
    //             todo!()
    //         }
    //         Expr::If(_) => {
    //             // same as Block, it is possible for an If/Else to resolve to
    //             // a callable function
    //             // but since this is a branch, the function needs to be a runtime value
    //             todo!()
    //         }

    //         _ => unreachable!(),
    //     }
    // }

    fn construct_var_ref_operand(
        &mut self,
        var_ref: &hir::VarRefExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Operand {
        let operand = if let Some(local_idx) = self.try_find_symbol(var_ref.symbol) {
            Operand::from_place(Place::from(local_idx), context)
        } else {
            // check if it's a defined symbol like "false" or "true" from the Bool union
            match context.type_of_value(&var_ref.symbol) {
                Type::Sum(sum_type) => {
                    let key = context.value_symbol_key(var_ref.symbol);
                    let index_of = sum_type.index_of(key);
                    let index_of = index_of.unwrap_or_else(|| {
                        panic!(
                            "expected symbol {} to have been defiend in union {}",
                            var_ref.symbol.display(context),
                            sum_type.display(context)
                        )
                    });
                    Operand::Constant(Constant::Int(index_of))
                }
                _ => todo!("is this possible?"),
            }
        };

        if let Some(assign_place) = assign_to {
            let assign = Statement::assign(assign_place.clone(), operand.clone());
            self.current_block_mut().statements.push(assign);
            self.def_local(assign_place.local);
        };

        operand
    }

    fn def_local(&mut self, local: Idx<Local>) {
        self.block_var_defs.insert(local, self.current_block);
    }

    /// Marks this local as being required to pass into this block
    /// as a block parameter.
    ///
    /// The CLIF representation requires explicit block parameters
    /// when there are multiple sources for a value, such as in branching.
    fn use_block_param(&mut self, local: Idx<Local>) {
        let bb = self
            .block_var_defs
            .get(local)
            .expect("local to have been defined");
        if *bb != self.current_block {
            self.current_block_mut().parameters.push(local);
        }
    }

    pub fn construct_local(
        &mut self,
        ty: Idx<hir::Type>,
        symbol: Option<ValueSymbol>,
        mutability: Mutability,
    ) -> Idx<Local> {
        let local = {
            let func = self.current_function_mut();
            let local = func.locals.alloc(Local { mutability, ty });
            func.locals_map.insert(local, symbol);
            local
        };

        self.def_local(local);

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

// TODO: move to HIR crate - maybe in method for a newtype?
fn is_empty_block(idx: Idx<Expr>, context: &Context) -> bool {
    match context.expr(idx) {
        Expr::Block(block) => block.exprs().is_empty(),
        _ => false,
    }
}
