//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::{
    BuiltinId, CallExpr, Context, ContextDisplay, Expr, FuncSignature, FunctionParam, IfExpr, Key,
    LoopExpr, MatchExpr, Mutability, Pattern, ReAssignment, SourceKind, Type, ValueSymbol,
    VarDefExpr,
};
use itertools::Itertools;
use la_arena::Idx;

use crate::builder::{
    ParameterizedFunctionTemplate, ParameterizedInstanceKey, PendingGenericInstance,
};
use crate::syntax::{
    BasicBlock, BinOp, BlockTarget, BranchIntTargets, BranchTarget, Callee, Constant, FieldIdx,
    FuncId, Local, Operand, OperandOrPlace, Place, ProjectionElem, Rvalue, RvalueUnionVariant,
    Statement, Terminator,
};
use crate::{Builder, Function, Module};
use util_macros::assert_matches;

impl Builder {
    fn expr_type_idx(&self, expr: Idx<Expr>, context: &Context) -> Idx<Type> {
        self.expr_type_overrides
            .get(&expr)
            .copied()
            .unwrap_or_else(|| context.expr_type_idx(expr))
    }

    fn expr_type<'a>(&self, expr: Idx<Expr>, context: &'a Context) -> &'a Type {
        context.type_(self.expr_type_idx(expr, context))
    }

    fn value_type_idx(&self, symbol: ValueSymbol, context: &Context) -> Idx<Type> {
        self.value_type_overrides
            .get(&symbol)
            .copied()
            .unwrap_or_else(|| context.type_idx_of_value(&symbol))
    }

    fn call_type_args(&self, expr: Idx<Expr>, call: &CallExpr) -> Box<[Idx<Type>]> {
        self.call_type_arg_overrides
            .get(&expr)
            .cloned()
            .unwrap_or_else(|| call.type_args.clone())
    }

    fn call_union_variant(
        &self,
        call: &CallExpr,
        context: &Context,
    ) -> Option<(Idx<Type>, hir::VariantIdx, Key)> {
        match self.expr_type(call.callee, context) {
            Type::Function(function) => function.variant,
            _ => None,
        }
    }

    pub(crate) fn construct_module(self, module: &hir::Module, context: &mut Context) -> Module {
        self.construct_modules(std::slice::from_ref(module), module, context)
    }

    pub(crate) fn construct_modules(
        mut self,
        modules: &[hir::Module],
        primary_module: &hir::Module,
        context: &mut Context,
    ) -> Module {
        let mut function_groups = Vec::new();
        for module in modules {
            for expr_idx in module.exprs.iter() {
                match context.expr(*expr_idx) {
                    Expr::VarDef(var_def) => {
                        if let Expr::Function(function) = context.expr(var_def.value) {
                            if module.source_kind == SourceKind::StandardLibrary
                                && function
                                    .overloads
                                    .iter()
                                    .all(|overload| overload.type_params.is_empty())
                            {
                                let is_exported =
                                    context.module(module.id).is_some_and(|module_node| {
                                        module_node
                                            .exports
                                            .values
                                            .values()
                                            .any(|exported| *exported == var_def.symbol)
                                    });
                                let is_intrinsic_operator =
                                    context.lookup_operator(var_def.symbol).is_some();
                                if !is_exported || is_intrinsic_operator {
                                    continue;
                                }
                            }
                            function_groups.push((module.id, var_def.symbol, function.clone()));
                        }
                    }
                    Expr::TypeStatement(_) => {}
                    Expr::Empty | Expr::Statement(_)
                        if module.source_kind == SourceKind::StandardLibrary => {}
                    Expr::Empty | Expr::Statement(_) => {
                        unreachable!(
                            "MIR module items must be function definitions or type statements"
                        )
                    }
                    _ if module.source_kind == SourceKind::StandardLibrary => {}
                    _ => {
                        unreachable!(
                            "MIR module items must be function definitions or type statements"
                        )
                    }
                }
            }
        }

        for (module_id, symbol, function_group) in function_groups {
            let base_name = function_group
                .name
                .map(|(key, _)| context.lookup(key).to_owned())
                .map(|name| {
                    if module_id == primary_module.id {
                        name
                    } else {
                        format!("stdlib_{}${name}", module_id.as_u32())
                    }
                });

            for (overload_index, function_expr) in function_group.overloads.iter().enumerate() {
                let overload_index = overload_index as u32;
                let name = base_name.as_ref().map(|name| {
                    if overload_index == 0 {
                        name.clone()
                    } else {
                        format!("{name}${overload_index}")
                    }
                });

                if !function_expr.type_params.is_empty() {
                    self.parameterized_functions.insert(
                        (symbol, overload_index),
                        ParameterizedFunctionTemplate {
                            function: function_expr.clone(),
                            name,
                        },
                    );
                    continue;
                }

                let func_id = self.allocate_func_id();
                if let Some(entry_point) = function_group.entry_point {
                    self.entry_points.insert(entry_point, func_id);
                }
                self.functions_map.insert((symbol, overload_index), func_id);
                self.construct_function_instance(func_id, function_expr, name, context);
            }
        }

        while let Some(instance) = self.pending_parameterized_instances.pop_front() {
            let expr_type_overrides = context.instantiate_expr_types(&instance.substitutions);
            let call_type_arg_overrides =
                context.instantiate_call_type_args(&instance.substitutions);
            let value_type_overrides = context.instantiate_value_types(&instance.substitutions);
            self.expr_type_overrides = expr_type_overrides;
            self.call_type_arg_overrides = call_type_arg_overrides;
            self.value_type_overrides = value_type_overrides;
            self.construct_function_instance(
                instance.func_id,
                &instance.function,
                instance.name,
                context,
            );
            self.expr_type_overrides.clear();
            self.call_type_arg_overrides.clear();
            self.value_type_overrides.clear();
        }

        self.build()
    }

    fn allocate_func_id(&mut self) -> FuncId {
        let func_id = (self.module_id, self.next_function_index).into();
        self.next_function_index += 1;
        func_id
    }

    fn construct_function_instance(
        &mut self,
        func_id: FuncId,
        function_expr: &hir::FunctionExpr,
        name: Option<String>,
        context: &Context,
    ) {
        let func = Function::new(func_id);
        self.current_block = func.entry_block();
        self.current_function = self.functions.alloc(func);
        self.function_names.insert(func_id, name.clone());
        self.construct_function(function_expr, name, context);
    }

    fn ensure_generic_instance(
        &mut self,
        symbol: ValueSymbol,
        signature_index: u32,
        type_args: &[Idx<Type>],
        context: &Context,
    ) -> FuncId {
        let type_args = type_args.to_vec().into_boxed_slice();
        let key = ParameterizedInstanceKey {
            symbol,
            signature_index,
            type_args: type_args.clone(),
        };
        if let Some(func_id) = self.parameterized_instances.get(&key) {
            return *func_id;
        }

        let template = self
            .parameterized_functions
            .get(&(symbol, signature_index))
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "MIR monomorphization could not find generic function template for {symbol:?} signature {signature_index}"
                )
            });
        assert_eq!(
            template.function.type_params.len(),
            type_args.len(),
            "HIR generic call arguments must match the function's type parameter count"
        );

        let substitutions = template
            .function
            .type_params
            .iter()
            .copied()
            .zip(type_args.iter().copied())
            .collect_vec()
            .into_boxed_slice();
        let func_id = self.allocate_func_id();
        self.parameterized_instances.insert(key.clone(), func_id);

        let instance_name = template.name.map(|name| {
            let args = type_args
                .iter()
                .map(|ty| context.type_(*ty).display(context).replace(' ', "_"))
                .join("$");
            let candidate = format!("{name}${args}");
            let name_is_reserved = self
                .function_names
                .values()
                .any(|existing| existing.as_deref() == Some(candidate.as_str()))
                || self
                    .pending_parameterized_instances
                    .iter()
                    .any(|instance| instance.name.as_deref() == Some(candidate.as_str()));
            if name_is_reserved {
                let (_, func_index): (u32, u32) = func_id.into();
                format!("{candidate}${func_index}")
            } else {
                candidate
            }
        });
        self.pending_parameterized_instances
            .push_back(PendingGenericInstance {
                func_id,
                function: template.function,
                name: instance_name,
                substitutions,
            });
        func_id
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

        let return_ty = self.expr_type_idx(*body, context);
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
        let ty_idx = self.value_type_idx(param.symbol, context);
        self.current_function_mut().params.push(ty_idx);
        let ty = self.value_type_idx(param.symbol, context);
        // TODO: mutability
        self.construct_local(ty, Some(param.symbol), Mutability::Not);
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
                        BlockTarget::with_args(otherwise.target, args)
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
        let ty = self.expr_type_idx(var_def.value, context);
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
                if array_builtin_id(call, context).is_some() {
                    self.construct_array_call(statement_idx, call, assign_to, context);
                } else if string_builtin_id(call, context).is_some() {
                    self.construct_string_builtin_call(statement_idx, call, assign_to, context);
                } else if let Some(binop) = try_get_binop(call, context) {
                    let rvalue = self.construct_binop(&binop, context);
                    if let Some(assign_place) = assign_to {
                        let assign = Statement::assign(assign_place.clone(), rvalue);
                        self.current_block_mut().statements.push(assign);
                    };
                } else if self.call_union_variant(call, context).is_some() {
                    self.construct_union_variant_call(statement_idx, call, assign_to, context);
                } else {
                    self.construct_call_terminator(statement_idx, call, assign_to, context);
                };
            }

            Expr::VarRef(var_ref) => {
                self.construct_var_ref_operand(var_ref, assign_to, context);
            }
            Expr::Path(path_expr) => {
                self.construct_path_expr(path_expr, assign_to, context);
            }
            Expr::RecordLiteral(record) => {
                // Each field can be arbitrarily complex expressions itself,
                // so recursively construct those operands first
                let mut field_operands = Vec::new();
                for (field_key, field_expr) in &record.fields {
                    let field_op_or_place = self.construct_operand(*field_expr, &None, context);
                    let field_operand = Operand::from_op_or_place(field_op_or_place, context);
                    field_operands.push((*field_key, field_operand));
                }

                let rvalue = Rvalue::Aggregate(field_operands);

                if let Some(assign_place) = assign_to {
                    let assign = Statement::assign(assign_place.clone(), rvalue);
                    self.current_block_mut().statements.push(assign);
                }
            }
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

    fn make_jump_terminator_with_source(
        &mut self,
        target: Idx<BasicBlock>,
        predecessor: Idx<BasicBlock>,
    ) -> Terminator {
        self.current_function_mut()
            .predecessors
            .add(predecessor, target);
        Terminator::jump(target, Vec::default())
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
        call_expr_idx: Idx<Expr>,
        call: &hir::CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Place {
        let ty = self.expr_type_idx(call_expr_idx, context);
        let destination = match assign_to {
            Some(assign_place) => assign_place.clone(),
            None => self.construct_local(ty, None, Mutability::Not).into(),
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
        let type_args = self.call_type_args(call_expr_idx, call);
        let callee = self.construct_callee(call.callee, signature_index, &type_args, context);
        let func = match callee {
            Callee::Direct(func_id) => Operand::Constant(Constant::Func {
                func_id,
                type_idx: self.expr_type_idx(call.callee, context),
            }),
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
        call_expr_idx: Idx<Expr>,
        call: &CallExpr,
        ty: Idx<Type>,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> OperandOrPlace {
        let place = if array_builtin_id(call, context).is_some() {
            return OperandOrPlace::from(self.construct_array_call(
                call_expr_idx,
                call,
                assign_to,
                context,
            ));
        } else if string_builtin_id(call, context).is_some() {
            return OperandOrPlace::from(self.construct_string_builtin_call(
                call_expr_idx,
                call,
                assign_to,
                context,
            ));
        } else if let Some(binop) = try_get_binop(call, context) {
            let assign_place = match assign_to {
                Some(assign_place) => assign_place.clone(),
                // New / temp local to hold the Rvalue
                None => self.construct_local(ty, None, Mutability::Not).into(),
            };
            let rvalue = self.construct_binop(&binop, context);

            let assign = Statement::assign(assign_place.clone(), rvalue);
            self.current_block_mut().statements.push(assign);

            assign_place
        } else if self.call_union_variant(call, context).is_some() {
            return OperandOrPlace::from(self.construct_union_variant_call(
                call_expr_idx,
                call,
                assign_to,
                context,
            ));
        } else {
            self.construct_call_terminator(call_expr_idx, call, assign_to, context)
        };

        OperandOrPlace::from(place)
    }

    fn construct_union_variant_call(
        &mut self,
        call_expr_idx: Idx<Expr>,
        call: &CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Place {
        let destination = assign_to.clone().unwrap_or_else(|| {
            self.construct_local(
                self.expr_type_idx(call_expr_idx, context),
                None,
                Mutability::Not,
            )
            .into()
        });
        let (_, variant_idx, key) = self
            .call_union_variant(call, context)
            .expect("union variant call was checked before lowering");
        assert_eq!(
            call.args.len(),
            1,
            "union variant constructors take one payload argument"
        );
        let operand = self
            .construct_operand(call.args[0], &None, context)
            .as_operand();

        let namespace = match self.expr_type(call_expr_idx, context) {
            Type::Sum(sum_type) => sum_type.name.map(|symbol| context.type_symbol_key(symbol)),
            _ => None,
        };
        let rvalue = Rvalue::UnionVariant(RvalueUnionVariant {
            idx: variant_idx,
            key,
            operand,
            namespace,
        });
        self.current_block_mut()
            .statements
            .push(Statement::assign(destination.clone(), rvalue));
        self.def_local(destination.local);
        destination
    }

    /// Lower compiler-provided Array operations without inventing function IDs
    /// for functions that do not exist as user-defined MIR functions.
    fn construct_array_call(
        &mut self,
        call_expr_idx: Idx<Expr>,
        call: &CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Place {
        let builtin = array_builtin_id(call, context).expect("Array builtin id");

        match builtin {
            BuiltinId::ArrayNew => {
                let return_ty = self.expr_type_idx(call_expr_idx, context);
                let destination = assign_to.clone().unwrap_or_else(|| {
                    self.construct_local(return_ty, call.symbol, Mutability::Not)
                        .into()
                });
                let capacity = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                self.current_block_mut().statements.push(Statement::assign(
                    destination.clone(),
                    Rvalue::ArrayNew(capacity),
                ));
                self.def_local(destination.local);
                destination
            }
            BuiltinId::ArrayLen | BuiltinId::ArrayCapacity => {
                let return_ty = context.core_types().int;
                let destination = assign_to.clone().unwrap_or_else(|| {
                    self.construct_local(return_ty, call.symbol, Mutability::Not)
                        .into()
                });
                let array = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                self.current_block_mut().statements.push(Statement::assign(
                    destination.clone(),
                    if builtin == BuiltinId::ArrayLen {
                        Rvalue::ArrayLen(array)
                    } else {
                        Rvalue::ArrayCapacity(array)
                    },
                ));
                self.def_local(destination.local);
                destination
            }
            BuiltinId::ArrayReserve => {
                let array = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                let capacity = Operand::from_op_or_place(
                    self.construct_operand(call.args[1], &None, context),
                    context,
                );
                self.current_block_mut()
                    .statements
                    .push(Statement::ArrayReserve { array, capacity });
                assign_to.clone().unwrap_or_else(|| {
                    self.construct_local(context.core_types().unit, call.symbol, Mutability::Not)
                        .into()
                })
            }
            BuiltinId::ArrayWriteNext => {
                let array = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                let index = Operand::from_op_or_place(
                    self.construct_operand(call.args[1], &None, context),
                    context,
                );
                let value = Operand::from_op_or_place(
                    self.construct_operand(call.args[2], &None, context),
                    context,
                );
                self.current_block_mut()
                    .statements
                    .push(Statement::ArrayWriteNext {
                        array,
                        index,
                        value,
                    });
                assign_to.clone().unwrap_or_else(|| {
                    self.construct_local(context.core_types().unit, call.symbol, Mutability::Not)
                        .into()
                })
            }
            _ => unreachable!("non-array builtin passed to array call lowering"),
        }
    }

    fn construct_string_builtin_call(
        &mut self,
        call_expr_idx: Idx<Expr>,
        call: &CallExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Place {
        let builtin = string_builtin_id(call, context).expect("String builtin id");
        let destination = assign_to.clone().unwrap_or_else(|| {
            self.construct_local(
                self.expr_type_idx(call_expr_idx, context),
                call.symbol,
                Mutability::Not,
            )
            .into()
        });

        let rvalue = match builtin {
            BuiltinId::StringByteLen => {
                let string = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                Rvalue::StringByteLen(string)
            }
            BuiltinId::StringConcat => {
                let left = Operand::from_op_or_place(
                    self.construct_operand(call.args[0], &None, context),
                    context,
                );
                let right = Operand::from_op_or_place(
                    self.construct_operand(call.args[1], &None, context),
                    context,
                );
                Rvalue::BinaryOp(crate::syntax::BinOpKind::Concat, Box::new((left, right)))
            }
            _ => unreachable!("non-String builtin passed to String call lowering"),
        };

        self.current_block_mut()
            .statements
            .push(Statement::assign(destination.clone(), rvalue));
        self.def_local(destination.local);
        destination
    }

    fn construct_callee(
        &mut self,
        callee: Idx<Expr>,
        signature_index: u32,
        type_args: &[Idx<Type>],
        context: &Context,
    ) -> Callee {
        let callee_expr = context.expr(callee);
        match callee_expr {
            Expr::VarRef(var_ref) if !type_args.is_empty() => Callee::from(
                self.ensure_generic_instance(var_ref.symbol, signature_index, type_args, context),
            ),
            Expr::VarRef(var_ref) => self
                .functions_map
                .get(&(var_ref.symbol, signature_index))
                .copied()
                .map(Callee::from)
                .unwrap_or_else(|| {
                    let local = self
                        .try_find_symbol(var_ref.symbol)
                        .unwrap_or_else(|| todo!("resolve indirect function value"));
                    Callee::Indirect(local)
                }),

            Expr::UnionVariant(variant) => {
                todo!("should never be hit?")
            }

            e => {
                dbg!(e);
                todo!(
                    "Internal Compiler Error (MIR): Unexpected 'callee_expr': {}",
                    e.display(context)
                )
            }
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
            .try_into_place()
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
        // TODO: assumes that scrutinee is an enum. eventually will have literal patterns
        // and more complex patterns like records

        let scrutinee = self.construct_operand(match_expr.scrutinee, &None, context);
        let scrutinee_place = scrutinee.try_into_place().expect("scrutinee to be a Place");

        let discriminant = self.construct_assign(
            Rvalue::Discriminant(scrutinee_place.clone()),
            context.core_types().int,
            None,
        );
        let discriminant = Operand::Copy(discriminant.into());

        let source_block = self.current_block;

        let mut match_arm_blocks = vec![];
        let mut branches = vec![];
        // TODO - rustc always generates an 'otherwise' block
        // if the match is exhaustive, it puts an "unreachable" no-op
        // should evaluate if/why we would want to do the same thing
        let mut otherwise = None;
        // Keep the fallback for exhaustive matches when there is no
        // standalone catch-all arm
        let mut exhaustive_fallback = None;
        for arm in match_expr.arms.iter() {
            self.current_block = self.new_block();
            match_arm_blocks.push(self.current_block);
            self.scopes.push();
            self.construct_match_pattern(
                arm.pattern,
                &scrutinee_place,
                &mut branches,
                &mut otherwise,
                &mut exhaustive_fallback,
                context,
            );
            if matches!(context.expr(arm.expr), Expr::Block(_)) {
                self.construct_block(arm.expr, None, assign_to, context);
            } else {
                self.construct_statement(arm.expr, assign_to, context);
            }
            self.scopes.pop();
        }

        let join_block = self.new_block();

        // jump each arm to the join block if it doesn't already have
        // a terminator, which could be set by control flow inside the arm
        // like a branch or return
        for block_idx in match_arm_blocks {
            if self.block(block_idx).terminator.is_none() {
                self.current_function_mut()
                    .predecessors
                    .add(block_idx, join_block);
                self.block_mut(block_idx).terminator = Some(Terminator::jump(join_block, vec![]));
            }
        }

        let targets = BranchIntTargets {
            branches: branches.into(),
            otherwise: otherwise.or(exhaustive_fallback),
        };

        self.construct_branch_terminator(source_block, discriminant, targets);

        self.current_block = join_block;
    }

    fn construct_match_pattern(
        &mut self,
        pattern: Idx<Pattern>,
        scrutinee_place: &Place,
        branches: &mut Vec<BranchTarget>,
        otherwise: &mut Option<BlockTarget>,
        exhaustive_fallback: &mut Option<BlockTarget>,
        context: &Context,
    ) {
        let scrutinee_ty = scrutinee_place.type_idx_of(self.current_function());
        let scrutinee_ty = context.type_(scrutinee_ty);
        let pattern = context.pattern(pattern);
        match pattern {
            hir::Pattern::Wild { meta: _ } => todo!(),
            hir::Pattern::IdentBinding { meta: _, binding } => {
                let new_place = match binding.variant {
                    Some(key) => {
                        let sum_type = assert_matches!(scrutinee_ty, hir::Type::Sum);
                        let idx = sum_type.index_of(key);
                        let idx = idx.unwrap_or_else(|| panic!("Internal Compiler Error (MIR): Found '{}' binding, expected that to exist on type {}",
                        context.lookup(key),
                        scrutinee_ty.display(context)));

                        scrutinee_place
                            .with_projection(ProjectionElem::DowncastVariant(Some(key), idx))
                    }
                    None => scrutinee_place.clone(),
                };

                self.construct_assign(
                    Rvalue::Use(Operand::Copy(new_place)),
                    self.value_type_idx(binding.symbol, context),
                    Some(binding.symbol),
                );

                if binding.variant.is_some() {
                    exhaustive_fallback.get_or_insert(BlockTarget {
                        target: self.current_block,
                        args: vec![],
                    });
                } else {
                    // Only a standalone binding (e.g. `otherwise`) handles
                    // unmatched discriminants.
                    otherwise.get_or_insert(BlockTarget {
                        target: self.current_block,
                        args: vec![],
                    });
                }
            }
            hir::Pattern::Variant { meta: _, pattern } => {
                let sum_type = assert_matches!(scrutinee_ty, hir::Type::Sum);
                let variant_index = sum_type.index_of(pattern.variant).unwrap_or_else(|| panic!(
                    "Internal Compiler Error (MIR): Found '{}' binding, expected that to exist on type {}",
                    context.lookup(pattern.variant),
                    scrutinee_ty.display(context)
                ));

                branches.push((
                    variant_index.into_raw() as i64,
                    BlockTarget::with_empty_args(self.current_block),
                ));

                if let Some(inner) = pattern.inner_pattern {
                    // make a new scrutinee_place to not clobber the outside one
                    let scrutinee_place = scrutinee_place.clone();

                    self.construct_match_pattern(
                        inner,
                        &scrutinee_place,
                        branches,
                        otherwise,
                        exhaustive_fallback,
                        context,
                    );
                }
            }
            hir::Pattern::IntLiteral { meta, literal } => todo!(),
            hir::Pattern::FloatLiteral { meta, literal } => todo!(),
            hir::Pattern::StringLiteral { meta, literal } => todo!(),
        }
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
            otherwise: Some(BlockTarget::with_empty_args(then_block)),
        };

        self.construct_branch_terminator(source_block, discriminant, targets);

        // The ScopesStack should be correct after popping the if/else scopes
        // which means the original hir::block should be able to continue
        // constructing the next BasicBlock(s) by starting at its current statement
        self.current_block = join_block;

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
        self.current_function().local_symbols.get(&symbol).copied()
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
        let ty = self.expr_type_idx(expr, context);

        match context.expr(expr) {
            Expr::FloatLiteral(f) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*f).into()))
            }
            Expr::IntLiteral(i) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*i).into()))
            }
            Expr::StringLiteral(s) => {
                self.construct_operand_assign(assign_to, Operand::Constant((*s).into()))
            }
            Expr::Unary(_) => todo!(),
            Expr::Block(_) => todo!(),
            Expr::Call(call) => self.construct_call_operand(expr, call, ty, assign_to, context),
            Expr::VarRef(var_ref) => {
                OperandOrPlace::from(self.construct_var_ref_operand(var_ref, assign_to, context))
            }
            Expr::Path(path_expr) => {
                OperandOrPlace::from(self.construct_path_expr(path_expr, assign_to, context))
            }
            Expr::RecordLiteral(record) => {
                let record_ty = self.expr_type_idx(expr, context);

                let assign_place = match assign_to {
                    Some(assign_place) => assign_place.clone(),
                    None => self
                        .construct_local(record_ty, None, Mutability::Not)
                        .into(),
                };

                // Each field value can be its own arbitrarily complex expression,
                // so recursively calculate those operands first
                let mut field_operands = Vec::new();
                for (field_key, field_expr) in &record.fields {
                    let field_op_or_place = self.construct_operand(*field_expr, &None, context);

                    let field_operand = Operand::from_op_or_place(field_op_or_place, context);
                    field_operands.push((*field_key, field_operand));
                }

                let rvalue = Rvalue::Aggregate(field_operands);

                let assign = Statement::assign(assign_place.clone(), rvalue);
                self.current_block_mut().statements.push(assign);

                OperandOrPlace::from(assign_place)
            }
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
                expr.display(context)
            ),
        }
    }

    fn construct_var_ref_operand(
        &mut self,
        var_ref: &hir::VarRefExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Operand {
        let operand = if let Some(local_idx) = self.try_find_symbol(var_ref.symbol) {
            Operand::from_place(Place::from(local_idx), context)
        } else {
            // TODO - remove after "false" and "true" are added to the prelude in HIR
            // check if it's a defined symbol like "false" or "true" from the Bool union
            match context.type_of_value(&var_ref.symbol) {
                Type::Sum(sum_type) => {
                    let key = context.value_symbol_key(var_ref.symbol);
                    let index_of = sum_type.index_of(key);
                    let index_of = index_of.unwrap_or_else(|| {
                        panic!(
                            "expected symbol {} to have been defined in union {}",
                            var_ref.symbol.display(context),
                            sum_type.display(context)
                        )
                    });
                    Operand::Constant(Constant::Int(index_of.into_raw() as i64))
                }
                Type::Function(function_type) if function_type.signatures.len() == 1 => {
                    let function_id = self
                        .functions_map
                        .get(&(var_ref.symbol, 0))
                        .copied()
                        .unwrap_or_else(|| {
                            todo!(
                                "function value for `{}` is not a concrete function",
                                context.value_symbol_str(var_ref.symbol)
                            )
                        });
                    Operand::Constant(Constant::Func {
                        func_id: function_id,
                        type_idx: self.value_type_idx(var_ref.symbol, context),
                    })
                }
                ty => todo!(
                    "Internal Compiler Error (MIR): Expected Type::Sum, found {}",
                    ty.display(context)
                ),
            }
        };

        if let Some(assign_place) = assign_to {
            let assign = Statement::assign(assign_place.clone(), operand.clone());
            self.current_block_mut().statements.push(assign);
            self.def_local(assign_place.local);
        };

        operand
    }

    fn construct_path_expr(
        &mut self,
        path_expr: &hir::PathExpr,
        assign_to: &Option<Place>,
        context: &Context,
    ) -> Operand {
        let operand = match self.expr_type(path_expr.subject, context) {
            Type::Sum(sum_type) => todo!("TODO - determine if this is possible - may not be because you're supposed to deconstruct an instance of the sum type with 'match' because you don't know the fields without checking the discriminant first"),
            Type::UnionNamespace(union_namespace_type) => {
                let member_key = match path_expr.member.map(|member| context.expr(member)) {
                    Some(hir::Expr::PathSegment(segment)) => segment.key,
                    other => panic!(
                        "Internal Compiler Error (MIR): expected union namespace path member to be a PathSegment, got {other:?}"
                    ),
                };
                let (i, variant_constructor_ty) =
                    union_namespace_type.variant_constructors[&member_key];
                let variant_constructor_ty = context.type_(variant_constructor_ty);
                match variant_constructor_ty {
                    Type::Sum(_) => Operand::Constant(Constant::Int(i as i64)),
                    Type::Function(function_type) => {
                        let signature = &function_type.signatures[0];
                        let FuncSignature { return_ty, .. } = signature;

                        todo!()
                    }
                    t => unreachable!("unreachable: {:?} {}", t, t.display(context)),
                }
            }
            Type::Product(product_type) => {
                let mut place = match context.expr(path_expr.subject) {
                    Expr::VarRef(var_ref) => {
                        // For variable references, the Place can be made directly
                        let local_idx = self.find_symbol(var_ref.symbol, context);
                        Place::from(local_idx)
                    }
                    _ => {
                        // For other expressions, need to construct an operand to store in a temporary place
                        let subject_op_or_place = self.construct_operand(path_expr.subject, &None, context);
                        match subject_op_or_place {
                            OperandOrPlace::Place(place) => place,
                            OperandOrPlace::Operand(operand) => {
                                let subject_ty = self.expr_type_idx(path_expr.subject, context);
                                let temp_local = self.construct_local(subject_ty, None, Mutability::Not);
                                let temp_place: Place = temp_local.into();
                                let assign = Statement::assign(temp_place.clone(), operand);
                                self.current_block_mut().statements.push(assign);
                                temp_place
                            }
                        }
                    }
                };

                // Walk the recursive PathExpr structure, accumulating projections until the member is None.
                //
                // The first "segment" projection is represented by `path_expr.member` (and then further
                // segments are nested inside `Expr::Path`).
                let mut current_expr: &hir::PathExpr = path_expr;
                let mut current_ty = self.expr_type(current_expr.subject, context);

                loop {
                    let Some(member_idx) = current_expr.member else {
                        break;
                    };

                    let member_expr = context.expr(member_idx);

                    match member_expr {
                        hir::Expr::PathSegment(segment_expr) => {
                            let field_key = segment_expr.key;

                            let product_type = assert_matches!(current_ty, Type::Product);

                            let (field_index, field_type_idx) = product_type.fields[&field_key];
                            let field_type = context.type_(field_type_idx);

                            let field_idx = FieldIdx::new(field_index);
                            let projection = ProjectionElem::Field(field_idx, field_type.clone());
                            place = place.with_projection(projection);

                            // current_ty = field_type;

                            // No more nested path segments after a plain segment.
                            break;
                        }

                        hir::Expr::Path(inner_path) => {
                            // The inner path represents additional segments.
                            // Apply its subject segment as a field projection, then continue.
                            let inner_subject = context.expr(inner_path.subject);
                            let field_key = assert_matches!(inner_subject, hir::Expr::PathSegment).key;

                            let product_type = assert_matches!(current_ty, Type::Product);

                            let (field_index, field_type_idx) = product_type.fields[&field_key];
                            let field_type = context.type_(field_type_idx);

                            let field_idx = FieldIdx::new(field_index);
                            let projection = ProjectionElem::Field(field_idx, field_type.clone());
                            place = place.with_projection(projection);

                            current_ty = field_type;
                            current_expr = inner_path;
                        }

                        other => unreachable!(
                            "Internal Compiler Error (MIR): invalid member of path expression: {other:?}"
                        ),
                    }
                }

                Operand::Copy(place)
            }
            t => todo!("handle {t:?}"),
        };

        if let Some(assign_place) = assign_to {
            let assign = Statement::assign(assign_place.clone(), operand.clone());
            self.current_block_mut().statements.push(assign);
            self.def_local(assign_place.local);
        };

        operand
    }

    fn construct_path_segment(
        &mut self,
        segment: Idx<Expr>,
        assign_to: &Option<Place>,
        context: &Context,
    ) {
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
            if let Some(symbol) = symbol {
                func.local_symbols.insert(symbol, local);
            }
            local
        };

        self.def_local(local);

        local
    }
}

fn array_builtin_id(call: &CallExpr, context: &Context) -> Option<BuiltinId> {
    let Expr::VarRef(var_ref) = context.expr(call.callee) else {
        return None;
    };
    context.builtin_id(var_ref.symbol).filter(|builtin| {
        matches!(
            builtin,
            BuiltinId::ArrayNew
                | BuiltinId::ArrayLen
                | BuiltinId::ArrayCapacity
                | BuiltinId::ArrayReserve
                | BuiltinId::ArrayWriteNext
        )
    })
}

fn string_builtin_id(call: &CallExpr, context: &Context) -> Option<BuiltinId> {
    let Expr::VarRef(var_ref) = context.expr(call.callee) else {
        return None;
    };
    context.builtin_id(var_ref.symbol).filter(|builtin| {
        matches!(
            builtin,
            BuiltinId::StringByteLen | BuiltinId::StringConcat
        )
    })
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
