//! Entry point of constructing the MIR (Control Flow Graph) from the root HIR node.

use hir::Expr;
use hir::FunctionParam;
use hir::IntrinsicExpr;
use hir::ValueSymbol;
use hir::VarDefExpr;
use la_arena::Idx;

use crate::syntax::BinOp;
use crate::syntax::Local;
use crate::syntax::Mutability;
use crate::syntax::{BasicBlock, Constant, Operand, Place, Rvalue, Statement, Terminator};
use crate::{Builder, Function};
use util_macros::assert_matches;

impl Builder {
    // allocates new function
    fn construct_new_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
        context: &hir::Context,
    ) -> Idx<Function> {
        self.functions.alloc(Function::new(function_expr.name));

        self.construct_function(function_expr, context)
    }

    // construct into existing allocated Function
    pub(crate) fn construct_function(
        &mut self,
        function_expr: &hir::FunctionExpr,
        context: &hir::Context,
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
        self.construct_block(statements, context);

        self.current_block_mut().terminator = Terminator::Return;

        self.current_function
    }

    fn construct_param(&mut self, param: &FunctionParam, context: &hir::Context) {
        let ty_idx = context.type_idx_of_value(&param.symbol);
        self.current_function_mut().params.push(ty_idx);
        let ty = context.type_idx_of_value(&param.symbol);
        self.make_local(ty, Some(param.symbol), Mutability::Not); // TODO: depends on the param
    }

    fn new_block(&mut self) -> Idx<BasicBlock> {
        self.current_block = self.current_function_mut().new_block();
        self.current_block
    }

    fn construct_new_block(
        &mut self,
        statements: &[Idx<Expr>],
        context: &hir::Context,
    ) -> Idx<BasicBlock> {
        let created_block = self.new_block();
        self.construct_block(statements, context);

        created_block
    }

    /// Constructs the provided statements into the current Basic Block
    // FIXME: how do we capture the `place` of `let place = { ... }`
    fn construct_block(&mut self, statements: &[Idx<Expr>], context: &hir::Context) {
        let statements_iter = statements
            .iter()
            .enumerate()
            .map(|(i, el)| (el, i == statements.len() - 1));

        for (hir_statement, is_last) in statements_iter {
            let return_place = if is_last {
                if self.scope_depth == 0 {
                    Some(self.current_function().return_place())
                } else {
                    todo!("capture the assignplace in `let a = {{ ... }}`")
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
        context: &hir::Context,
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

    fn construct_var_def(&mut self, var_def: &VarDefExpr, context: &hir::Context) {
        // TODO: capture mutability in parser, AST, HIR, and through here
        let ty = context.expr_type_idx(var_def.value);
        let local = self.make_local(ty, Some(var_def.symbol), Mutability::Not);
        let place = Place {
            local,
            projection: vec![],
        };
        let rvalue = self.construct_rvalue(context.expr(var_def.value));
        let statement = Statement::Assign(Box::new((place, rvalue)));
        self.current_block_mut().statements.push(statement);
    }

    fn construct_statement(
        &mut self,
        statement_expr: Idx<Expr>,
        context: &hir::Context,
        // TODO: decide if this is the best way to indicate when the statement is the "return value" of the function/block
        assign_place: Option<Place>,
    ) {
        let statement_expr = context.expr(statement_expr);

        match statement_expr {
            Expr::BoolLiteral(_) | Expr::FloatLiteral(_) | Expr::StringLiteral(_) => todo!(),

            Expr::IntLiteral(i) => {
                if let Some(assign_place) = assign_place {
                    let operand = Operand::Constant(Constant::Int(*i));
                    let rvalue = Rvalue::Use(operand);
                    let assign = Statement::Assign(Box::new((assign_place, rvalue)));
                    self.current_block_mut().statements.push(assign);
                }
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
                self.construct_block(block.exprs(), context);
                // FIXME: new_block terminator needs to be set to the next block
                // TODO: there could be a return value here
                // fun () => {
                //     // stuff...
                //     { return_val }
                // }
                // i.e. it is a "statement block" but it is the last statement of another
                // block therefore it should assign the _0 return
                todo!()
            }

            Expr::Call(call) => {
                // FIXME: pull return_place only if is last statement of the *function*
                // otherwise make a new place/local for assigning
                let place = self.current_function().return_place();

                // FIXME: create try_make_binop_rvalue or whatever and use that here
                if let Some(binop) = try_get_binop(call, context) {
                    self.construct_binop(context, call, binop, place);
                }

                // FIXME: otherwise, create a Call terminator
                // direct Call for non-capturing functions
                // indirect Call for capturing/closures (not implemented for a while)
            }

            Expr::VarRef(var_ref) => {
                if let Some(assign_place) = assign_place {
                    let local = self.find_symbol(var_ref.symbol);
                    let place = Place {
                        local,
                        projection: vec![],
                    };
                    let operand = Operand::Copy(place);
                    let rvalue = Rvalue::Use(operand);
                    let assign = Statement::Assign(Box::new((assign_place, rvalue)));
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

    fn construct_binop(
        &mut self,
        context: &hir::Context,
        call: &hir::CallExpr,
        binop: BinOp,
        place: Place,
    ) {
        // FIXME: where do we construct the Call terminator for user-defined operations?
        // such as Point + Point
        let arg1 = context.expr(call.args[0]);
        let operand1 = self.construct_operand(arg1);

        let arg2 = context.expr(call.args[1]);
        let operand2 = self.construct_operand(arg2);
        let rvalue = Rvalue::BinaryOp(binop, Box::new((operand1, operand2)));
        let assign_statement = Statement::Assign(Box::new((place, rvalue)));

        self.current_block_mut().statements.push(assign_statement);
    }

    fn find_symbol(&self, symbol: ValueSymbol) -> Idx<Local> {
        self.current_function()
            .locals_map
            .iter()
            .find(|(_, s)| **s == Some(symbol))
            .expect("should only have access to symbols defined in this function")
            .0
    }

    fn construct_expr(&mut self, expr: Idx<Expr>, context: &hir::Context) {
        let expr = context.expr(expr);
        match expr {
            Expr::Empty => unreachable!(
                "compiler bug: MIR construction should not have been called with Empty expression"
            ),
            Expr::UnresolvedVarRef { .. } => {
                unreachable!("compiler bug: MIR construction should not have been called with unresolved variables")
            }

            Expr::BoolLiteral(b) => {
                let constant = Constant::Int(*b as i64);
            }
            Expr::FloatLiteral(f) => {
                let constant = Constant::Float(*f);
            }
            Expr::IntLiteral(i) => {
                let constant = Constant::Int(*i);
            }
            Expr::StringLiteral(s) => {
                todo!("need to think more about the representation of strings in MIR")
            }

            Expr::ArrayLiteral(_) => todo!(),

            // TODO: will be removed in favor of Call
            Expr::Unary(_) => unreachable!("remove me after converting HIR to Call"),

            Expr::Block(block) => {
                let old_block = self.current_block;
                let created_block = self.construct_new_block(block.exprs(), context);

                self.block_mut(old_block).terminator = Terminator::Goto {
                    target: created_block,
                }
            }

            Expr::Call(call) => {
                let curr_idx = self.current_block;
                let next_block = self.new_block();

                let func: Operand = todo!();
                let args: Box<[Operand]> = todo!();
                let destination: Place = todo!();
                let target: Option<Idx<BasicBlock>> = todo!();

                self.block_mut(curr_idx).terminator = Terminator::Call {
                    func,
                    args,
                    destination,
                    target,
                };
                // call.
                todo!()
            }

            Expr::VarRef(var_ref) => {}
            Expr::Path(_) => todo!(),
            Expr::IndexInt(_) => todo!(),
            Expr::Function(_) => todo!(),
            Expr::If(_) => todo!(),

            Expr::VarDef(var_def) => {
                self.construct_var_def(var_def, context);
            }

            Expr::Statement(_) => {
                // HIR should have just 1 expression nested inside a statement
                // so the expression should have already been pulled out
                unreachable!("Compiler bug (MIR): Found hir::Expr::Statement directly nested inside hir::Expr::Statement")
            }
            Expr::ReturnStatement(_) => todo!(),

            _ => {
                eprintln!("Compiler Bug (MIR): Unknown expression {expr:?}");
            }
        }
    }

    fn construct_rvalue(&self, expr: &Expr) -> Rvalue {
        todo!()
    }

    fn construct_operand(&self, expr: &Expr) -> Operand {
        if let Some(constant) = self.construct_constant(expr) {
            return Operand::Constant(constant);
        }

        match expr {
            Expr::VarRef(var_ref) => {
                let local = self.find_symbol(var_ref.symbol);
                let place = Place {
                    local,
                    projection: vec![],
                };
                Operand::Copy(place)
            }
            _ => panic!("something went wrong!"),
        }
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

fn try_get_binop(call: &hir::CallExpr, context: &hir::Context) -> Option<BinOp> {
    if let Some(symbol) = call.symbol {
        if let Some(f) = context.lookup_function_symbol(symbol) {
            if let Expr::Intrinsic(intrinsic) = context.expr(f) {
                return match intrinsic {
                    IntrinsicExpr::Add => BinOp::Add,
                    IntrinsicExpr::Sub => BinOp::Sub,
                    IntrinsicExpr::Mul => BinOp::Mul,
                    IntrinsicExpr::Div => BinOp::Div,
                    IntrinsicExpr::Rem => BinOp::Rem,
                    IntrinsicExpr::Eq => BinOp::Eq,
                    IntrinsicExpr::Ne => BinOp::Ne,
                    IntrinsicExpr::Lt => BinOp::Lt,
                    IntrinsicExpr::Le => BinOp::Le,
                    IntrinsicExpr::Gt => BinOp::Gt,
                    IntrinsicExpr::Ge => BinOp::Ge,
                }
                .into();
            }
        }
    }
    None
}
