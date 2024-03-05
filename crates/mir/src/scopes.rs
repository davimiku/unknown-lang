use std::collections::HashMap;

use hir::ValueSymbol;
use la_arena::Idx;

use crate::Local;

#[derive(Debug)]
pub struct ScopesStack(Vec<Scope>);

impl ScopesStack {
    pub fn push(&mut self, working_block: Idx<hir::Expr>) {
        self.0.push(Scope::new(working_block))
    }

    pub fn pop(&mut self) {
        debug_assert!(self.0.len() > 1, "pop() must never remove the last scope");
        self.0.pop();
    }

    pub fn peek(&self) -> &Scope {
        self.0.last().unwrap()
    }

    pub fn peek_statement_counter(&self) -> usize {
        self.peek().statement_counter
    }

    pub fn increment_statement_counter(&mut self) {
        self.0.last_mut().unwrap().statement_counter += 1;
    }

    pub fn insert_local(&mut self, local: Idx<Local>, symbol: Option<ValueSymbol>) {
        self.0.last_mut().unwrap().locals.insert(local, symbol);
    }
}

impl Default for ScopesStack {
    fn default() -> Self {
        let idx = Idx::from_raw(u32::MAX.into());
        Self(vec![Scope::new(idx)])
    }
}

#[derive(Debug)]
pub struct Scope {
    statement_counter: usize,

    working_block: Idx<hir::Expr>,

    locals: HashMap<Idx<Local>, Option<ValueSymbol>>,
}

impl Scope {
    fn new(working_block: Idx<hir::Expr>) -> Self {
        Self {
            statement_counter: Default::default(),
            working_block,
            locals: Default::default(),
        }
    }
}
