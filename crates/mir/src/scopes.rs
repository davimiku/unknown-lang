use std::collections::HashMap;

use hir::ValueSymbol;
use la_arena::Idx;

use crate::Local;

#[derive(Debug)]
pub struct ScopesStack(Vec<Scope>);

impl ScopesStack {
    pub fn push(&mut self) {
        self.0.push(Scope::new())
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
        Self(vec![Scope::new()])
    }
}

#[derive(Debug)]
pub struct Scope {
    statement_counter: usize,

    // TODO: is this being used?
    // if it is, should be an ArenaMap with Idx as the key
    locals: HashMap<Idx<Local>, Option<ValueSymbol>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            statement_counter: Default::default(),
            locals: Default::default(),
        }
    }
}
