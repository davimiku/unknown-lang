use std::ops::{Index, IndexMut};

use cranelift::prelude::Variable;
use la_arena::Idx;
use mir::Local;

#[derive(Debug, Clone, Default)]
pub(crate) struct VariableMap {
    locals: Vec<Idx<Local>>,
}

impl VariableMap {
    pub(crate) fn insert(&mut self, var: Variable, local: Idx<Local>) {
        self[var] = local;
    }
}

impl Index<Variable> for VariableMap {
    type Output = Idx<Local>;

    fn index(&self, var: Variable) -> &Self::Output {
        let i = var.as_u32() as usize;
        &self.locals[i]
    }
}

impl IndexMut<Variable> for VariableMap {
    fn index_mut(&mut self, var: Variable) -> &mut Self::Output {
        let i = var.as_u32() as usize;
        &mut self.locals[i]
    }
}
