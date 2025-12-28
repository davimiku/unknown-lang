use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::ir::StackSlot;
use cranelift::prelude::{Value, Variable};
use la_arena::Idx;
use mir::Local;

use crate::layout::Layout;

/// Represents a place where a value can be read or written
///
/// Lowered from and analogous to mir::Place, but for usage in
/// CLIF codegen
#[derive(Debug, Clone)]
pub(crate) enum CPlace {
    /// One-to-one mapping between a MIR local and a CLIF variable
    Var {
        local: Local,
        variable: Variable,
        layout: Idx<Layout>,
    },
    /// A MIR local that has been split into two CLIF variables
    ///
    /// Some examples could include:
    /// - union (split tag and data)
    /// - structs with two fields
    /// - closures (data ptr and function ptr)
    /// - "trait objects" (data ptr and vtable ptr)
    VarPair {
        local: Local,
        first: Variable,
        second: Variable,
        layout: Idx<Layout>,
    },
    /// Place representing an address to another location, such as a stack slot or heap allocation
    Address {
        pointer: Pointer,
        layout: Idx<Layout>,
    },
}

impl CPlace {
    // TODO - smallvec optimization
    pub(crate) fn variables(&self) -> Vec<Variable> {
        match self {
            CPlace::Var { variable, .. } => vec![*variable],
            CPlace::VarPair { first, second, .. } => vec![*first, *second],
            CPlace::Address { .. } => vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CValue {
    Ref {
        ptr: Pointer,
        val: Option<Value>,
        layout: Idx<Layout>,
    },
    Val {
        val: Value,
        layout: Idx<Layout>,
    },
    ValPair {
        first: Value,
        second: Value,
        layout: Idx<Layout>,
    },
    ValTriple {
        first: Value,
        second: Value,
        third: Value,
        layout: Idx<Layout>,
    },
}

impl CValue {
    pub(crate) fn as_val(&self) -> Option<Value> {
        match self {
            CValue::Ref { .. } => None,
            CValue::Val { val, .. } => Some(*val),
            CValue::ValPair { .. } => None,
            CValue::ValTriple { .. } => None,
        }
    }
}

pub(crate) fn to_vec_values(cvalues: Vec<CValue>) -> Vec<Value> {
    let mut values = Vec::with_capacity(cvalues.len());
    for cvalue in cvalues {
        match cvalue {
            CValue::Ref { val, .. } => {
                if let Some(val) = val {
                    values.push(val);
                }
            }
            CValue::Val { val, .. } => values.push(val),
            CValue::ValPair { first, second, .. } => {
                values.push(first);
                values.push(second);
            }
            CValue::ValTriple {
                first,
                second,
                third,
                ..
            } => {
                values.push(first);
                values.push(second);
                values.push(third);
            }
        }
    }
    values
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Pointer {
    Heap { addr: Value, offset: Offset32 },
    Stack { slot: StackSlot, offset: Offset32 },
}

impl Pointer {
    fn offset(&self) -> Offset32 {
        match self {
            Pointer::Heap { offset, .. } => *offset,
            Pointer::Stack { offset, .. } => *offset,
        }
    }
}
