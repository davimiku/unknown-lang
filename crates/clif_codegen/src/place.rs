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
    fn layout(&self) -> Idx<Layout> {
        match self {
            CPlace::Var { layout, .. } => *layout,
            CPlace::VarPair { layout, .. } => *layout,
            CPlace::Address { layout, .. } => *layout,
        }
    }

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
    ByRef {
        ptr: Pointer,
        val: Option<Value>,
        layout: Idx<Layout>,
    },
    ByVal {
        val: Value,
        layout: Idx<Layout>,
    },
    ByValPair {
        first: Value,
        second: Value,
        layout: Idx<Layout>,
    },
}

impl CValue {
    pub(crate) fn as_val(&self) -> Option<Value> {
        match self {
            CValue::ByRef { .. } => None,
            CValue::ByVal { val, .. } => Some(*val),
            CValue::ByValPair { .. } => None,
        }
    }

    pub(crate) fn as_valpair(&self) -> Option<(Value, Value)> {
        match self {
            CValue::ByRef { .. } => None,
            CValue::ByVal { .. } => None,
            CValue::ByValPair { first, second, .. } => Some((*first, *second)),
        }
    }
}

pub(crate) fn to_vec_values(cvalues: Vec<CValue>) -> Vec<Value> {
    let mut values = Vec::with_capacity(cvalues.len());
    for cvalue in cvalues {
        match cvalue {
            CValue::ByRef { val, .. } => {
                if let Some(val) = val {
                    values.push(val);
                }
            }
            CValue::ByVal { val, .. } => values.push(val),
            CValue::ByValPair { first, second, .. } => {
                values.push(first);
                values.push(second);
            }
        }
    }
    values
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Pointer {
    Addr { addr: Value, offset: Offset32 },
    Stack { slot: StackSlot, offset: Offset32 },
}

impl Pointer {
    fn offset(&self) -> Offset32 {
        match self {
            Pointer::Addr { offset, .. } => *offset,
            Pointer::Stack { offset, .. } => *offset,
        }
    }
}
