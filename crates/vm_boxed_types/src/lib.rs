use lasso::Spur;
use vm_codegen::FunctionChunk;

mod function_arity;

/// Metadata (stored on the heap) that describes
#[derive(Debug)]
pub struct Boxed {
    data: BoxedData,
    next: Option<Box<Boxed>>,
}

impl Boxed {
    pub fn new_function(chunk: FunctionChunk, parameter_slots: u32, name: Option<Spur>) -> Self {
        let function = VMFunction {
            parameter_slots,
            chunk,
            name,
        };
        Self {
            data: BoxedData::Function(function),
            next: None,
        }
    }
}

#[derive(Debug)]
pub enum BoxedData {
    /// Dynamically generated strings above a certain size are boxed
    String,

    /// Functions that cannot be proven to be static are boxed
    Function(VMFunction),

    /// Closures that cannot be proven to be static and capture no boxed items are boxed
    Closure(BoxedClosure),

    /// User defined `union` above a certain size are boxed
    Union(BoxedUnion),

    /// User defined `struct` above a certain size are boxed
    Struct(BoxedStruct),
}

/// Metadata for a function, stored on the heap
#[derive(Debug)]
pub struct VMFunction {
    parameter_slots: u32,
    chunk: FunctionChunk,
    name: Option<Spur>,
}

#[derive(Debug)]
pub struct BoxedClosure {
    //
}

#[derive(Debug)]
pub struct BoxedUnion {
    //
}

#[derive(Debug)]
pub struct BoxedStruct {
    //
}
