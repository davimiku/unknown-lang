use lasso::Spur;
use vm_codegen::Chunk;

mod function_arity;

/// Metadata (stored on the heap) that describes
#[derive(Debug)]
pub struct Boxed<'a> {
    data: BoxedData<'a>,
    next: Option<Box<Boxed<'a>>>,
}

impl<'a> Boxed<'a> {
    pub fn new_function(chunk: &'a Chunk, parameter_slots: u32, name: Option<Spur>) -> Self {
        let function = BoxedFunction {
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
pub enum BoxedData<'chunk> {
    /// Dynamically generated strings above a certain size are boxed
    String,

    /// Functions that cannot be proven to be static are boxed
    Function(BoxedFunction<'chunk>),

    /// Closures that cannot be proven to be static and capture no boxed items are boxed
    Closure(BoxedClosure),

    /// User defined `union` above a certain size are boxed
    Union(BoxedUnion),

    /// User defined `struct` above a certain size are boxed
    Struct(BoxedStruct),
}

/// Metadata for a function, stored on the heap
#[derive(Debug)]
pub struct BoxedFunction<'chunk> {
    parameter_slots: u32, // TODO: work out the encoding for arity
    chunk: &'chunk Chunk,
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