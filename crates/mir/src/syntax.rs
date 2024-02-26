use hir::{Key, Type, ValueSymbol};
use la_arena::{Arena, ArenaMap, Idx};

// rustc calls a function "Body"
// TODO: is that a good name?
// #[derive(Debug)]
// pub struct Body {
//     pub basic_blocks: Vec<Idx<BasicBlock>>,
//     // vec of local declarations
//     // vec of user type annotations?
//     // source scopes for debug info

//     // param count of the function

//     // span / source location

//     // function coverage
//     // debug info

//     // extra state to keep that is used during lowering
//     // but not directly related to an input or output
//     // - phase
//     // - number of passes
// }

#[derive(Debug)]
pub struct Function {
    /// Interned string name of the function with the symbol, or None for anonymous functions
    pub name: Option<(Key, ValueSymbol)>,

    /// Blocks that represent the control flow through this function
    pub blocks: Arena<BasicBlock>,

    /// Parameters to the function, with their types
    ///
    /// TODO: add source location info for potential error reporting?
    pub params: Vec<Idx<Type>>,

    /// Temporary variables
    // TODO: does this need to track Place too?
    pub locals: Arena<Local>,

    pub locals_map: ArenaMap<Idx<Local>, Option<ValueSymbol>>,
}

impl Default for Function {
    fn default() -> Self {
        let mut blocks = Arena::new();

        blocks.alloc(BasicBlock::default());

        Self {
            name: None,
            blocks,
            params: Vec::default(),
            locals: Arena::default(),
            locals_map: ArenaMap::default(),
        }
    }
}

impl Function {
    pub fn new(name: Option<(Key, ValueSymbol)>) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }
}

impl Function {
    pub fn new_block(&mut self) -> Idx<BasicBlock> {
        self.blocks.alloc(BasicBlock::default())
    }

    /// Returns the initial_block of the function, or panics otherwise
    ///
    /// The invariant of always having an initial block is maintained by the only
    /// public constructor allocating an initial block (via Default trait impl)
    pub fn entry_block(&self) -> Idx<BasicBlock> {
        self.blocks
            .iter()
            .next()
            .expect("function to have at least one block")
            .0
    }

    pub fn param_locals(&self) -> impl Iterator<Item = (Idx<Local>, &Local)> {
        self.locals.iter().skip(1).take(self.params.len())
    }

    pub fn return_place(&self) -> Place {
        let (local_idx, _) = self.return_local();
        Place {
            local: local_idx,
            projection: vec![],
        }
    }

    pub fn return_local(&self) -> (Idx<Local>, &Local) {
        self.locals
            .iter()
            .next()
            .expect("local _0 should always exist which is the return value")
    }

    pub fn return_ty(&self) -> Idx<Type> {
        let (_, return_local) = self.return_local();
        return_local.ty_idx()
    }
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Debug)]
pub enum Statement {
    Assign(Box<(Place, Rvalue)>),

    SetDiscriminant {
        place: Box<Place>,
        variant_index: VariantIdx,
    },

    StorageLive(Idx<Local>),
    StorageDead(Idx<Local>),
    // TODO: decide if we need this, was copied from rustc
    // Intrinsic(Box<NonDivergingIntrinsic>),
    Intrinsic(Box<()>),
    // / This statement exists to preserve a trace of a scrutinee matched against a wildcard binding.
    // / This is especially useful for `let _ = PLACE;` bindings that desugar to a single
    // / `PlaceMention(PLACE)`.
    // /
    // / When executed at runtime, this computes the given place, but then discards
    // / it without doing a load. It is UB if the place is not pointing to live memory.
    //PlaceMention(Box<Place<'tcx>>),

    // / Carries control-flow-sensitive information injected by `-Cinstrument-coverage`,
    // / such as where to generate physical coverage-counter-increments during codegen.
    // /
    // / Coverage statements are used in conjunction with the coverage mappings and other
    // / information stored in the function's
    // / [`mir::Body::function_coverage_info`](crate::mir::Body::function_coverage_info).
    // / (For inlined MIR, take care to look up the *original function's* coverage info.)
    // /
    // / Interpreters and codegen backends that don't support coverage instrumentation
    // / can usually treat this as a no-op.
    // Coverage(Box<Coverage>),

    // No-op. Useful for deleting instructions without affecting statement indices.
    // Noop,
}

/// Source-order index of a variant in a union type
#[derive(Debug)]
pub struct VariantIdx {
    private: u32,
}

#[derive(Debug, Default)]
pub enum Terminator {
    /// Continues execution in the next block.
    /// This terminator has a single successor.
    Goto { target: Idx<BasicBlock> },

    /// Returns from the current function
    ///
    /// By definition, this also terminates the current block.
    /// Assigns the value currently in the return place (_0) to
    /// the place specified in the associated Call terminator in
    /// the calling function, as if assigned via dest = move _0.
    Return,

    Call {
        /// The function that’s being called.
        func: Operand,
        /// Arguments the function is called with.
        /// These are owned by the callee, which is free to modify them.
        /// This allows the memory occupied by "by-value" arguments to be
        /// reused across function calls without duplicating the contents.
        /// The span for each arg is also included
        /// (e.g. `a` and `b` in `x.foo(a, b)`).
        args: Box<[Operand]>,
        /// Where the returned value will be written
        destination: Place,
        /// Where to go after this call returns. If none, the call necessarily diverges.
        target: Option<Idx<BasicBlock>>,
        // EXAMPLE FROM RUSTC:
        // / Action to be taken if the call unwinds.
        // unwind: UnwindAction,
        // / Where this call came from in HIR/THIR.
        // call_source: CallSource,
        // / This `Span` is the span of the function, without the dot and receiver
        // / e.g. `foo(a, b)` in `x.foo(a, b)`
        // fn_span: Span,
    },

    /// Drops a value from memory
    ///
    /// TODO: what will this be used for?
    /// in the future, when `resource` is added, this should call the disposal function?
    /// Is this applicable for values with Rc heap allocations like strings?
    Drop {
        place: Place,
        target: Idx<BasicBlock>,
        // unwind: UnwindAction,
        // replace: bool,
    },

    #[default]
    Unreachable,
}

impl Terminator {
    pub const fn name(&self) -> &'static str {
        match self {
            Terminator::Goto { .. } => "Goto",
            Terminator::Return => "Return",
            Terminator::Call { .. } => "Call",
            Terminator::Drop { .. } => "Drop",
            Terminator::Unreachable => "Unreachable",
        }
    }
}

#[derive(Debug)]
pub struct Place {
    pub local: Idx<Local>,
    pub projection: Vec<PlaceElem>,
}

type PlaceElem = ProjectionElem<Idx<Local>, hir::Type>;

// struct Ty<'tcx>(Interned<'tcx, WithCachedTypeInfo<TyKind<'tcx>>>);

// struct Interned<'a, T>(pub &'a T, pub PrivateZst);

/// Projections, which are fields or other things that "project out" from
/// a base place. These are represented by the newtype'd type ProjectionElem.
/// So e.g. the place _1.f is a projection, with f being the "projection element"
/// and _1 being the base path. *_1 is also a projection, with the * being
/// represented by the ProjectionElem::Deref element.
#[derive(Debug)]
pub enum ProjectionElem<V, T> {
    Deref,
    /// A field (e.g., f in _1.f) is one variant of ProjectionElem.
    /// Conceptually, rustc can identify that a field projection refers
    /// to either two different regions of memory or the same one between
    /// the base and the ‘projection element’. Read more about projections
    /// in the rustc-dev-guide
    Field(FieldIdx, T),

    /// Index into a slice/array.
    ///
    /// Note that this does not also dereference, and so it does not exactly
    /// correspond to slice indexing in Rust.
    Index(V),

    /// These indices are generated by slice patterns.
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },

    /// These indices are generated by slice patterns.
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },

    /// “Downcast” to a variant of an enum or a generator.
    ///
    /// The included Symbol is the name of the variant, used for printing MIR.
    // Downcast(Option<Symbol>, VariantIdx),

    /// Like an explicit cast from an opaque type to a concrete type,
    /// but without requiring an intermediate variable.
    OpaqueCast(T),
}

#[derive(Debug)]
pub struct Local {
    pub(crate) mutability: Mutability,
    pub(crate) ty: Idx<Type>,
}

impl Local {
    pub fn ty_idx(&self) -> Idx<Type> {
        self.ty
    }

    pub fn type_<'a>(&self, context: &'a hir::Context) -> &'a Type {
        context.type_(self.ty)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mutability {
    Not,
    Mut,
}

#[derive(Debug)]
struct FieldIdx {
    private: u32,
}

#[derive(Debug)]
pub enum Rvalue {
    /// Yields the operand unchanged
    Use(Operand),

    // Creates an array where each element is the value of the operand.
    // in rustc Const is struct { ty: Ty, kind: ConstKind }
    // we could do IntLiteral instead or something
    // Repeat(Operand, Const<'tcx>),

    // Creates a reference of the indicated kind to the place.
    // Ref(Region<'tcx>, BorrowKind, Place<'tcx>),

    // Creates a pointer with the indicated mutability to the place.
    // AddressOf(Mutability, Place<'tcx>),

    // TODO: what casts should we insert for the user?
    // Cast(CastKind, Operand<'tcx>, Ty<'tcx>),

    // -------------
    /// Binary operation
    BinaryOp(BinOp, Box<(Operand, Operand)>),

    // Same as `BinaryOp`, but yields `(T, bool)` with a `bool` indicating an error condition.
    // CheckedBinaryOp(BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>),
    // -------------
    /// Exactly like BinaryOp, but less operands.
    ///
    /// Also does two’s-complement arithmetic.
    /// Negation requires a signed integer or a float; bitwise not requires
    /// a signed integer, unsigned integer, or bool.
    /// Both operation kinds return a value with the same type as their operand.
    UnaryOp(UnOp, Operand),
    // Computes a value as described by the operation.
    // examples : sizeof, alignof, typeof
    // NullaryOp(NullOp<'tcx>, Ty<'tcx>),
    // -------------------------
    // / Computes the discriminant of the place, returning it as an integer
    // / of type discriminant_ty.
    // / Returns zero for types without discriminant
    // Discriminant(Place),
    // -------------------------
    // / Yields the length of the value at the given place
    // /
    // / This would generally be the length of an array, and
    // / the byte length of a string.
    // Len(Place),
}

/// Intrinsic operation that does not diverge
///
/// For example, integer addition with overflow is non-diverging, but
/// panic on overflow is diverging. TODO: determine integer overflow behavior
#[derive(Debug)]
enum NonDivergingIntrinsic {
    IntAddition,
}

#[derive(Debug)]
pub enum BinOp {
    /// Addition `+`
    Add,

    /// Subtraction `-`
    Sub,

    /// Multiplication `*`
    Mul,

    /// Division `/`
    Div,

    /// Remainder `%`
    Rem,

    // bitwise could be added here
    /// Equality `==`
    Eq,

    /// Not equal `!=`
    Ne,

    /// Less than or equal `<=`
    Le,

    /// Less than `<`
    Lt,

    /// Greater than or equal `>=`
    Ge,

    /// Greater than `>`
    Gt,
}

#[derive(Debug)]
pub enum UnOp {
    /// Not: Logical inversion `!`
    Not,

    /// Negation `-`
    Neg,
}

#[derive(Debug)]
pub enum Operand {
    /// Copies the value from the given Place
    Copy(Place),

    /// Constant value, so does not need to be copied
    Constant(Constant),

    /// Moves ownership of the resource from the given Place
    ///
    /// This applies only to `resource` types which are not yet
    /// implemented. It should be an error to reference this place
    /// again after the resource has been moved.
    Move(Place),
}

impl Operand {
    pub fn type_of(&self, context: &hir::Context) -> Idx<Type> {
        let core = context.core_types();
        match self {
            Operand::Copy(place) | Operand::Move(place) => todo!(),
            Operand::Constant(constant) => match constant {
                Constant::Int(_) => core.int,
                Constant::Float(_) => core.float,
                Constant::StringLiteral(_) => core.string,
            },
        }
    }
}

#[derive(Debug)]
pub enum Constant {
    /// Integer constants.
    ///
    /// At this point, boolean constants are converted to integers
    Int(i64),

    /// Floating point constants.
    Float(f64),

    /// String constants
    StringLiteral(Key),
}
