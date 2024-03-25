use std::fmt::{self, Display};
use std::slice::Iter;

use hir::{Context, Expr, IntrinsicExpr, Key, Mutability, Type, ValueSymbol};
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

/// Mid-Level Representation (MIR) of a Function
///
/// This represents the data for a function, primarily organized as
/// a group of Basic Blocks that represent the flow of control through
/// the function.
#[derive(Debug, Clone)]
pub struct Function {
    /// String name of the function
    ///
    /// Anonymous functions are None.
    /// An overloaded function would have a different name for each overload
    // TODO: would be nice for this to be a Key to reduce the struct size,
    // but this name is being generated in the MIR currently and we don't
    // have mutable access to the hir::Context::Interner at this point.
    // Could intern all the strings in the HIR in advance which works for
    // static overloads but not for generic functions that will be monomorphized
    // here. Or, could make a new Interner for the MIR
    pub name: Option<String>,

    pub id: FuncId,

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

impl Function {
    pub(crate) fn new(id: impl Into<FuncId>) -> Self {
        let id = id.into();
        let mut blocks = Arena::new();

        blocks.alloc(BasicBlock::default());

        Self {
            id,
            blocks,
            name: None,
            params: Vec::default(),
            locals: Arena::default(),
            locals_map: ArenaMap::default(),
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
            projection: Box::new([]),
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
        return_local.type_idx_of()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId {
    module_id: u32,

    func_id: u32,
}

impl Display for FuncId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("\\f:{}:{}", self.module_id, self.func_id))
    }
}

impl From<(u32, u32)> for FuncId {
    fn from((module_id, func_id): (u32, u32)) -> Self {
        Self { module_id, func_id }
    }
}

impl From<FuncId> for (u32, u32) {
    fn from(value: FuncId) -> Self {
        (value.module_id, value.func_id)
    }
}

#[derive(Debug, Default, Clone)]
pub struct BasicBlock {
    /// Executable code of the basic block
    pub statements: Vec<Statement>,

    /// How the basic block ends
    pub terminator: Option<Terminator>,

    /// Locals used in this block that were defined in a previous block
    ///
    /// CLIF codegen requires these parameters to be explicitly passed to
    /// jump instructions to the next block. Currently these are being
    /// captured here in the MIR for easier translation to CLIF.
    pub parameters: BlockParameters,
}

// TODO: use a HashSet instead? uniqueness is required
// should benchmark whether O(N) search in the Vec is actually
// worse than the cost of hashing
#[derive(Debug, Default, Clone)]
pub struct BlockParameters(Vec<Idx<Local>>);

impl BlockParameters {
    /// Adds this local to the block parameters if it isn't already in
    /// the parameters (O(N) search).
    ///
    /// If the local was added, returns Some(local), otherwise if it already
    /// was in the parameters it returns None.
    pub fn push(&mut self, local: Idx<Local>) -> Option<Idx<Local>> {
        if self.0.contains(&local) {
            None
        } else {
            self.0.push(local);
            Some(local)
        }
    }

    // TODO: use newtype_derive or something else to not do this so hacky
    pub fn iter(&self) -> Iter<'_, Idx<Local>> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
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

impl Statement {
    pub(crate) fn assign(place: impl Into<Place>, rvalue: impl Into<Rvalue>) -> Self {
        Self::Assign(Box::new((place.into(), rvalue.into())))
    }
}

/// Source-order index of a variant in a union type
#[derive(Debug, Clone)]
pub struct VariantIdx {
    private: u32,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    /// Continues execution in the next block.
    /// This terminator has a single successor.
    Jump {
        target: Idx<BasicBlock>,
    },

    /// Chooses between multiple branches to determine the next block.
    ///
    /// Tests the discriminant for an integer value and picks the corresponding
    /// target. Boolean checks are included here, which are represented by
    /// integer values.
    SwitchInt {
        discriminant: Operand,
        targets: SwitchIntTargets,
    },

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

        /// Where to go after this call returns.
        ///
        /// None indicates a diverging call, such as `panic` or `abort`
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

    Unreachable,
}

impl Terminator {
    pub const fn name(&self) -> &'static str {
        match self {
            Terminator::Jump { .. } => "Goto",
            Terminator::SwitchInt { .. } => "SwitchInt",
            Terminator::Return => "Return",
            Terminator::Call { .. } => "Call",
            Terminator::Drop { .. } => "Drop",
            Terminator::Unreachable => "Unreachable",
        }
    }
}

/// Targets / branches for a SwitchInt terminator
///
/// The branches are organized into pairs of Int + Block to
/// switch to.
/// The `otherwise` branch is `None` when the `branches` are already
/// an exhaustive match.
// TODO: use a SmallVec to stack allocate branches with 1 item
#[derive(Debug, Default, Clone)]
pub struct SwitchIntTargets {
    pub branches: Box<[(i64, Idx<BasicBlock>)]>,

    pub otherwise: Option<Idx<BasicBlock>>,
}

#[derive(Debug, Clone)]
pub struct Place {
    pub local: Idx<Local>,
    pub projection: Box<[PlaceElem]>,
}

impl From<Idx<Local>> for Place {
    fn from(local: Idx<Local>) -> Self {
        Self {
            local,
            projection: Box::new([]),
        }
    }
}

impl Place {
    pub fn type_idx_of(&self, func: &Function, context: &hir::Context) -> Idx<Type> {
        // TODO: include projections
        let local = &func.locals[self.local];
        local.type_idx_of()
    }
}

type PlaceElem = ProjectionElem<Idx<Local>, hir::Type>;

// struct Ty<'tcx>(Interned<'tcx, WithCachedTypeInfo<TyKind<'tcx>>>);

// struct Interned<'a, T>(pub &'a T, pub PrivateZst);

/// Projections, which are fields or other things that "project out" from
/// a base place. These are represented by the newtype'd type ProjectionElem.
/// So e.g. the place _1.f is a projection, with f being the "projection element"
/// and _1 being the base path. *_1 is also a projection, with the * being
/// represented by the ProjectionElem::Deref element.
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy)]
pub struct Local {
    pub(crate) mutability: Mutability,
    pub(crate) ty: Idx<Type>,
}

impl Local {
    pub fn type_idx_of(&self) -> Idx<Type> {
        self.ty
    }

    pub fn type_<'a>(&self, context: &'a hir::Context) -> &'a Type {
        context.type_(self.ty)
    }
}

#[derive(Debug, Clone, Copy)]
struct FieldIdx {
    private: u32,
}

#[derive(Debug, Clone)]
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
    BinaryOp(BinOpKind, Box<(Operand, Operand)>),

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

impl From<Operand> for Rvalue {
    fn from(operand: Operand) -> Self {
        Self::Use(operand)
    }
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
pub(crate) struct BinOp {
    pub(crate) lhs: Idx<Expr>,
    pub(crate) rhs: Idx<Expr>,
    pub(crate) kind: BinOpKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    /// Addition `+`
    Add,

    /// Subtraction `-`
    Sub,

    /// Multiplication `*`
    Mul,

    /// Division `/`
    Div,

    /// Concatenation `++`
    Concat,

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

impl From<&IntrinsicExpr> for BinOpKind {
    fn from(value: &IntrinsicExpr) -> Self {
        (*value).into()
    }
}

impl From<IntrinsicExpr> for BinOpKind {
    fn from(value: IntrinsicExpr) -> Self {
        match value {
            IntrinsicExpr::Add => BinOpKind::Add,
            IntrinsicExpr::Sub => BinOpKind::Sub,
            IntrinsicExpr::Mul => BinOpKind::Mul,
            IntrinsicExpr::Div => BinOpKind::Div,
            IntrinsicExpr::Concat => BinOpKind::Concat,
            IntrinsicExpr::Rem => BinOpKind::Rem,
            IntrinsicExpr::Eq => BinOpKind::Eq,
            IntrinsicExpr::Ne => BinOpKind::Ne,
            IntrinsicExpr::Lt => BinOpKind::Lt,
            IntrinsicExpr::Le => BinOpKind::Le,
            IntrinsicExpr::Gt => BinOpKind::Gt,
            IntrinsicExpr::Ge => BinOpKind::Ge,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    /// Not: Logical inversion `!`
    Not,

    /// Negation `-`
    Neg,
}

#[derive(Debug, Clone)]
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
    pub fn from_op_or_place(op_or_place: OperandOrPlace, context: &Context) -> Self {
        match op_or_place {
            OperandOrPlace::Operand(operand) => operand,
            OperandOrPlace::Place(place) => Self::from_place(place, context),
        }
    }

    pub fn from_place(place: Place, context: &Context) -> Self {
        // TODO: use Context to get type information to determine Copy/Move/Share
        Self::Copy(place)
    }

    pub fn as_local(&self) -> Option<Idx<Local>> {
        match self {
            Operand::Copy(place) | Operand::Move(place) => Some(place.local),
            Operand::Constant(_) => None,
        }
    }
}

impl Operand {
    pub fn as_constant(&self) -> Option<&Constant> {
        match self {
            Operand::Constant(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<&i64> {
        self.as_constant().and_then(|constant| match constant {
            Constant::Int(i) => Some(i),
            _ => None,
        })
    }
}

impl From<Constant> for Operand {
    fn from(constant: Constant) -> Self {
        Self::Constant(constant)
    }
}

pub enum OperandOrPlace {
    Operand(Operand),

    Place(Place),
}

impl OperandOrPlace {
    pub fn as_place(self) -> Option<Place> {
        match self {
            OperandOrPlace::Operand(operand) => match operand {
                Operand::Copy(place) => Some(place),
                Operand::Constant(_) => None,
                Operand::Move(place) => Some(place),
            },
            OperandOrPlace::Place(place) => Some(place),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    /// Integer constants.
    ///
    /// Boolean constants are 0 (false) or 1 (true)
    Int(i64),

    /// Floating point constants.
    Float(f64),

    /// String constants
    String(Key),

    /// A function expression
    ///
    /// ex. `fun (i: Int) -> { i + 2 }`
    Func(FuncId),
}

impl From<bool> for Constant {
    fn from(b: bool) -> Self {
        Self::Int(b as i64)
    }
}

impl From<i64> for Constant {
    fn from(i: i64) -> Self {
        Self::Int(i)
    }
}

impl From<f64> for Constant {
    fn from(f: f64) -> Self {
        Self::Float(f)
    }
}

impl From<Key> for Constant {
    fn from(key: Key) -> Self {
        Self::String(key)
    }
}

impl From<FuncId> for Constant {
    fn from(id: FuncId) -> Self {
        Self::Func(id)
    }
}

pub(crate) enum Callee {
    Direct(FuncId),
    Indirect(Idx<Local>),
}

impl From<&FuncId> for Callee {
    fn from(func_id: &FuncId) -> Self {
        Self::Direct(*func_id)
    }
}

impl From<FuncId> for Callee {
    fn from(func_id: FuncId) -> Self {
        Self::Direct(func_id)
    }
}
