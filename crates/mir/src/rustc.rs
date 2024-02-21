/*
*this is not included in the module tree*

These are example data structures copy/pasted from rustc to use as a reference/
to learn from.

Basic Blocks:
    - Statements: an action with only one successor
    - Terminators: actions with potentially multiple successors,
                   always at the end of block

Body:
    - Blocks: list/vec of Basic Blocks


*/
/// Basic Block is a set of statements with a terminator
///
struct BasicBlock;

struct BasicBlockData<'tcx> {
    statements: Vec<Statement<'tcx>>,
    terminator: Option<Terminator<'tcx>>,
    // Used for unwinding in the case of a panic
    // pub is_cleanup: bool,
}

struct SourceInfo {
    span: Span,
    scope: SourceScope,
}

struct SourceScope {
    private: u32,
}

struct Span {
    base_or_index: u32,
    len_or_tag: u16,
    ctxt_or_tag: u16,
}

struct Statement<'tcx> {
    source_info: SourceInfo,
    kind: StatementKind<'tcx>,
}

pub enum StatementKind<'tcx> {
    /// Assign statements roughly correspond to an assignment in Rust proper (x = ...)
    /// except without the possibility of dropping the previous value
    /// (that must be done separately, if at all).
    ///
    /// It probably does something like evaluating the LHS to a place and
    /// the RHS to a value, and then storing the value to the place.
    /// Various parts of this may do type specific things that are more
    /// complicated than simply copying bytes.
    Assign(Box<(Place<'tcx>, Rvalue<'tcx>)>),

    /// This represents all the reading that a pattern match may do
    /// (e.g., inspecting constants and discriminant values), and the
    /// kind of pattern it comes from. This is in order to adapt potential error
    /// messages to these specific patterns.
    ///
    /// When executed at runtime this is a nop.
    FakeRead(Box<(FakeReadCause, Place<'tcx>)>),

    /// Write the discriminant for a variant to the enum Place.
    ///
    /// This is permitted for both generators and ADTs. This does not necessarily
    /// write to the entire place; instead, it writes to the minimum set of bytes as
    /// required by the layout for the type.
    SetDiscriminant {
        place: Box<Place<'tcx>>,
        variant_index: VariantIdx,
    },

    /// Deinitializes the place.
    ///
    /// This writes uninit bytes to the entire place.
    Deinit(Box<Place<'tcx>>),

    /// StorageLive and StorageDead statements mark the live range of a local.
    ///
    /// At any point during the execution of a function, each local is either
    /// allocated or unallocated. Except as noted below, all locals except function
    /// parameters are initially unallocated. StorageLive statements cause memory
    /// to be allocated for the local while StorageDead statements cause the memory
    /// to be freed. Using a local in any way (not only reading/writing from it)
    /// while it is unallocated is UB.
    StorageLive(Local),
    StorageDead(Local),

    /// Retag references in the given place, ensuring they got fresh tags.
    ///
    /// This is part of the Stacked Borrows model. These statements are
    /// currently only interpreted by miri and only generated when -Z mir-emit-retag
    /// is passed.
    /// See https://internals.rust-lang.org/t/stacked-borrows-an-aliasing-model-for-rust/8153/ for more details.
    Retag(RetagKind, Box<Place<'tcx>>),

    /// This statement exists to preserve a trace of a scrutinee matched against
    /// a wildcard binding. This is especially useful for let _ = PLACE;
    /// bindings that desugar to a single PlaceMention(PLACE).
    ///
    /// When executed at runtime, this computes the given place, but then discards
    /// it without doing a load. It is UB if the place is not pointing to live memory.
    PlaceMention(Box<Place<'tcx>>),

    /// Encodes a user’s type ascription. These need to be preserved intact so that
    /// NLL can respect them.
    ///
    /// The effect of this annotation is to relate the type T_y of the place y to
    /// the user-given type T.
    /// When executed at runtime this is a nop.
    AscribeUserType(Box<(Place<'tcx>, UserTypeProjection)>, Variance),

    /// Marks the start of a “coverage region”, injected with ‘-Cinstrument-coverage’.
    /// A Coverage statement carries metadata about the coverage region, used to
    /// inject a coverage map into the binary. If Coverage::kind is a Counter, the statement also generates executable code, to increment a counter variable at runtime, each time the code region is executed.
    Coverage(Box<Coverage>),

    /// Denotes a call to an intrinsic that does not require an unwind path
    /// and always returns. This avoids adding a new block and a terminator
    /// for simple intrinsics.
    Intrinsic(Box<NonDivergingIntrinsic<'tcx>>),

    /// Instructs the const eval interpreter to increment a counter;
    /// this counter is used to track how many steps the interpreter has taken.
    /// It is used to prevent the user from writing const code that runs for
    /// too long or infinitely. Other than in the const eval interpreter,
    /// this is a no-op.
    ConstEvalCounter,

    /// No-op. Useful for deleting instructions without affecting statement indices.
    Noop,
}

enum NonDivergingIntrinsic {
    /// Denotes a call to the intrinsic function `assume`.
    ///
    /// The operand must be a boolean. Optimizers may use the value of the boolean to backtrack its
    /// computation to infer information about other variables. So if the boolean came from a
    /// `x < y` operation, subsequent operations on `x` and `y` could elide various bound checks.
    /// If the argument is `false`, this operation is equivalent to `TerminatorKind::Unreachable`.
    Assume(Operand<'tcx>),

    /// Denotes a call to the intrinsic function `copy_nonoverlapping`.
    ///
    /// First, all three operands are evaluated. `src` and `dest` must each be a reference, pointer,
    /// or `Box` pointing to the same type `T`. `count` must evaluate to a `usize`. Then, `src` and
    /// `dest` are dereferenced, and `count * size_of::<T>()` bytes beginning with the first byte of
    /// the `src` place are copied to the contiguous range of bytes beginning with the first byte
    /// of `dest`.
    ///
    /// **Needs clarification**: In what order are operands computed and dereferenced? It should
    /// probably match the order for assignment, but that is also undecided.
    ///
    /// **Needs clarification**: Is this typed or not, ie is there a typed load and store involved?
    /// I vaguely remember Ralf saying somewhere that he thought it should not be.
    CopyNonOverlapping(CopyNonOverlapping<'tcx>),
}

struct CopyNonOverlapping<'tcx> {
    pub src: Operand<'tcx>,
    pub dst: Operand<'tcx>,
    pub count: Operand<'tcx>,
}

struct Terminator<'tcx> {
    source_info: SourceInfo,
    kind: TerminatorKind<'tcx>,
}

enum TerminatorKind<'tcx> {
    /// Block has one successor; we continue execution there.
    Goto {
        target: BasicBlock,
    },

    /// Switches based on the computed value.
    ///
    /// First, evaluates the discr operand.
    /// The type of the operand must be a signed or unsigned integer, char, or bool,
    /// and must match the given type. Then, if the list of switch targets contains
    /// the computed value, continues execution at the associated basic block.
    /// Otherwise, continues execution at the “otherwise” basic block.
    ///
    /// Target values may not appear more than once.
    SwitchInt {
        discr: Operand<'tcx>,
        targets: SwitchTargets,
    },
    UnwindResume,
    UnwindTerminate,

    /// Returns from the function.
    ///
    /// Like function calls, the exact semantics of returns in Rust are unclear.
    /// Returning very likely at least assigns the value currently in the return
    /// place (_0) to the place specified in the associated Call terminator in
    /// the calling function, as if assigned via dest = move _0. It might additionally
    /// do other things, like have side-effects in the aliasing model.
    ///
    /// If the body is a generator body, this has slightly different semantics;
    /// it instead causes a GeneratorState::Returned(_0) to be created (as if
    /// by an Aggregate rvalue) and assigned to the return place.
    Return,

    /// Indicates a terminator that can never be reached.
    /// Executing this terminator is UB.
    Unreachable,

    Drop {
        place: Place<'tcx>,
        target: BasicBlock,
        unwind: UnwindAction,
        replace: bool,
    },

    /// Roughly speaking, evaluates the func operand and the arguments,
    /// and starts execution of the referred to function. The operand types
    /// must match the argument types of the function. The return place type
    /// must match the return type. The type of the func operand must be callable,
    /// meaning either a function pointer, a function type, or a closure type.
    Call {
        func: Operand<'tcx>,
        args: Vec<Operand<'tcx>>,
        destination: Place<'tcx>,
        target: Option<BasicBlock>,
        unwind: UnwindAction,
        call_source: CallSource,
        fn_span: Span,
    },

    /// Evaluates the operand, which must have type bool. If it is not equal to
    /// expected, initiates a panic. Initiating a panic corresponds to a Call
    /// terminator with some unspecified constant as the function to call, all
    /// the operands stored in the AssertMessage as parameters, and None for the
    /// destination. Keep in mind that the cleanup path is not necessarily executed
    /// even in the case of a panic, for example in -C panic=abort. If the assertion
    /// does not fail, execution continues at the specified basic block.
    Assert {
        cond: Operand<'tcx>,
        expected: bool,
        msg: Box<AssertMessage<'tcx>>,
        target: BasicBlock,
        unwind: UnwindAction,
    },
    Yield {
        value: Operand<'tcx>,
        resume: BasicBlock,
        resume_arg: Place<'tcx>,
        drop: Option<BasicBlock>,
    },
    GeneratorDrop,
    FalseEdge {
        real_target: BasicBlock,
        imaginary_target: BasicBlock,
    },
    FalseUnwind {
        real_target: BasicBlock,
        unwind: UnwindAction,
    },
    InlineAsm {
        template: &'tcx [InlineAsmTemplatePiece],
        operands: Vec<InlineAsmOperand<'tcx>>,
        options: InlineAsmOptions,
        line_spans: &'tcx [Span],
        destination: Option<BasicBlock>,
        unwind: UnwindAction,
    },

    Noop,
}

struct Place<'tcx> {
    local: Local,
    projection: &'tcx List<PlaceElem<'tcx>>,
}

type PlaceElem<'tcx> = ProjectionElem<Local, Ty<'tcx>>;

/// Projections, which are fields or other things that "project out" from
/// a base place. These are represented by the newtype'd type ProjectionElem.
/// So e.g. the place _1.f is a projection, with f being the "projection element"
/// and _1 being the base path. *_1 is also a projection, with the * being
/// represented by the ProjectionElem::Deref element.
enum ProjectionElem<V, T> {
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
    Downcast(Option<Symbol>, VariantIdx),

    /// Like an explicit cast from an opaque type to a concrete type,
    /// but without requiring an intermediate variable.
    OpaqueCast(T),
}

enum Rvalue<'tcx> {
    /// Yields the operand unchanged
    Use(Operand<'tcx>),

    /// Creates an array where each element is the value of the operand.
    ///
    /// Corresponds to source code like [x; 32].
    Repeat(Operand<'tcx>, Const<'tcx>),

    /// Creates a reference of the indicated kind to the place.
    Ref(Region<'tcx>, BorrowKind, Place<'tcx>),

    /// Creates a pointer/reference to the given thread local.
    ///
    /// The yielded type is a *mut T if the static is mutable, otherwise
    /// if the static is extern a *const T, and if neither of those apply a &T.
    ///
    /// Note: This is a runtime operation that actually executes code and is
    /// in this sense more like a function call. Also, eliminating dead stores
    /// of this rvalue causes fn main() {} to SIGILL for some reason
    /// that I (JakobDegen) never got a chance to look into.
    ThreadLocalRef(DefId),

    /// Creates a pointer with the indicated mutability to the place.
    ///
    /// This is generated by pointer casts like &v as *const _ or
    /// raw address of expressions like &raw v or addr_of!(v).
    AddressOf(Mutability, Place<'tcx>),

    /// Yields the length of the place, as a usize.
    ///
    /// If the type of the place is an array, this is the array length.
    /// For slices ([T], not &[T]) this accesses the place’s metadata to
    /// determine the length. This rvalue is ill-formed for places of other types.
    Len(Place<'tcx>),

    /// Performs essentially all of the casts that can be performed via as.
    /// This allows for casts from/to a variety of types.
    Cast(CastKind, Operand<'tcx>, Ty<'tcx>),
    BinaryOp(BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>),

    /// Same as BinaryOp, but yields (T, bool) with a bool indicating an error condition.
    ///
    /// For addition, subtraction, and multiplication on integers the error condition
    /// is set when the infinite precision result would not be equal to the actual result.
    CheckedBinaryOp(BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>),
    NullaryOp(NullOp<'tcx>, Ty<'tcx>),

    /// Exactly like BinaryOp, but less operands.
    ///
    /// Also does two’s-complement arithmetic.
    /// Negation requires a signed integer or a float; bitwise not requires
    /// a signed integer, unsigned integer, or bool.
    /// Both operation kinds return a value with the same type as their operand.
    UnaryOp(UnOp, Operand<'tcx>),

    /// Computes the discriminant of the place, returning it as an integer
    /// of type discriminant_ty.
    /// Returns zero for types without discriminant
    Discriminant(Place<'tcx>),

    /// Creates an aggregate value, like a tuple or struct.
    ///
    /// This is needed because dataflow analysis needs to distinguish
    /// `dest = Foo { x: ..., y: ... }` from `dest.x = ...; dest.y = ...;`
    /// in the case that Foo has a destructor.
    Aggregate(Box<AggregateKind<'tcx>>, IndexVec<FieldIdx, Operand<'tcx>>),
    ShallowInitBox(Operand<'tcx>, Ty<'tcx>),
    CopyForDeref(Place<'tcx>),
}

enum BorrowKind {
    Shared,
    Shallow,
    Mut { kind: MutBorrowKind },
}

enum MutBorrowKind {
    Default,
    TwoPhaseBorrow,
    ClosureCapture,
}

enum Operand<'tcx> {
    /// Creates a value by loading the given place.
    ///
    ///Before drop elaboration, the type of the place must be Copy. After drop elaboration there is no such requirement.
    Copy(Place<'tcx>),

    /// Creates a value by performing loading the place, just like the Copy operand.
    Move(Place<'tcx>),

    /// Constants are already semantically values, and remain unchanged.
    Constant(Box<Constant<'tcx>>),
}

enum TyKind<'hir> {
    Slice(&'hir Ty<'hir>),
    Array(&'hir Ty<'hir>, ArrayLen),
    Ptr(MutTy<'hir>),
    Ref(&'hir Lifetime, MutTy<'hir>),
    BareFn(&'hir BareFnTy<'hir>),
    Never,
    Tup(&'hir [Ty<'hir>]),
    Path(QPath<'hir>),
    OpaqueDef(ItemId, &'hir [GenericArg<'hir>], bool),
    TraitObject(
        &'hir [PolyTraitRef<'hir>],
        &'hir Lifetime,
        TraitObjectSyntax,
    ),
    Typeof(AnonConst),
    Infer,
    Err(ErrorGuaranteed),
}

enum TyKind<I: Interner> {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Adt(I::AdtDef, I::GenericArgsRef),
    Foreign(I::DefId),
    Str,
    Array(I::Ty, I::Const),
    Slice(I::Ty),
    RawPtr(I::TypeAndMut),
    Ref(I::Region, I::Ty, I::Mutability),
    FnDef(I::DefId, I::GenericArgsRef),
    FnPtr(I::PolyFnSig),
    Dynamic(I::ListBinderExistentialPredicate, I::Region, DynKind),
    Closure(I::DefId, I::GenericArgsRef),
    Generator(I::DefId, I::GenericArgsRef, I::Movability),
    GeneratorWitness(I::BinderListTy),
    GeneratorWitnessMIR(I::DefId, I::GenericArgsRef),
    Never,
    Tuple(I::ListTy),
    Alias(AliasKind, I::AliasTy),
    Param(I::ParamTy),
    Bound(DebruijnIndex, I::BoundTy),
    Placeholder(I::PlaceholderType),
    Infer(I::InferTy),
    Error(I::ErrorGuaranteed),
}

/// The lowered representation of a single function.
#[derive(Clone, TyEncodable, TyDecodable, Debug, HashStable, TypeFoldable, TypeVisitable)]
pub struct Body<'tcx> {
    /// A list of basic blocks. References to basic block use a newtyped index type [`BasicBlock`]
    /// that indexes into this vector.
    pub basic_blocks: BasicBlocks<'tcx>,

    /// Records how far through the "desugaring and optimization" process this particular
    /// MIR has traversed. This is particularly useful when inlining, since in that context
    /// we instantiate the promoted constants and add them to our promoted vector -- but those
    /// promoted items have already been optimized, whereas ours have not. This field allows
    /// us to see the difference and forego optimization on the inlined promoted items.
    pub phase: MirPhase,

    /// How many passses we have executed since starting the current phase. Used for debug output.
    pub pass_count: usize,

    pub source: MirSource<'tcx>,

    /// A list of source scopes; these are referenced by statements
    /// and used for debuginfo. Indexed by a `SourceScope`.
    pub source_scopes: IndexVec<SourceScope, SourceScopeData<'tcx>>,

    pub coroutine: Option<Box<CoroutineInfo<'tcx>>>,

    /// Declarations of locals.
    ///
    /// The first local is the return value pointer, followed by `arg_count`
    /// locals for the function arguments, followed by any user-declared
    /// variables and temporaries.
    pub local_decls: IndexVec<Local, LocalDecl<'tcx>>,

    /// User type annotations.
    pub user_type_annotations: ty::CanonicalUserTypeAnnotations<'tcx>,

    /// The number of arguments this function takes.
    ///
    /// Starting at local 1, `arg_count` locals will be provided by the caller
    /// and can be assumed to be initialized.
    ///
    /// If this MIR was built for a constant, this will be 0.
    pub arg_count: usize,

    /// Mark an argument local (which must be a tuple) as getting passed as
    /// its individual components at the LLVM level.
    ///
    /// This is used for the "rust-call" ABI.
    pub spread_arg: Option<Local>,

    /// Debug information pertaining to user variables, including captures.
    pub var_debug_info: Vec<VarDebugInfo<'tcx>>,

    /// A span representing this MIR, for error reporting.
    pub span: Span,

    /// Constants that are required to evaluate successfully for this MIR to be well-formed.
    /// We hold in this field all the constants we are not able to evaluate yet.
    pub required_consts: Vec<ConstOperand<'tcx>>,

    /// Does this body use generic parameters. This is used for the `ConstEvaluatable` check.
    ///
    /// Note that this does not actually mean that this body is not computable right now.
    /// The repeat count in the following example is polymorphic, but can still be evaluated
    /// without knowing anything about the type parameter `T`.
    ///
    /// ```rust
    /// fn test<T>() {
    ///     let _ = [0; std::mem::size_of::<*mut T>()];
    /// }
    /// ```
    ///
    /// **WARNING**: Do not change this flags after the MIR was originally created, even if an optimization
    /// removed the last mention of all generic params. We do not want to rely on optimizations and
    /// potentially allow things like `[u8; std::mem::size_of::<T>() * 0]` due to this.
    pub is_polymorphic: bool,

    /// The phase at which this MIR should be "injected" into the compilation process.
    ///
    /// Everything that comes before this `MirPhase` should be skipped.
    ///
    /// This is only `Some` if the function that this body comes from was annotated with `rustc_custom_mir`.
    pub injection_phase: Option<MirPhase>,

    pub tainted_by_errors: Option<ErrorGuaranteed>,

    /// Per-function coverage information added by the `InstrumentCoverage`
    /// pass, to be used in conjunction with the coverage statements injected
    /// into this body's blocks.
    ///
    /// If `-Cinstrument-coverage` is not active, or if an individual function
    /// is not eligible for coverage, then this should always be `None`.
    pub function_coverage_info: Option<Box<coverage::FunctionCoverageInfo>>,
}

/*
The lowering of HIR to MIR occurs for the following (probably incomplete) list of items:

    Function and closure bodies
    Initializers of static and const items
    Initializers of enum discriminants
    Glue and shims of any kind
        Tuple struct initializer functions
        Drop code (the Drop::drop function is not called directly)
        Drop implementations of types without an explicit Drop implementation



*/
