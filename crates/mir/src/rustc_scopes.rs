// copied from rustc to study & learn from

#[derive(Debug)]
pub struct Scopes<'tcx> {
    scopes: Vec<Scope>,

    /// The current set of breakable scopes. See module comment for more details.
    breakable_scopes: Vec<BreakableScope<'tcx>>,

    /// The scope of the innermost if-then currently being lowered.
    if_then_scope: Option<IfThenScope>,

    /// Drops that need to be done on unwind paths. See the comment on
    /// [DropTree] for more details.
    unwind_drops: DropTree,
}

#[derive(Debug)]
struct Scope {
    /// The source scope this scope was created in.
    source_scope: SourceScope,

    /// the region span of this scope within source code.
    region_scope: region::Scope,

    /// set of places to drop when exiting this scope. This starts
    /// out empty but grows as variables are declared during the
    /// building process. This is a stack, so we always drop from the
    /// end of the vector (top of the stack) first.
    drops: Vec<DropData>,

    moved_locals: Vec<Local>,

    /// The drop index that will drop everything in and below this scope on an
    /// unwind path.
    cached_unwind_block: Option<DropIdx>,
}
#[derive(Clone, Copy, Debug)]
struct DropData {
    /// The `Span` where drop obligation was incurred (typically where place was
    /// declared)
    source_info: SourceInfo,

    /// local to drop
    local: Local,

    /// Whether this is a value Drop or a StorageDead.
    kind: DropKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DropKind {
    Value,
    Storage,
}

#[derive(Debug)]
struct BreakableScope<'tcx> {
    /// Region scope of the loop
    region_scope: region::Scope,
    /// The destination of the loop/block expression itself (i.e., where to put
    /// the result of a `break` or `return` expression)
    break_destination: Place<'tcx>,
    /// Drops that happen on the `break`/`return` path.
    break_drops: DropTree,
    /// Drops that happen on the `continue` path.
    continue_drops: Option<DropTree>,
}

#[derive(Debug)]
struct IfThenScope {
    /// The if-then scope or arm scope
    region_scope: region::Scope,
    /// Drops that happen on the `else` path.
    else_drops: DropTree,
}

/// The target of an expression that breaks out of a scope
#[derive(Clone, Copy, Debug)]
pub(crate) enum BreakableTarget {
    Continue(region::Scope),
    Break(region::Scope),
    Return,
}
#[derive(Clone, Copy, Debug)]
struct DropData {
    /// The `Span` where drop obligation was incurred (typically where place was
    /// declared)
    source_info: SourceInfo,

    /// local to drop
    local: Local,

    /// Whether this is a value Drop or a StorageDead.
    kind: DropKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DropKind {
    Value,
    Storage,
}

#[derive(Debug)]
struct BreakableScope<'tcx> {
    /// Region scope of the loop
    region_scope: region::Scope,
    /// The destination of the loop/block expression itself (i.e., where to put
    /// the result of a `break` or `return` expression)
    break_destination: Place<'tcx>,
    /// Drops that happen on the `break`/`return` path.
    break_drops: DropTree,
    /// Drops that happen on the `continue` path.
    continue_drops: Option<DropTree>,
}

#[derive(Debug)]
struct IfThenScope {
    /// The if-then scope or arm scope
    region_scope: region::Scope,
    /// Drops that happen on the `else` path.
    else_drops: DropTree,
}

/// The target of an expression that breaks out of a scope
#[derive(Clone, Copy, Debug)]
pub(crate) enum BreakableTarget {
    Continue(region::Scope),
    Break(region::Scope),
    Return,
}

/// A tree of drops that we have deferred lowering. It's used for:
///
/// * Drops on unwind paths
/// * Drops on coroutine drop paths (when a suspended coroutine is dropped)
/// * Drops on return and loop exit paths
/// * Drops on the else path in an `if let` chain
///
/// Once no more nodes could be added to the tree, we lower it to MIR in one go
/// in `build_mir`.
#[derive(Debug)]
struct DropTree {
    /// Drops in the tree.
    drops: IndexVec<DropIdx, (DropData, DropIdx)>,
    /// Map for finding the inverse of the `next_drop` relation:
    ///
    /// `previous_drops[(drops[i].1, drops[i].0.local, drops[i].0.kind)] == i`
    previous_drops: FxHashMap<(DropIdx, Local, DropKind), DropIdx>,
    /// Edges into the `DropTree` that need to be added once it's lowered.
    entry_points: Vec<(DropIdx, BasicBlock)>,
}

mod region {
    #[derive(
        Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Copy, TyEncodable, TyDecodable, HashStable,
    )]
    pub struct Scope {
        pub id: hir::ItemLocalId,
        pub data: ScopeData,
    }
    #[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy)]
    pub enum ScopeData {
        Node,

        /// Scope of the call-site for a function or closure
        /// (outlives the arguments as well as the body).
        CallSite,

        /// Scope of arguments passed to a function or closure
        /// (they outlive its body).
        Arguments,

        /// Scope of destructors for temporaries of node-id.
        Destruction,

        /// Scope of the condition and then block of an if expression
        /// Used for variables introduced in an if-let expression.
        IfThen,

        /// Scope following a `let id = expr;` binding in a block.
        Remainder(FirstStatementIndex),
    }

    #[derive(Default, Debug, HashStable)]
    pub struct ScopeTree {
        /// If not empty, this body is the root of this region hierarchy.
        pub root_body: Option<hir::HirId>,

        /// Maps from a scope ID to the enclosing scope id;
        /// this is usually corresponding to the lexical nesting, though
        /// in the case of closures the parent scope is the innermost
        /// conditional expression or repeating block. (Note that the
        /// enclosing scope ID for the block associated with a closure is
        /// the closure itself.)
        pub parent_map: FxIndexMap<Scope, (Scope, ScopeDepth)>,

        /// Maps from a variable or binding ID to the block in which that
        /// variable is declared.
        var_map: FxIndexMap<hir::ItemLocalId, Scope>,

        /// Identifies expressions which, if captured into a temporary, ought to
        /// have a temporary whose lifetime extends to the end of the enclosing *block*,
        /// and not the enclosing *statement*. Expressions that are not present in this
        /// table are not rvalue candidates. The set of rvalue candidates is computed
        /// during type check based on a traversal of the AST.
        pub rvalue_candidates: HirIdMap<RvalueCandidateType>,

        /// If there are any `yield` nested within a scope, this map
        /// stores the `Span` of the last one and its index in the
        /// postorder of the Visitor traversal on the HIR.
        ///
        /// HIR Visitor postorder indexes might seem like a peculiar
        /// thing to care about. but it turns out that HIR bindings
        /// and the temporary results of HIR expressions are never
        /// storage-live at the end of HIR nodes with postorder indexes
        /// lower than theirs, and therefore don't need to be suspended
        /// at yield-points at these indexes.
        ///
        /// For an example, suppose we have some code such as:
        /// ```rust,ignore (example)
        ///     foo(f(), yield y, bar(g()))
        /// ```
        ///
        /// With the HIR tree (calls numbered for expository purposes)
        ///
        /// ```text
        ///     Call#0(foo, [Call#1(f), Yield(y), Call#2(bar, Call#3(g))])
        /// ```
        ///
        /// Obviously, the result of `f()` was created before the yield
        /// (and therefore needs to be kept valid over the yield) while
        /// the result of `g()` occurs after the yield (and therefore
        /// doesn't). If we want to infer that, we can look at the
        /// postorder traversal:
        /// ```plain,ignore
        ///     `foo` `f` Call#1 `y` Yield `bar` `g` Call#3 Call#2 Call#0
        /// ```
        ///
        /// In which we can easily see that `Call#1` occurs before the yield,
        /// and `Call#3` after it.
        ///
        /// To see that this method works, consider:
        ///
        /// Let `D` be our binding/temporary and `U` be our other HIR node, with
        /// `HIR-postorder(U) < HIR-postorder(D)`. Suppose, as in our example,
        /// U is the yield and D is one of the calls.
        /// Let's show that `D` is storage-dead at `U`.
        ///
        /// Remember that storage-live/storage-dead refers to the state of
        /// the *storage*, and does not consider moves/drop flags.
        ///
        /// Then:
        ///
        ///   1. From the ordering guarantee of HIR visitors (see
        ///   `rustc_hir::intravisit`), `D` does not dominate `U`.
        ///
        ///   2. Therefore, `D` is *potentially* storage-dead at `U` (because
        ///   we might visit `U` without ever getting to `D`).
        ///
        ///   3. However, we guarantee that at each HIR point, each
        ///   binding/temporary is always either always storage-live
        ///   or always storage-dead. This is what is being guaranteed
        ///   by `terminating_scopes` including all blocks where the
        ///   count of executions is not guaranteed.
        ///
        ///   4. By `2.` and `3.`, `D` is *statically* storage-dead at `U`,
        ///   QED.
        ///
        /// This property ought to not on (3) in an essential way -- it
        /// is probably still correct even if we have "unrestricted" terminating
        /// scopes. However, why use the complicated proof when a simple one
        /// works?
        ///
        /// A subtle thing: `box` expressions, such as `box (&x, yield 2, &y)`. It
        /// might seem that a `box` expression creates a `Box<T>` temporary
        /// when it *starts* executing, at `HIR-preorder(BOX-EXPR)`. That might
        /// be true in the MIR desugaring, but it is not important in the semantics.
        ///
        /// The reason is that semantically, until the `box` expression returns,
        /// the values are still owned by their containing expressions. So
        /// we'll see that `&x`.
        pub yield_in_scope: UnordMap<Scope, Vec<YieldData>>,
    }
}
