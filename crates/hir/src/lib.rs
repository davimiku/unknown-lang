mod builtins;
mod database;
mod diagnostic;
mod display;
mod expr;
mod interner;
mod intrinsics;
mod lowering_context;
mod scope;
mod type_expr;
mod typecheck;

#[cfg(test)]
mod tests;

pub use ast::Mutability;
use ast::Root;
pub use builtins::BuiltinId;
pub use diagnostic::Diagnostic;
pub use display::{display_root, ContextDisplay};
pub use expr::{
    BinaryOp, BlockExpr, CallExpr, Expr, FunctionExpr, FunctionExprGroup, FunctionParam, IfExpr,
    IndexIntExpr, IntrinsicExpr, LoopExpr, MatchExpr, PathExpr, Pattern, ReAssignment, UnaryExpr,
    UnaryOp, ValueSymbol, VarDefExpr, VarRefExpr,
};
use itertools::Itertools;
pub use lowering_context::{Context, COMPILER_BRAND};
pub use typecheck::{
    CoreTypes, FuncSignature, FunctionType, ListType, ProductType, SumType, Type, VariantIdx,
    VecVariantIdx,
};

use database::Database;

pub use interner::{Interner, Key};
use la_arena::Idx;
use scope::ModuleScopes;
use std::path::{Path, PathBuf};
use type_expr::TypeExpr;

/// The source/context how a module was defined
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    /// Userland code - the current project or another library it imports
    User,
    /// Standard library (stdlib) - similar to userland but has extra privileged capabilities
    StandardLibrary,
    /// Functionality builtin/hardcoded into the compiler itself
    CompilerBuiltin,
}

pub struct Module {
    pub id: u32,

    pub source_kind: SourceKind,

    pub exprs: Box<[Idx<Expr>]>,
}

pub fn lower(input: &str) -> (Module, Context) {
    let mut context = Context::new(Interner::default());
    let module = context.load_module(input, SourceKind::User);

    (module, context)
}

impl Context {
    /// Loads and type-checks one source file in the current compiler context.
    ///
    /// Module hierarchy and imports are deliberately not part of this API yet.
    /// Each loaded source receives a distinct module id so introducing those
    /// concepts later does not require changing the basic loading boundary.
    pub fn load_module(&mut self, input: &str, source_kind: SourceKind) -> Module {
        if self.loaded_module_count > 0 {
            self.current_module_id += 1;
            self.module_scopes
                .push(ModuleScopes::new(self.current_module_id));
        }

        let ast = parse(input);
        let module_id = self.current_module_id;

        let exprs: Vec<Idx<Expr>> = ast
            .exprs()
            .map(|expr| self.lower_expr_statement(Some(expr)))
            .collect();

        let module = Module {
            exprs: exprs.into(),
            id: module_id,
            source_kind,
        };

        if source_kind == SourceKind::StandardLibrary {
            let exports = self
                .database
                .value_builtins
                .iter()
                .filter(|(symbol, _)| {
                    let (symbol_module, _) = (**symbol).into();
                    symbol_module == module_id
                })
                .filter_map(|(symbol, _)| {
                    self.database
                        .value_names
                        .get(symbol)
                        .map(|key| (*key, *symbol))
                })
                .collect_vec();
            self.stdlib_builtins.extend(exports);
        }

        self.type_check_module(&module);
        self.loaded_module_count += 1;

        module
    }

    /// Loads all files in the repository standard-library tree.
    ///
    /// The prelude is loaded first so it can serve as the root declarations,
    /// otherwise modules are just loaded in traversal order (no guaranteed order)
    pub fn load_stdlib(&mut self) -> std::io::Result<Vec<Module>> {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib");
        let mut paths = Vec::new();
        collect_hopper_files(&root, &mut paths)?;
        paths.sort();
        paths.sort_by_key(|path| !is_prelude(path));

        paths
            .into_iter()
            .map(|path| self.load_module_file(&path, SourceKind::StandardLibrary))
            .collect()
    }

    /// Load a source file through the normal module loader
    pub fn load_module_file(
        &mut self,
        path: &Path,
        source_kind: SourceKind,
    ) -> std::io::Result<Module> {
        let input = std::fs::read_to_string(path)?;
        Ok(self.load_module(&input, source_kind))
    }
}

fn collect_hopper_files(root: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(root)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_hopper_files(&path, files)?;
        } else if path.extension().is_some_and(|extension| extension == "hop") {
            files.push(path);
        }
    }
    Ok(())
}

fn is_prelude(path: &Path) -> bool {
    path.file_name().is_some_and(|name| name == "prelude.hop")
}

/// In "function mode", the only top-level expression is a function expression
pub fn lower_function(input: &str) -> (Idx<Expr>, Context) {
    let (ast, mut context) = parse_with_context(input);

    let ast_function = ast
        .exprs()
        .next()
        .expect("the first expression to be the function to lower");

    let function_idx = context.lower_expr(Some(ast_function));
    let function = context.expr(function_idx);

    assert!(matches!(function, Expr::Function(_)));

    context.type_check(function_idx, context.core_types().top);

    (function_idx, context)
}

/// In "script mode", wrap the list of expressions in the script into a synthetic
/// "main" function
pub fn lower_script(input: &str) -> (Idx<Expr>, Context) {
    let (ast, mut context) = parse_with_context(input);

    let exprs: Vec<Idx<Expr>> = ast
        .exprs()
        .map(|expr| context.lower_expr_statement(Some(expr)))
        .collect();

    // FIXME: this doesn't seem to handle an empty script
    let body = Expr::Block(BlockExpr::NonEmpty { exprs });
    let body = context.alloc_expr(body, None);
    let main_symbol = ValueSymbol::synthetic_main();
    let main_name = context.interner.intern("main");

    let params = Box::new([]);
    let return_type_annotation = None;
    let captures = Box::new([]);
    let function = Expr::Function(FunctionExprGroup {
        // FIXME: `args: Array String` for CLI scripts?
        overloads: Box::new([FunctionExpr {
            params,
            body,
            return_type_annotation,
            captures,
        }]),
        name: Some((main_name, main_symbol)),
        entry_point: Some(main_name),
    });
    let function_idx = context.alloc_expr(function, None);

    context.type_check(function_idx, context.core_types().top);

    (function_idx, context)
}

fn parse(input: &str) -> Root {
    let parsed = parser::parse(input);
    assert_eq!(parsed.errors(), &[]);

    ast::Root::cast(parsed.syntax()).expect("valid Root node")
}

fn parse_with_context(input: &str) -> (Root, Context) {
    let root = parse(input);

    let interner = Interner::default();
    let context = Context::new(interner);

    (root, context)
}
