mod builtins;
mod database;
mod diagnostic;
mod display;
mod expr;
mod interner;
mod intrinsics;
mod lowering_context;
mod module_tree;
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
pub use lowering_context::{Context, COMPILER_BRAND};
pub use module_tree::{ModuleExports, ModuleId, ModuleNode};
pub use type_expr::TypeSymbol;
pub use typecheck::{
    ArrayType, CoreTypes, FuncSignature, FunctionType, ProductType, SumType, Type, VariantIdx,
    VecVariantIdx,
};

use database::Database;
use diagnostic::LoweringDiagnostic;

pub use interner::{Interner, Key};
use la_arena::Idx;
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
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub name: Option<Key>,

    pub source_kind: SourceKind,

    pub exprs: Box<[Idx<Expr>]>,
}

pub fn lower(input: &str) -> (Module, Context) {
    let mut context = Context::new(Interner::default());
    let module = context.load_module(input, SourceKind::User);

    (module, context)
}

/// File name of the module that is the special "prelude" module
const PRELUDE: &'static str = "prelude";

impl Context {
    pub fn module(&self, id: ModuleId) -> Option<&ModuleNode> {
        self.module_tree.get(id)
    }

    /// Loads and type-checks one source file in the current compiler context.
    ///
    /// Module hierarchy and imports are deliberately not part of this API yet.
    /// Each loaded source receives a distinct module id so introducing those
    /// concepts later does not require changing the basic loading boundary.
    pub fn load_module(&mut self, input: &str, source_kind: SourceKind) -> Module {
        let (module_id, parent, name) = if self.loaded_module_count > 0 {
            let module_id = self.module_tree.allocate(None, None, source_kind);
            (module_id, None, None)
        } else {
            (self.current_module_id, None, None)
        };

        self.load_module_at(input, source_kind, module_id, parent, name)
    }

    /// Loads a named module as a child of an existing module.
    pub fn load_module_in(
        &mut self,
        parent: ModuleId,
        name: Key,
        input: &str,
        source_kind: SourceKind,
    ) -> Module {
        let module_id = self
            .module_tree
            .allocate(Some(parent), Some(name), source_kind);
        self.load_module_at(input, source_kind, module_id, Some(parent), Some(name))
    }

    fn load_module_at(
        &mut self,
        input: &str,
        source_kind: SourceKind,
        module_id: ModuleId,
        parent: Option<ModuleId>,
        name: Option<Key>,
    ) -> Module {
        self.current_module_id = module_id;

        let ast = parse(input);
        self.module_tree.set_source_kind(module_id, source_kind);
        if source_kind == SourceKind::User {
            reject_userland_builtin_attributes(self, &ast);
        }

        let exprs: Vec<Idx<Expr>> = ast
            .exprs()
            .map(|expr| self.lower_expr_statement(Some(expr)))
            .collect();

        let module = Module {
            exprs: exprs.into(),
            id: module_id,
            parent,
            name,
            source_kind,
        };

        if source_kind == SourceKind::StandardLibrary
            && name.is_some_and(|name| self.lookup(name) == PRELUDE)
        {
            self.stdlib_prelude = Some(module_id);
        }

        self.type_check_module(&module);
        self.loaded_module_count += 1;

        module
    }

    /// Loads all files in the standard library tree.
    ///
    /// The `core` directory is the root of the public library root.
    /// Directory names become module namespaces (ex. "/collections" -> Collections namespace)
    /// A file with the same name as its parent directory makes that namespace also a module
    /// with a real body (ex. "/collections/list/list.hop" -> List module).
    /// The prelude is compiled last so its re-exports are ready.
    pub fn load_stdlib(&mut self) -> std::io::Result<Vec<Module>> {
        let stdlib_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib");
        let core_root = stdlib_root.join("core");
        let mut paths = Vec::new();
        collect_hopper_files(&core_root, &mut paths)?;
        paths.sort();
        // The prelude imports/re-exports declarations from the other core
        // modules, so compile it after their exports have been collected.
        paths.sort_by_key(|path| is_prelude(path));

        let core_key = self.interner.intern("core");
        let core_module = self.module_tree.ensure_child(
            ModuleId::BOOTSTRAP,
            core_key,
            SourceKind::StandardLibrary,
        );
        let mut planned = Vec::with_capacity(paths.len());

        for path in paths {
            let relative = path
                .strip_prefix(&core_root)
                .expect("stdlib file to be below core root");

            let mut directory_module = core_module;
            let parent_path = relative.parent().unwrap_or_else(|| Path::new(""));
            for component in parent_path.components() {
                let key = self
                    .interner
                    .intern(&component.as_os_str().to_string_lossy());
                directory_module = self.module_tree.ensure_child(
                    directory_module,
                    key,
                    SourceKind::StandardLibrary,
                );
            }

            let stem = path
                .file_stem()
                .expect("stdlib file to have a stem")
                .to_string_lossy();
            let stem_key = self.interner.intern(&stem);
            let parent_dir_name = parent_path.file_name().map(|name| name.to_string_lossy());

            // A same-named file is like if the parent directory had a
            // module (for example `collections/list/list.hop`) can be imported without
            // the double "list"
            let module_id = if parent_dir_name.as_deref() == Some(stem.as_ref()) {
                directory_module
            } else {
                self.module_tree.ensure_child(
                    directory_module,
                    stem_key,
                    SourceKind::StandardLibrary,
                )
            };
            let node = self.module_tree.get(module_id).expect("module to exist");
            planned.push((path, module_id, node.parent, node.name));
        }

        let mut modules = Vec::with_capacity(planned.len());
        for (path, module_id, parent, name) in planned {
            let input = std::fs::read_to_string(&path)?;
            modules.push(self.load_module_at(
                &input,
                SourceKind::StandardLibrary,
                module_id,
                parent,
                name,
            ));
        }

        Ok(modules)
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

    pub fn load_module_file_in(
        &mut self,
        parent: ModuleId,
        name: Key,
        path: &Path,
        source_kind: SourceKind,
    ) -> std::io::Result<Module> {
        let input = std::fs::read_to_string(path)?;
        let module_id = self.module_tree.ensure_child(parent, name, source_kind);
        Ok(self.load_module_at(&input, source_kind, module_id, Some(parent), Some(name)))
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

fn reject_userland_builtin_attributes(context: &mut Context, ast: &Root) {
    for range in ast.builtin_attribute_ranges() {
        context
            .diagnostics
            .push(LoweringDiagnostic::builtin_attribute_not_allowed(range).into());
    }
}

/// In "function mode", the only top-level expression is a function expression
pub fn lower_function(input: &str) -> (Idx<Expr>, Context) {
    let (ast, mut context) = parse_with_context(input);
    reject_userland_builtin_attributes(&mut context, &ast);

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
    reject_userland_builtin_attributes(&mut context, &ast);

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
            type_params: Box::new([]),
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
