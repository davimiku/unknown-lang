use std::collections::HashMap;

use crate::scope::ModuleScopes;
use crate::type_expr::TypeSymbol;
use crate::{Key, SourceKind, ValueSymbol};

/// Identity for a loaded module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(u32);

impl ModuleId {
    pub const BOOTSTRAP: Self = Self(0);

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl From<u32> for ModuleId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct ModuleNode {
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub name: Option<Key>,
    pub source_kind: SourceKind,
    pub children: HashMap<Key, ModuleId>,
    pub exports: ModuleExports,
}

#[derive(Debug, Default)]
pub struct ModuleExports {
    pub values: HashMap<Key, ValueSymbol>,
    pub types: HashMap<Key, TypeSymbol>,
    pub modules: HashMap<Key, ModuleId>,
}

#[derive(Debug)]
pub struct ModuleTree {
    modules: HashMap<ModuleId, ModuleNode>,
    scopes: HashMap<ModuleId, ModuleScopes>,
    next_id: u32,
}

impl ModuleTree {
    pub fn new() -> Self {
        let bootstrap = ModuleId::BOOTSTRAP;
        let mut modules = HashMap::new();
        modules.insert(
            bootstrap,
            ModuleNode {
                id: bootstrap,
                parent: None,
                name: None,
                source_kind: SourceKind::CompilerBuiltin,
                children: HashMap::new(),
                exports: ModuleExports::default(),
            },
        );

        Self {
            modules,
            scopes: HashMap::from([(bootstrap, ModuleScopes::new(bootstrap))]),
            next_id: 1,
        }
    }

    pub fn allocate(
        &mut self,
        parent: Option<ModuleId>,
        name: Option<Key>,
        source_kind: SourceKind,
    ) -> ModuleId {
        let id = ModuleId(self.next_id);
        self.next_id += 1;
        self.insert(id, parent, name, source_kind);
        id
    }

    pub fn ensure_child(
        &mut self,
        parent: ModuleId,
        name: Key,
        source_kind: SourceKind,
    ) -> ModuleId {
        if let Some(id) = self.child(parent, name) {
            return id;
        }
        self.allocate(Some(parent), Some(name), source_kind)
    }

    pub fn insert(
        &mut self,
        id: ModuleId,
        parent: Option<ModuleId>,
        name: Option<Key>,
        source_kind: SourceKind,
    ) {
        if let (Some(parent_id), Some(name_key)) = (parent, name) {
            self.modules
                .get_mut(&parent_id)
                .expect("module parent to exist")
                .children
                .insert(name_key, id);
        }

        self.next_id = self.next_id.max(id.0 + 1);
        self.scopes.insert(id, ModuleScopes::new(id));
        self.modules.insert(
            id,
            ModuleNode {
                id,
                parent,
                name,
                source_kind,
                children: HashMap::new(),
                exports: ModuleExports::default(),
            },
        );
    }

    pub fn get(&self, id: ModuleId) -> Option<&ModuleNode> {
        self.modules.get(&id)
    }

    pub fn scope(&self, id: ModuleId) -> &ModuleScopes {
        self.scopes.get(&id).expect("module scope to exist")
    }

    pub fn scope_mut(&mut self, id: ModuleId) -> &mut ModuleScopes {
        self.scopes.get_mut(&id).expect("module scope to exist")
    }

    pub fn set_source_kind(&mut self, id: ModuleId, source_kind: SourceKind) {
        self.modules
            .get_mut(&id)
            .expect("module to exist")
            .source_kind = source_kind;
    }

    pub fn export_value(&mut self, id: ModuleId, name: Key, symbol: ValueSymbol) {
        self.modules
            .get_mut(&id)
            .expect("module to exist")
            .exports
            .values
            .insert(name, symbol);
    }

    pub fn export_type(&mut self, id: ModuleId, name: Key, symbol: TypeSymbol) {
        self.modules
            .get_mut(&id)
            .expect("module to exist")
            .exports
            .types
            .insert(name, symbol);
    }

    pub fn export_module(&mut self, id: ModuleId, name: Key, module: ModuleId) {
        self.modules
            .get_mut(&id)
            .expect("module to exist")
            .exports
            .modules
            .insert(name, module);
    }

    pub fn child(&self, parent: ModuleId, name: Key) -> Option<ModuleId> {
        self.modules
            .get(&parent)
            .and_then(|module| module.children.get(&name).copied())
    }

    pub fn resolve_path(&self, from: ModuleId, path: &[Key]) -> Option<ModuleId> {
        path.iter()
            .try_fold(from, |module, segment| self.module_member(module, *segment))
    }

    /// Resolves a namespace member, including a child module or a re-exported module alias
    pub fn module_member(&self, module: ModuleId, name: Key) -> Option<ModuleId> {
        self.child(module, name).or_else(|| {
            self.modules
                .get(&module)
                .and_then(|module| module.exports.modules.get(&name).copied())
        })
    }
}
