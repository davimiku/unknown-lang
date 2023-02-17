use std::collections::{HashMap, HashSet};

use text_size::TextRange;

use crate::{interner::Key, Type};

pub(crate) enum Definition {
    Function(Function),
    Record(Record),
}

pub(crate) struct Function {
    pub params: Vec<Param>,
    pub return_ty: Type,
}

pub(crate) struct Record {
    pub fields: Vec<Field>,
}

pub struct RangeInfo {
    pub whole: TextRange,
    pub name: TextRange,
    pub tys: TysRangeInfo,
}

pub enum TysRangeInfo {
    Function {
        return_ty: Option<TextRange>,
        param_tys: Vec<Option<TextRange>>,
    },
    Record {
        field_tys: Vec<Option<TextRange>>,
    },
}

pub(crate) struct Param {
    pub name: Option<Key>,
    pub ty: Type,
}

pub(crate) struct Field {
    pub name: Option<Key>,
    pub ty: Type,
}

struct Docs {
    paras: Vec<String>,
}

// ----------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Fqn {
    pub module: Key,
    pub name: Key,
}

pub(crate) enum Path {
    ThisModule(Key),
    OtherModule(Fqn),
}

pub(crate) enum PathWithRange {
    ThisModule {
        name: Key,
        range: TextRange,
    },
    OtherModule {
        fqn: Fqn,
        module_range: TextRange,
        name_range: TextRange,
    },
}

pub(crate) struct NameResDiagnostic {
    pub kind: NameResDiagnosticKind,
    pub range: TextRange,
}

pub(crate) enum NameResDiagnosticKind {
    Undefined(Key),
}

pub struct Index {
    pub(crate) definitions: HashMap<Key, Definition>,
    pub(crate) range_info: HashMap<Key, RangeInfo>,
    docs: HashMap<Key, Docs>,
    tys: HashSet<ast::Ident>,
}

impl Index {
    pub(crate) fn functions(&self) -> impl Iterator<Item = (Key, &Function)> {
        self.definitions
            .iter()
            .filter_map(|(name, definition)| match definition {
                Definition::Function(f) => Some((*name, f)),
                Definition::Record(_) => None,
            })
    }

    pub(crate) fn get_definition(&self, name: Key) -> Option<&Definition> {
        self.definitions.get(&name)
    }

    pub(crate) fn range_info(&self, name: Key) -> &RangeInfo {
        &self.range_info[&name]
    }

    pub(crate) fn definition_names(&self) -> impl Iterator<Item = Key> + '_ {
        self.definitions.keys().copied()
    }

    pub(crate) fn function_names(&self) -> impl Iterator<Item = Key> + '_ {
        self.definitions.iter().filter_map(|(name, def)| match def {
            Definition::Function(_) => Some(*name),
            Definition::Record(_) => None,
        })
    }

    pub(crate) fn ranges(&self) -> impl Iterator<Item = (Key, &RangeInfo)> + '_ {
        self.range_info.iter().map(|(n, r)| (*n, r))
    }

    pub(crate) fn is_ident_ty(&self, ident: ast::Ident) -> bool {
        self.tys.contains(&ident)
    }

    fn shrink_to_fit(&mut self) {
        let Self {
            definitions,
            range_info,
            docs,
            tys,
        } = self;
        definitions.shrink_to_fit();
        range_info.shrink_to_fit();
        docs.shrink_to_fit();
        tys.shrink_to_fit();
    }
}

pub(crate) fn resolve<'a>(
    path: PathWithRange,
    index: &'a Index,
    world_index: &'a WorldIndex,
    diagnostics: &mut Vec<NameResDiagnostic>,
) -> Option<&'a Definition> {
    match path {
        PathWithRange::ThisModule { name, range } => match index.get_definition(name) {
            Some(definition) => Some(definition),
            None => {
                diagnostics.push(NameResDiagnostic {
                    kind: NameResDiagnosticKind::Undefined(name),
                    range,
                });
                None
            }
        },

        PathWithRange::OtherModule {
            fqn,
            module_range,
            name_range,
        } => match world_index.get_definition(fqn) {
            Ok(definition) => Some(definition),
            Err(GetDefinitionError::UnknownModule) => {
                diagnostics.push(NameResDiagnostic {
                    kind: NameResDiagnosticKind::Undefined(fqn.module),
                    range: module_range,
                });
                None
            }
            Err(GetDefinitionError::UnknownDefinition) => {
                diagnostics.push(NameResDiagnostic {
                    kind: NameResDiagnosticKind::Undefined(fqn.name),
                    range: name_range,
                });
                None
            }
        },
    }
}

impl PathWithRange {
    pub fn path(self) -> Path {
        match self {
            PathWithRange::ThisModule { name, .. } => Path::ThisModule(name),
            PathWithRange::OtherModule { fqn, .. } => Path::OtherModule(fqn),
        }
    }
}

pub struct WorldIndex(HashMap<Key, Index>);

impl WorldIndex {
    pub(crate) fn get_definition(&self, fqn: Fqn) -> Result<&Definition, GetDefinitionError> {
        match self.0.get(&fqn.module) {
            Some(index) => match index.get_definition(fqn.name) {
                Some(def) => Ok(def),
                None => Err(GetDefinitionError::UnknownDefinition),
            },
            None => Err(GetDefinitionError::UnknownModule),
        }
    }

    pub(crate) fn range_info(&self, fqn: Fqn) -> &RangeInfo {
        &self.0[&fqn.module].range_info[&fqn.name]
    }

    pub(crate) fn add_module(&mut self, module: Key, index: Index) {
        assert!(self.0.insert(module, index).is_none());
    }

    pub(crate) fn update_module(&mut self, module: Key, index: Index) {
        *self.0.get_mut(&module).unwrap() = index;
    }

    pub(crate) fn ranges(&self) -> impl Iterator<Item = (Fqn, &RangeInfo)> + '_ {
        self.0.iter().flat_map(|(module, index)| {
            index.ranges().map(|(name, range)| {
                (
                    Fqn {
                        module: *module,
                        name,
                    },
                    range,
                )
            })
        })
    }
}

#[derive(Debug)]
pub enum GetDefinitionError {
    UnknownModule,
    UnknownDefinition,
}
