use lasso::{Rodeo, Spur};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Spur);

#[derive(Default, Debug)]
pub(crate) struct Interner(Rodeo);

impl Interner {
    pub fn intern(&mut self, s: &str) -> Name {
        Name(self.0.get_or_intern(s))
    }

    pub fn lookup(&self, key: Name) -> &str {
        self.0.resolve(&key.0)
    }
}
