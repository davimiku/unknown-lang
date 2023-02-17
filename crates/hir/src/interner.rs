use lasso::{Rodeo, Spur};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(Spur);

#[derive(Default, Debug)]
pub(crate) struct Interner(Rodeo);

impl Interner {
    pub fn intern(&mut self, s: &str) -> Key {
        Key(self.0.get_or_intern(s))
    }

    pub fn lookup(&self, key: Key) -> &str {
        self.0.resolve(&key.0)
    }
}
