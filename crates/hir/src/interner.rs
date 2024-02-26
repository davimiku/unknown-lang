use lasso::{Rodeo, Spur};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key(Spur);

#[derive(Debug)]
pub struct Interner(Rodeo, CoreKeys);

impl Default for Interner {
    fn default() -> Self {
        let mut interner = Self(Rodeo::default(), CoreKeys::default());

        interner.1 = CoreKeys::new(&mut interner);

        interner
    }
}

impl Interner {
    /// Interns the given string and returns the Key for
    /// that string.
    ///
    /// If the string was already interned, returns the existing Key.
    pub fn intern(&mut self, s: &str) -> Key {
        Key(self.0.get_or_intern(s))
    }

    /// Gets the string out of the interner for the given Key.
    pub fn lookup(&self, key: Key) -> &str {
        self.0.resolve(&key.0)
    }

    pub fn core_keys(&self) -> &CoreKeys {
        &self.1
    }
}

#[derive(Debug, Default)]
pub struct CoreKeys {
    /// `Bool`
    pub bool: Key,
    /// `Int`
    pub int: Key,
    /// `Float`
    pub float: Key,
    /// `String`
    pub string: Key,

    /// `print`
    pub print: Key,
    /// `args`
    pub args: Key,

    /// `+`
    pub add: Key,
    /// `++`
    pub concat: Key,
    /// `-`
    pub sub: Key,
    /// `*`
    pub mul: Key,
    /// `/`
    pub div: Key,
    /// `%`
    pub rem: Key,

    /// `==`
    pub eq: Key,
    /// `!=`
    pub ne: Key,
    /// `<`
    pub lt: Key,
    /// `<=`
    pub le: Key,
    /// `>`
    pub gt: Key,
    /// `>=`
    pub ge: Key,
}

impl CoreKeys {
    fn new(interner: &mut Interner) -> Self {
        Self {
            bool: interner.intern("Bool"),
            int: interner.intern("Int"),
            float: interner.intern("Float"),
            string: interner.intern("String"),

            print: interner.intern("print"),
            args: interner.intern("args"),

            add: interner.intern("+"),
            concat: interner.intern("++"),
            sub: interner.intern("-"),
            mul: interner.intern("*"),
            div: interner.intern("/"),
            rem: interner.intern("%"),

            eq: interner.intern("=="),
            ne: interner.intern("!="),
            lt: interner.intern("<"),
            le: interner.intern("<="),
            gt: interner.intern(">"),
            ge: interner.intern(">="),
        }
    }
}
