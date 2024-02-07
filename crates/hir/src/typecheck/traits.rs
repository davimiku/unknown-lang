use crate::{ContextDisplay, Expr, Type};
pub(crate) struct Trait {
    // type parameters
    /// functions that need to be implemented
    function_sigs: Vec<Type>, // FunctionType

    /// function implementations defined on the trait
    provided_functions: Vec<Expr>, // FunctionExpr
}

impl ContextDisplay for Trait {
    fn display(&self, context: &crate::Context) -> String {
        todo!()
    }
}

// example Trait = Eq
// type parameters - one generic type (no bounds)
// functions that need to be implemented - `==`
// function implementations defined on the trait - `!=`

// `!=` would need to be able to call `==` even though there's no concrete implementation,
// so the type checker needs to resolve `==` to be the type signature from the trait def first
// before looking in scope for that function

// ex. Float would implement `==` as required but also implement `!=` to handle NaN
