//! Type definitions for builtin functions.
//!
//! Currently this is harded coded, but eventually many
//! or all of these will be parsed from definition files
//! on startup.

use std::{collections::HashMap, vec};

use crate::Type;

pub(super) type BuiltinSignatures = HashMap<&'static str, BuiltinFunctionSignature>;

#[derive(Debug)]
pub(super) struct BuiltinFunctionSignature {
    pub(super) arg_types: Vec<Type>,
    pub(super) return_type: Type,
}

// TODO: compile-time HashMap
pub(super) fn get_builtin_functions() -> BuiltinSignatures {
    let mut types = HashMap::new();

    types.insert(
        "print",
        BuiltinFunctionSignature {
            arg_types: vec![Type::String],
            return_type: Type::Unit,
        },
    );

    types
}
