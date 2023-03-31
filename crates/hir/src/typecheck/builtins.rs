//! Type definitions for builtin functions.
//!
//! Currently this is harded coded, but eventually many
//! or all of these will be parsed from definition files
//! on startup.

use std::{collections::HashMap, vec};

use crate::Type;

pub(super) type BuiltinFunctions = HashMap<&'static str, Vec<BuiltinFunctionSignature>>;

#[derive(Debug)]
pub(super) struct BuiltinFunctionSignature {
    pub(super) arg_types: Vec<Type>,
    pub(super) return_type: Type,
}

// TODO: compile-time HashMap
pub(super) fn get_builtin_functions() -> BuiltinFunctions {
    let mut types = HashMap::new();

    types.insert(
        "print",
        vec![
            BuiltinFunctionSignature {
                // print_string
                arg_types: vec![Type::String],
                return_type: Type::Unit,
            },
            BuiltinFunctionSignature {
                // print_int
                arg_types: vec![Type::Int],
                return_type: Type::Unit,
            },
            BuiltinFunctionSignature {
                // print_float
                arg_types: vec![Type::Float],
                return_type: Type::Unit,
            },
            BuiltinFunctionSignature {
                // print_bool
                arg_types: vec![Type::Bool],
                return_type: Type::Unit,
            },
        ],
    );

    types
}
