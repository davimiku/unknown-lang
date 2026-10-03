use std::mem;

use itertools::Itertools;

use crate::jit::JIT;

pub(crate) struct PendingFunctionStackMap {
    pub(crate) code_size: u32,
    pub(crate) safepoints: Vec<PendingSafepoint>,
}

pub(crate) struct PendingSafepoint {
    pub(crate) code_offset: u32,
    pub(crate) frame_size: u32,
    pub(crate) root_offsets: Vec<u32>,
}

impl JIT {
    pub(crate) fn register_stack_maps(&mut self) {
        let pending_stack_maps = mem::take(&mut self.pending_stack_maps);
        for (func_id, pending_stack_map) in pending_stack_maps {
            let function_start = self.module.get_finalized_function(func_id) as usize;
            let function_size = pending_stack_map.code_size;

            let safepoints = pending_stack_map
                .safepoints
                .into_iter()
                .map(|safepoint| runtime::Safepoint {
                    return_address: function_start + (safepoint.code_offset as usize),
                    frame_size: safepoint.frame_size,
                    root_offsets: safepoint.root_offsets,
                })
                .collect_vec();

            runtime::register_function_stack_maps(function_start, function_size, safepoints);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn emits_root_for_lib_across_later_allocations() {
        let input = r#"
let main = fun () -> {
    let first = "first"
    let second = "second"
    first
}
"#;

        let (module, context) = mir::construct(input);

        assert_eq!(context.diagnostics, vec![]);

        let mut jit = JIT::with_builtins();
        let mut func_map = HashMap::new();

        jit.compile_module(&module, &mut func_map, &context)
            .expect("test program to compile");

        let safepoints = jit
            .pending_stack_maps
            .values()
            .flat_map(|func| &func.safepoints)
            .collect_vec();

        assert!(!safepoints.is_empty());
        assert!(safepoints
            .iter()
            .any(|safepoint| !safepoint.root_offsets.is_empty()))
    }
}
