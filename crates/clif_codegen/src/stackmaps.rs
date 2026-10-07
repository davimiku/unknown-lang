use std::mem;

use itertools::Itertools;

use crate::jit::JIT;

pub(crate) struct PendingFunctionStackMap {
    pub(crate) code_size: u32,
    pub(crate) frame_to_fp_offset: u32,
    pub(crate) safepoints: Vec<PendingSafepoint>,
}

pub(crate) struct PendingSafepoint {
    pub(crate) return_address_offset: u32,
    pub(crate) stack_span: u32,
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
                    return_address: function_start
                        .checked_add(safepoint.return_address_offset as usize)
                        .expect("safepoint address overflow"),
                    stack_span: safepoint.stack_span,
                    root_offsets: safepoint.root_offsets,
                })
                .collect_vec();

            runtime::register_function_stack_maps(
                function_start,
                function_size,
                pending_stack_map.frame_to_fp_offset,
                safepoints,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn emits_root_for_string_across_later_allocations() {
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

        let functions_with_roots = jit
            .pending_stack_maps
            .values()
            .filter(|func| func.safepoints.iter().any(|sp| !sp.root_offsets.is_empty()))
            .collect_vec();

        assert_eq!(functions_with_roots.len(), 1);
        let function = functions_with_roots[0];
        assert!(function.frame_to_fp_offset > 0);
        assert!(function.safepoints.iter().any(|sp| {
            !sp.root_offsets.is_empty()
                && sp.root_offsets.iter().all(|offset| {
                    offset
                        .checked_add(8)
                        .is_some_and(|end| end <= sp.stack_span)
                })
        }));
    }
}
