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

    #[cfg(debug_assertions)]
    #[test]
    fn allocation_entry_captures_hopper_call_site() {
        let input = r#"
let main = fun () -> {
    let first = "first"
    let second = "second"
    first
}
"#;
        let (module, context) = mir::construct(input);
        assert!(context.diagnostics.is_empty());

        let mut jit = JIT::with_builtins();
        let mut func_map = HashMap::new();
        jit.compile_module(&module, &mut func_map, &context)
            .expect("test program to compile");
        jit.module
            .finalize_definitions()
            .expect("test functions to finalize");
        jit.register_stack_maps();

        let (_, main_id) = module.entry_points.iter().next().expect("main entry point");
        let main_ptr = jit.module.get_finalized_function(func_map[main_id]);
        // SAFETY: the Hopper function has no parameters and returns one word.
        let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };
        runtime::ffi::probe_next_live_allocation_roots();
        let first_string = main();
        assert_ne!(first_string, 0);

        let observed = runtime::ffi::take_probed_roots()
            .expect("second allocation to expose the first string as a live root");
        assert!(observed.roots.iter().any(|root| root.word == first_string));

        let captured = runtime::ffi::take_last_allocation_call_site()
            .expect("allocator entry shim to capture a call site");
        let (frame_to_fp_offset, root_count) = runtime::stack_map_info(captured.return_address)
            .expect("captured call site to have registered Hopper frame and root metadata");
        assert!(root_count > 0);
        assert_eq!(observed.return_address, captured.return_address);
        assert_eq!(
            captured.caller_fp.checked_sub(captured.caller_sp),
            Some(frame_to_fp_offset as usize),
            "captured call-time SP must agree with Cranelift's frame layout"
        );
    }

    #[cfg(debug_assertions)]
    #[test]
    fn allocation_walks_roots_in_hopper_caller() {
        let input = r#"
let allocate = fun () -> Int {
    let child = "child"
    0
}
let main = fun () -> {
    let first = "first"
    allocate ()
    first
}
"#;
        let (module, context) = mir::construct(input);
        assert!(context.diagnostics.is_empty(), "{:#?}", context.diagnostics);

        let mut jit = JIT::with_builtins();
        let mut func_map = HashMap::new();
        jit.compile_module(&module, &mut func_map, &context)
            .expect("test program to compile");
        jit.module.finalize_definitions().unwrap();
        jit.register_stack_maps();

        let (_, main_id) = module.entry_points.iter().next().expect("main entry point");
        let main_ptr = jit.module.get_finalized_function(func_map[main_id]);
        // SAFETY: main has no parameters and returns a single string handle.
        let main: extern "C" fn() -> usize = unsafe { std::mem::transmute(main_ptr) };
        runtime::ffi::probe_next_live_allocation_roots();
        let first = main();
        let observed = runtime::ffi::take_probed_roots()
            .expect("allocation in callee to find caller's live string");
        assert!(
            observed.roots.iter().any(|root| {
                root.word == first && root.return_address != observed.return_address
            }),
            "expected a live root from the older Hopper frame: {observed:?}"
        );
    }
}
