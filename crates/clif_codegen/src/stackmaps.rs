use std::{collections::BTreeMap, mem};

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
    pub fn register_stack_maps(&mut self) {
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
