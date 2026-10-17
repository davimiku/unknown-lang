use std::collections::BTreeMap;

#[derive(Debug, Default)]
pub struct StackMapRegistry {
    safepoints: BTreeMap<usize, Safepoint>,
    function_ranges: Vec<FunctionRange>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Safepoint {
    pub return_address: usize,
    pub stack_span: u32,
    pub root_offsets: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionRange {
    pub start: usize,
    pub end: usize,
    /// Offset from the bottom (SP) of this function's frame to its FP
    pub frame_to_fp_offset: u32,
}

impl StackMapRegistry {
    pub fn register_function(
        &mut self,
        function_start: usize,
        function_size: u32,
        frame_to_fp_offset: u32,
        safepoints: impl IntoIterator<Item = Safepoint>,
    ) {
        let function_size = function_size as usize;
        let function_end = function_start + function_size;

        let function_range = FunctionRange {
            start: function_start,
            end: function_end,
            frame_to_fp_offset,
        };
        self.function_ranges.push(function_range);

        for safepoint in safepoints {
            assert!(
                safepoint.return_address >= function_start
                    && safepoint.return_address < function_end,
                "safepoint {:#x} lies outside function range {:#x}..{:#x}",
                safepoint.return_address,
                function_start,
                function_end,
            );

            let return_address = safepoint.return_address;
            let previous = self.safepoints.insert(return_address, safepoint);
            assert!(
                previous.is_none(),
                "duplicate stack map for return address {return_address:#x}"
            );
        }
    }

    pub fn get(&self, return_address: usize) -> Option<&Safepoint> {
        self.safepoints.get(&return_address)
    }

    pub fn function_containing(&self, address: usize) -> Option<FunctionRange> {
        self.function_ranges
            .iter()
            .find(|range| address >= range.start && address < range.end)
            .cloned()
    }

    pub fn len(&self) -> usize {
        self.safepoints.len()
    }

    pub fn is_empty(&self) -> bool {
        self.safepoints.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registers_and_finds_multiple_safepoints() {
        let mut registry = StackMapRegistry::default();

        registry.register_function(
            0x1000,
            0x100,
            32,
            [
                Safepoint {
                    return_address: 0x1020,
                    stack_span: 64,
                    root_offsets: vec![8, 24],
                },
                Safepoint {
                    return_address: 0x1060,
                    stack_span: 64,
                    root_offsets: vec![16],
                },
            ],
        );

        assert_eq!(
            registry.get(0x1020),
            Some(&Safepoint {
                return_address: 0x1020,
                stack_span: 64,
                root_offsets: vec![8, 24],
            })
        );

        assert_eq!(
            registry.get(0x1060),
            Some(&Safepoint {
                return_address: 0x1060,
                stack_span: 64,
                root_offsets: vec![16],
            })
        );

        assert_eq!(registry.get(0x1040), None);
    }

    #[test]
    fn finds_function_containing_address() {
        let mut registry = StackMapRegistry::default();

        registry.register_function(0x1000, 0x100, 32, []);

        assert_eq!(
            registry.function_containing(0x1080),
            Some(FunctionRange {
                start: 0x1000,
                end: 0x1100,
                frame_to_fp_offset: 32,
            })
        );

        assert_eq!(registry.function_containing(0x1100), None);
    }

    #[test]
    #[should_panic(expected = "lies outside function range")]
    fn rejects_safepoint_outside_function() {
        let mut registry = StackMapRegistry::default();

        registry.register_function(
            0x1000,
            0x100,
            32,
            [Safepoint {
                return_address: 0x1200,
                stack_span: 64,
                root_offsets: vec![8],
            }],
        );
    }
}
