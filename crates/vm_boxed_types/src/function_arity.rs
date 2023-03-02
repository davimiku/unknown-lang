/// Packed function arity
///
/// If we limit to 16 function parameters, then the detailed arity
/// can be packed into 8 bytes.
///
/// Each "nibble" (half a byte) encodes the size of a parameter.
///
/// i.e. [u8; 8]
///
/// ```txt
/// |0100 0001|0010 0010|0001 0100|1111 0000|0000 0000|0000 0000|...
///  ^^4  ^^1  ^^2  ^^2  ^^1  ^^4  ^^16 ^^0  ...
/// ```
///
/// In the example above, the first function parameter is 4 words,
/// the second is 1 word, and so on.
///
/// TODO: this may not be necessary. It may only be necessary to store the
/// total number of words of function parameters to be popped at the end of the
/// function. The args are pushed on the stack before calling the function, and the
/// function itself treats the parameters as locals which we'd know the offsets/indexes
/// at compile time.
#[derive(Debug, Clone, Copy, Default)]
pub(super) struct FunctionArity {
    bytes: [u8; 8],
}
