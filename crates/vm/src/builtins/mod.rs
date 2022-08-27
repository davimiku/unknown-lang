use chunk::{Chunk, Op};

pub(crate) fn print(op: Op, chunk: &Chunk) {
    match op {
        Op::IConstant(i) => println!("{}", chunk.get_int(i)),
        Op::FConstant(i) => println!("{}", chunk.get_float(i)),
        _ => unreachable!(),
    }
}

trait MyLangDisplay {
    fn display(&self) -> &str;
}

// Testing how a built-in could receive arguments
//
// Consider:
// ```
// print 1
// ```
//
// Should `print` allow any args that implement `Display` inside
// the guest language? How to translate the guest trait to a host trait?
//
// Should it be serialized to string before being passed to the built-in?
// `serde` as a bridge between the host and guest language? or share memory?
fn test_print<T: MyLangDisplay>(arg: T) {
    println!("{}", arg.display())
}
