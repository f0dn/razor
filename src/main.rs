use std::env::args;

pub use razor::*;

pub fn main() {
    let mut args = args();
    args.next();
    let inp_path = args.next().expect("No input file provided");
    let out_path = args.next().expect("No output file provided");
    let keep_asm = args.next().is_some();

    compiler::Compiler::new().compile(&inp_path, &out_path, keep_asm);
}
