use std::env::args;

mod TODO;
mod compiler;
mod generator;
mod parser;
mod preproc;
mod tokenizer;
mod tokenlist;

const ERROR_SIZE: usize = 100;

fn main() {
    let mut args = args();
    args.next();
    let inp_path = args.next().expect("No input file provided");
    let out_path = args.next().expect("No output file provided");
    let keep_asm = args.next().is_some();

    compiler::Compiler::new().compile(&inp_path, &out_path, keep_asm);
}
