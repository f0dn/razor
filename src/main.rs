use std::env::args;
use std::fs::File;
use std::io::{Read, Write};
use std::process::Command;

mod generator;
mod parser;
mod tokenizer;

fn main() {
    let inp_path = args().nth(1).expect("No input file provided");
    let out_path = args().nth(2).expect("No output file provided");

    let mut files = Vec::new();
    compile(&inp_path, false, &mut files);

    Command::new("ld")
        .args(&files)
        .arg("-o")
        .arg(&out_path)
        .status()
        .expect("Failed to execute ld");

    for file in &files {
        Command::new("rm")
            .arg(&file)
            .status()
            .expect("Failed to execute rm");
    }
}

fn compile(path: &str, lib: bool, files: &mut Vec<String>) {
    let mut file = File::open(path).expect("Could not open file");

    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect("Could not read from file");
    text.push('\n');

    let mut tokenizer = tokenizer::Tokenizer::new(text);
    tokenizer.tokenize();

    let mut parser = parser::Parser::new(tokenizer.tokens);
    parser.parse();

    let mut generator = generator::Generator::new(&parser.macros);
    generator.gen(&parser.parse_tree, lib);

    let out_path = format!("{}.asm", path);
    let mut out = File::create(&out_path).expect("Can't create file");
    out.write_all(generator.text.as_bytes())
        .expect("Can't write to file");

    files.push(out_path.replace(".asm", ".o"));

    Command::new("nasm")
        .args(&["-f", "elf64"])
        .arg(&out_path)
        .arg("-o")
        .arg(&out_path.replace(".asm", ".o"))
        .status()
        .expect("Failed to execute nasm");

    Command::new("rm")
        .arg(&out_path)
        .status()
        .expect("Failed to execute rm");

    for name in &generator.links {
        compile(name, true, files);
    }
}
