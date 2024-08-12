use std::env::args;
use std::fs::File;
use std::io::{Read, Write};
use std::process::Command;

mod TODO;
mod generator;
mod parser;
mod preproc;
mod tokenizer;
mod tokenlist;

// STEPS
// tokenize the file
// pre-process by finding # tokens - this outputs tokens which replace the # tokens
// for all the macro imports also tokenize and pre-process those files
// then parse the file and generate
// then link all the files together

fn main() {
    let mut args = args();
    args.next();
    let inp_path = args.next().expect("No input file provided");
    let out_path = args.next().expect("No output file provided");
    let keep_asm = args.next().is_some();

    let mut files = Vec::new();
    compile(&inp_path, false, &mut files, false, keep_asm);

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

fn compile2(path: &str) {
    let mut file = File::open(path).expect("Could not open file");

    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect("Could not read from file");
    text.push('\n');

    //let mut tokenizer = tokenizer::Tokenizer::new(text);
    //tokenizer.tokenize();
}

fn compile(
    path: &str,
    lib: bool,
    files: &mut Vec<String>,
    for_macro: bool,
    keep_asm: bool,
) -> Vec<Macro> {
    let mut file = File::open(path).expect("Could not open file");

    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect("Could not read from file");
    text.push('\n');

    let mut tokenizer = tokenizer::Tokenizer::new(&text);
    tokenizer.tokenize();

    let mut preproc = preproc::Preproc::new(tokenizer.tokens);
    preproc.preprocess();

    let mut parser = parser::Parser::new(preproc.tokens);
    let macro_uses = parser.parse_macro_uses();

    for macro_use in &macro_uses {
        let macros = compile(&macro_use.path, true, files, true, keep_asm);
        for m in macros {
            if m.ident.name == macro_use.ident.name {
                parser.add_macro(m);
                break;
            }
        }
    }

    parser.parse();

    if for_macro {
        return parser.macros;
    }

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

    if !keep_asm {
        Command::new("rm")
            .arg(&out_path)
            .status()
            .expect("Failed to execute rm");
    }

    for name in &generator.links {
        if !files.contains(&format!("{}.o", name)) {
            compile(name, true, files, false, keep_asm);
        }
    }

    return Vec::new();
}
