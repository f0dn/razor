use std::io::{Read, Write};
use std::fs::File;
use std::env::args;

mod tokenizer;
mod parser;
mod generator;

fn main() {
    let inp_path = args().nth(1).expect("No input file provided"); 
    let out_path = args().nth(2).expect("No output file provided");
    let mut file = File::open(inp_path).expect("Could not open file");

    let mut text = String::new();
    file.read_to_string(&mut text).expect("Could not read from file");
    text.push('\n');

    let mut tokenizer = tokenizer::Tokenizer::new(text);
    tokenizer.tokenize();
    println!("Done creating tokens!");

    let mut parser = parser::Parser::new(tokenizer.tokens);
    parser.parse();
    println!("Done parsing!");

    let mut generator = generator::Generator::new(&parser.macros);
    generator.gen(&parser.parse_tree);
    println!("Done generating code!");

    let mut out = File::create(out_path).expect("Can't create file");
    out.write_all(generator.text.as_bytes()).expect("Can't write to file");
}
