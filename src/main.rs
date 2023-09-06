use std::io::Read;
use std::fs::File;
use std::env::args;

mod tokenizer;
mod parser;
mod generator;

fn main() {
    let path = args().nth(1).expect("No file provided"); 
    let mut file = File::open(path).expect("Could not open file");
    let mut text = String::new();
    file.read_to_string(&mut text).expect("Could not read from file");
    text.push('\n');
    let mut tokenizer = tokenizer::Tokenizer::new(text);
    tokenizer.tokenize();
    println!("Done creating tokens!");
    let mut parser = parser::Parser::new(tokenizer.tokens);
    parser.parse();
    println!("Done parsing!");
    let mut generator = generator::Generator::new();
    generator.gen(parser.parse_tree);
    println!("{}", generator.string);
}
