use std::io::Read;
use std::fs::File;
use std::env::args;

mod tokenizer;

fn main() {
    let path = args().nth(1).expect("No file provided"); 
    let mut file = File::open(path).expect("Could not open file");
    let mut text = String::new();
    file.read_to_string(&mut text).expect("Could not read from file");
    text.push('\n');
    println!("{}", text);
    let mut tokenizer = tokenizer::Tokenizer::new(text);
    tokenizer.tokenize();
}
