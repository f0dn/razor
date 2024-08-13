use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    process::Command,
};

use crate::{generator::Generator, parser::Parser, preproc::Preproc, tokenizer::Tokenizer};

struct FileState {
    preproc: Preproc,
    compiled_text: Option<String>,
}

impl FileState {
    fn new(path: &String) -> FileState {
        let mut file = File::open(path).expect("Could not open file");

        // TODO is this the way?
        let mut text = String::new();
        file.read_to_string(&mut text)
            .expect("Could not read from file");
        text.push('\n');

        let mut tokenizer = Tokenizer::new(&text);
        tokenizer.tokenize();

        let mut preproc = Preproc::new(tokenizer.tokens);
        preproc.preprocess_macros();

        return FileState {
            preproc,
            compiled_text: None,
        };
    }
}

pub struct Compiler {
    files: HashMap<String, FileState>,
}

impl Compiler {
    pub fn new() -> Compiler {
        return Compiler {
            files: HashMap::new(),
        };
    }

    fn compile_for_macros(&mut self, path: &String) {
        if !self.files.contains_key(path) {
            self.files.insert(path.clone(), FileState::new(&path));
        }
    }

    fn compile_full(&mut self, path: &String, is_main: bool) {
        let mut file_state = if let Some(file_state) = self.files.remove(path) {
            if file_state.compiled_text.is_some() {
                return;
            } else {
                file_state
            }
        } else {
            FileState::new(path)
        };

        let mut macros = HashMap::new();

        let macro_uses = file_state.preproc.preprocess_uses();
        for macro_use in &macro_uses {
            self.compile_for_macros(&macro_use.path);
        }
        for macro_use in &macro_uses {
            macros.insert(
                &macro_use.path,
                self.files
                    .get(&macro_use.path)
                    .unwrap()
                    .preproc
                    .macros
                    .get(&macro_use.id)
                    .unwrap(),
            );
        }

        file_state.preproc.preprocess_macro_calls(&macros);

        let mut parser = Parser::new(file_state.preproc.take_tokens());
        parser.parse();

        let mut generator = Generator::new();
        generator.gen(&parser.parse_tree, is_main);

        for link in generator.links {
            self.compile_full(&link, false);
        }

        file_state.compiled_text = Some(generator.text);

        self.files.insert(path.to_string(), file_state);
    }

    pub fn compile(&mut self, path: &String, out_path: &String, keep_asm: bool) {
        self.compile_full(path, true);

        for (path, file_state) in &self.files {
            if let Some(compiled_text) = &file_state.compiled_text {
                let asm_path = format!("{}.asm", path);
                let mut out = File::create(&asm_path).expect("Can't create file");
                out.write_all(compiled_text.as_bytes())
                    .expect("Can't write to file");

                let object_path = format!("{}.o", path);

                Command::new("nasm")
                    .args(&["-f", "elf64"])
                    .arg(&asm_path)
                    .arg("-o")
                    .arg(&object_path)
                    .status()
                    .expect("Failed to execute nasm");

                if !keep_asm {
                    Command::new("rm")
                        .arg(&asm_path)
                        .status()
                        .expect("Failed to execute rm");
                }
            }
        }

        let files = &self
            .files
            .iter()
            .map(|(path, _)| format!("{}.o", path))
            .collect::<Vec<String>>();

        Command::new("ld")
            .args(files)
            .arg("-o")
            .arg(&out_path)
            .status()
            .expect("Failed to execute ld");

        for path in files {
            Command::new("rm")
                .arg(path)
                .status()
                .expect("Failed to execute rm");
        }
    }
}
