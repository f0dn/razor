use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    process::Command,
};

use crate::{
    generator::Generator, parser::Parser, path::UsePath, preproc::Preproc, tokenizer::Tokenizer,
};

struct FileState {
    preproc: Preproc,
    compiled_text: Option<String>,
}

impl FileState {
    fn new(path: &UsePath) -> FileState {
        let path = if path.first() == Some(&String::from("std")) {
            String::from("/usr/share/razor/") + &path.to_path()
        } else {
            path.to_path()
        };
        let mut file = File::open(path).expect("Could not open file");

        // TODO is this the way?
        let mut text = String::new();
        file.read_to_string(&mut text)
            .expect("Could not read from file");
        text.push('\n');

        let mut tokenizer = Tokenizer::new(&text);
        tokenizer.tokenize();

        let preproc = Preproc::new(tokenizer.tokens);

        FileState {
            preproc,
            compiled_text: None,
        }
    }
}

pub struct Compiler {
    files: HashMap<UsePath, FileState>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            files: HashMap::new(),
        }
    }

    fn compile_for_macros(&mut self, path: &UsePath) {
        if !self.files.contains_key(path) {
            let mut file_state = FileState::new(path);

            let mut macros = HashMap::new();

            let macro_uses = file_state.preproc.preprocess_uses();

            for macro_use in &macro_uses {
                self.compile_for_macros(&macro_use.path);
            }

            for macro_use in &macro_uses {
                if let Some(mac) = self
                    .files
                    .get(&macro_use.path)
                    .unwrap()
                    .preproc
                    .macros
                    .get(&macro_use.id)
                {
                    macros.insert(&macro_use.id, mac);
                }
            }

            file_state.preproc.preprocess_macro_calls(&macros);

            file_state.preproc.preprocess_macros();

            file_state.preproc.preprocess_macro_calls(&macros);

            self.files.insert(path.clone(), file_state);
        }
    }

    fn compile_full(&mut self, path: &UsePath, is_main: bool) {
        let mut file_state = if let Some(file_state) = self.files.remove(path) {
            if file_state.compiled_text.is_some() {
                self.files.insert(path.clone(), file_state);
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
            if let Some(mac) = self
                .files
                .get(&macro_use.path)
                .unwrap()
                .preproc
                .macros
                .get(&macro_use.id)
            {
                macros.insert(&macro_use.id, mac);
            }
        }

        file_state.preproc.preprocess_macro_calls(&macros);

        file_state.preproc.preprocess_macros();

        file_state.preproc.preprocess_macro_calls(&macros);

        let mut parser = Parser::new(file_state.preproc.take_tokens());
        parser.parse();

        let mut generator = Generator::new(path);
        generator.gen(&parser.parse_tree, !is_main);

        for link in generator.links() {
            self.compile_full(link, false);
        }

        file_state.compiled_text = Some(generator.text);

        self.files.insert(path.clone(), file_state);
    }

    pub fn compile(&mut self, path: &str, out_path: &str, keep_asm: bool) {
        self.compile_full(&UsePath::from_path(path), true);

        for (path, file_state) in &self.files {
            if let Some(compiled_text) = &file_state.compiled_text {
                let asm_path = format!("{}.asm", path);
                let mut out = File::create(&asm_path).expect("Can't create file");
                out.write_all(compiled_text.as_bytes())
                    .expect("Can't write to file");

                let object_path = format!("{}.o", path);

                Command::new("nasm")
                    .args(["-f", "elf64"])
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
            .filter(|(_, state)| state.compiled_text.is_some())
            .map(|(path, _)| format!("{}.o", path))
            .collect::<Vec<String>>();

        Command::new("ld")
            .args(files)
            .arg("-o")
            .arg(out_path)
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
