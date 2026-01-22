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

    // TODO keep_asm should just be for debugging
    pub fn compile(&mut self, path: &str, out_path: &str, keep_asm: bool) {
        //TODO make sure the loop ends
        //TODO handle recursive macros

        let mut stack = vec![(UsePath::from_path(path), None, false, true, true)];
        while let Some((
            current_path,
            macro_uses,
            used_macros_preprocessed,
            compile_full,
            is_main,
        )) = stack.pop()
        {
            let mut file_state = self
                .files
                .remove(&current_path)
                .unwrap_or_else(|| FileState::new(&current_path));

            if !file_state.preproc.is_preprocessed() {
                let macro_uses = macro_uses.unwrap_or_else(|| file_state.preproc.preprocess_uses());

                if !used_macros_preprocessed {
                    stack.push((
                        current_path.clone(),
                        Some(macro_uses.clone()),
                        true,
                        compile_full,
                        is_main,
                    ));
                    for macro_use in macro_uses {
                        stack.push((macro_use.path, None, false, false, false));
                    }
                    self.files.insert(current_path, file_state);
                    continue;
                } else {
                    let mut macros = HashMap::new();
                    for macro_use in &macro_uses {
                        if let Some(mac) = self
                            .files
                            .get(&macro_use.path)
                            .unwrap() // TODO I think this never fails
                            .preproc
                            .macros
                            .get(&macro_use.id)
                        {
                            macros.insert(&macro_use.id, mac);
                        }
                        file_state.preproc.preprocess(&macros);
                    }
                }
            }

            if compile_full && file_state.compiled_text.is_none() {
                let mut parser = Parser::new(file_state.preproc.take_tokens());
                parser.parse();

                let mut generator = Generator::new(&current_path);
                generator.gen(&parser.parse_tree, !is_main);

                for link in generator.links() {
                    stack.push((link.clone(), None, false, true, false));
                }

                file_state.compiled_text = Some(generator.text);
            }
            self.files.insert(current_path, file_state);
        }

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
