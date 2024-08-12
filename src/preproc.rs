use std::collections::HashMap;

use crate::{
    tokenizer::{Token, TokenType},
    tokenlist::TokenList,
};

/*
MACRO_USE     : #USE;

MACRO         : mac id ( MACRO_ARG* ) { [token MACRO_REPEAT MACRO_VAR ]* # }

MACRO_CALL    : id # ( token* # )

MACRO_REPEAT  : # ( [token MACRO_REPEAT MACRO_VAR ]* )

MACRO_VAR     : # id

MACRO_ARG     : { token id } or
                token or
                ( MACRO_ARG* )[?*+]
*/

struct Macro {
    name: String,
    args: Vec<MacroArg>,
    body: Vec<MacroBody>,
}

enum MacroBody {
    Var(String),
    Token(Token),
    Repeat(Vec<MacroBody>),
}

struct MacroRepeat {
    vars: HashMap<String, Token>,
    repeats: Vec<Vec<MacroRepeat>>,
}

enum MacroArg {
    Token(TokenType),
    Id(TokenType, String),
    Repeat(Vec<MacroArg>, RepeatType),
}

#[derive(PartialEq)]
enum RepeatType {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

pub struct Preproc {
    pub tokens: TokenList,
    macros: Vec<Macro>,
}

impl Preproc {
    pub fn new(mut tokens: TokenList) -> Preproc {
        tokens.reset();
        return Preproc {
            tokens,
            macros: Vec::new(),
        };
    }

    fn process_macro_args(&mut self) -> Vec<MacroArg> {
        let mut args = Vec::new();

        while !self.peek_type(&TokenType::RPar) {
            let arg = match self.consume().t_type {
                TokenType::LPar => {
                    let args = self.process_macro_args();
                    let repeat = match self.consume().t_type {
                        TokenType::QMark => RepeatType::ZeroOrOne,
                        TokenType::Star => RepeatType::ZeroOrMore,
                        TokenType::Plus => RepeatType::OneOrMore,
                        _ => panic!("Expected repeat token"),
                    };
                    self.consume_type(TokenType::RPar);
                    MacroArg::Repeat(args, repeat)
                }
                TokenType::LBr => {
                    let token = self.consume();
                    let id = match self.consume().t_type {
                        TokenType::Var(id) => id,
                        _ => panic!("Expected identifier"),
                    };
                    self.consume_type(TokenType::RBr);
                    MacroArg::Id(token.t_type, id)
                }
                TokenType::Hash => {
                    let token = self.consume();
                    match token.t_type {
                        TokenType::LBr | TokenType::LPar => MacroArg::Token(token.t_type),
                        _ => panic!("No need to escape {}", token.t_type),
                    }
                }
                token @ _ => MacroArg::Token(token),
            };
            args.push(arg);
        }
        return args;
    }

    fn process_macro_body(&mut self) -> Vec<MacroBody> {
        let mut body = Vec::new();

        loop {
            match self.consume() {
                Token {
                    t_type: TokenType::Hash,
                    line: _,
                } => match self.consume().t_type {
                    TokenType::LPar => body.push(MacroBody::Repeat(self.process_macro_body())),
                    TokenType::RPar | TokenType::LBr => break,
                    TokenType::Var(var) => body.push(MacroBody::Var(var)),
                    token @ _ => panic!("Expected {} after {}", token, TokenType::Hash),
                },
                token @ _ => body.push(MacroBody::Token(token)),
            };
        }

        return body;
    }

    fn process_macro_def(&mut self) {
        self.consume_type(TokenType::Mac);
        let name = match self.consume().t_type {
            TokenType::Var(id) => id,
            _ => panic!("Expected identifier"),
        };

        self.consume_type(TokenType::LPar);

        let args = self.process_macro_args();

        self.consume_type(TokenType::RPar);

        self.consume_type(TokenType::LBr);

        let body = self.process_macro_body();

        self.macros.push(Macro { name, args, body });
    }

    fn process_macro_inner(&mut self, args: &Vec<MacroArg>) -> Option<MacroRepeat> {
        let mut vars = HashMap::new();
        let mut repeats = Vec::new();
        for arg in args {
            match arg {
                MacroArg::Token(t_type) => {
                    if self.peek_type(t_type) {
                        self.consume();
                    } else {
                        return None;
                    }
                }
                MacroArg::Id(t_type, name) => {
                    if self.peek_type(t_type) {
                        vars.insert(name.clone(), self.consume());
                    } else {
                        return None;
                    }
                }
                MacroArg::Repeat(args, repeat_type) => {
                    let mut repeat = Vec::new();
                    loop {
                        if let Some(inner_vars) = self.process_macro_inner(args) {
                            repeat.push(inner_vars);
                        } else {
                            break;
                        }
                        if repeat_type == &RepeatType::ZeroOrOne {
                            break;
                        }
                    }
                    if repeat_type == &RepeatType::OneOrMore && repeat.is_empty() {
                        panic!("Expected at least one repetition");
                    }
                    repeats.push(repeat);
                }
            }
        }

        return Some(MacroRepeat { vars, repeats });
    }

    fn insert_macro_body(&mut self, body: &Vec<MacroBody>, repeat: &MacroRepeat) {
        let mut repeats = repeat.repeats.iter();
        for token in body {
            match token {
                MacroBody::Token(token) => self.tokens.push(token.clone()),
                MacroBody::Var(var) => {
                    let var = repeat.vars.get(var).expect("Variable not found");
                    self.tokens.push(var.clone());
                }
                MacroBody::Repeat(body) => {
                    let macro_repeat = if let Some(repeat) = repeats.next() {
                        repeat
                    } else {
                        repeats = repeat.repeats.iter();
                        repeats.next().expect("No more repeats")
                    };
                    for repeat in macro_repeat {
                        self.insert_macro_body(body, &repeat);
                    }
                }
            }
        }
    }

    fn process_macro_call(&mut self, name: String) {
        let mac = self.macros.swap_remove(
            self.macros
                .iter()
                .position(|m| m.name == name)
                .expect("Macro not found"),
        );
        self.consume_type(TokenType::Hash);
        self.consume_type(TokenType::LPar);

        let vars = self.process_macro_inner(&mac.args).unwrap();

        self.consume_type(TokenType::RPar);

        self.insert_macro_body(&mac.body, &vars);

        self.macros.push(mac);
    }

    pub fn preprocess(&mut self) {
        while let Some(token) = self.tokens.next() {
            match token.t_type {
                TokenType::Mac => self.process_macro_def(),
                _ => {}
            }
        }

        self.tokens.reset();

        while let Some(token) = self.tokens.next() {
            match token.t_type {
                TokenType::Var(name) => {
                    if self.peek_type(&TokenType::Hash) {
                        self.process_macro_call(name);
                    }
                }
                _ => {}
            }
        }

        // TODO make sure we can have macros inside macros
    }

    fn peek(&self) -> Option<&Token> {
        return self.tokens.peek();
    }

    fn peek_type(&self, t_type: &TokenType) -> bool {
        return match self.peek() {
            Some(token) => &token.t_type == t_type,
            None => false,
        };
    }

    fn consume(&mut self) -> Token {
        return self.tokens.remove();
    }

    fn consume_type(&mut self, t_type: TokenType) -> Token {
        let token = self.consume();
        if token.t_type != t_type {
            panic!("Expected token of type {}, got {}", t_type, token.t_type);
        }
        return token;
    }
}
