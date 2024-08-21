use std::collections::HashMap;

use crate::{
    tokenizer::{Token, TokenType},
    tokenlist::TokenList,
};

pub struct MacroUse {
    pub id: String,
    pub path: String,
}

pub struct Macro {
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
    tokens: TokenList,
    num_tokens_added: usize,
    pub macros: HashMap<String, Macro>,
}

impl Preproc {
    pub fn new(mut tokens: TokenList) -> Preproc {
        tokens.reset();
        Preproc {
            tokens,
            num_tokens_added: 0,
            macros: HashMap::new(),
        }
    }

    fn process_macro_args(&mut self) -> Vec<MacroArg> {
        let mut args = Vec::new();

        while !self.peek_type(&TokenType::RPar) {
            let arg = match self.consume().t_type {
                TokenType::LPar => {
                    let args = self.process_macro_args();
                    self.consume_type(TokenType::RPar);
                    let repeat = match self.consume().t_type {
                        TokenType::QMark => RepeatType::ZeroOrOne,
                        TokenType::Star => RepeatType::ZeroOrMore,
                        TokenType::Plus => RepeatType::OneOrMore,
                        _ => panic!("Expected repeat token"),
                    };
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
                TokenType::RBr => {
                    panic!("{} should be escaped", TokenType::RBr);
                }
                TokenType::Hash => {
                    let token = self.consume();
                    match token.t_type {
                        TokenType::LBr | TokenType::LPar | TokenType::RPar | TokenType::RBr => {
                            MacroArg::Token(token.t_type)
                        }
                        _ => panic!("No need to escape {}", token.t_type),
                    }
                }
                token => MacroArg::Token(token),
            };
            args.push(arg);
        }

        args
    }

    fn process_macro_body(&mut self, body: &mut Vec<MacroBody>) {
        // TODO this function is pretty ugly
        loop {
            match self.consume() {
                token @ Token {
                    t_type: TokenType::Var(_),
                    line: _,
                } => {
                    if self.peek_type(&TokenType::Hash) {
                        match token.t_type {
                            TokenType::Var(var) => {
                                if let Some(mac) = self.macros.remove(&var) {
                                    self.num_tokens_added = 0;
                                    self.process_macro_call(&mac.body, &mac.args);
                                    self.macros.insert(var.clone(), mac);
                                    self.tokens.back(self.num_tokens_added);
                                }
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        body.push(MacroBody::Token(token));
                    }
                }
                token @ Token {
                    t_type: TokenType::LPar,
                    line: _,
                } => {
                    if self.peek_type(&TokenType::Hash) {
                        self.consume();
                        let mut repeat_body = Vec::new();
                        self.process_macro_body(&mut repeat_body);
                        body.push(MacroBody::Repeat(repeat_body));
                    } else {
                        body.push(MacroBody::Token(token));
                    }
                }
                hash @ Token {
                    t_type: TokenType::Hash,
                    line: _,
                } => match &self.peek().unwrap().t_type {
                    TokenType::RPar => {
                        self.consume();
                        break;
                    }
                    TokenType::RBr => {
                        body.push(MacroBody::Token(self.consume()));
                        break;
                    }
                    TokenType::Var(var) => {
                        body.push(MacroBody::Var(var.to_string()));
                        self.consume();
                    }
                    TokenType::LPar => {
                        body.push(MacroBody::Token(hash));
                        body.push(MacroBody::Token(self.consume()));
                    }
                    t_type => panic!("Unexpected {} after {}", t_type, TokenType::Hash),
                },
                token => body.push(MacroBody::Token(token)),
            };
        }
    }

    fn process_macro_def(&mut self) {
        let name = match self.consume().t_type {
            TokenType::Var(id) => id,
            _ => panic!("Expected identifier"),
        };

        self.consume_type(TokenType::LPar);

        let args = self.process_macro_args();

        self.consume_type(TokenType::RPar);

        let mut body = vec![MacroBody::Token(self.consume_type(TokenType::LBr))];

        self.process_macro_body(&mut body);

        self.macros.insert(name, Macro { args, body });
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
                    while let Some(inner_vars) = self.process_macro_inner(args) {
                        repeat.push(inner_vars);
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

        Some(MacroRepeat { vars, repeats })
    }

    fn insert_macro_body(&mut self, body: &Vec<MacroBody>, repeat: &MacroRepeat) {
        let mut repeats = repeat.repeats.iter();
        for token in body {
            match token {
                MacroBody::Token(token) => {
                    self.tokens.push(token.clone());
                    self.num_tokens_added += 1;
                }
                MacroBody::Var(var) => {
                    let var = repeat.vars.get(var).expect("Variable not found");
                    self.tokens.push(var.clone());
                    self.num_tokens_added += 1;
                }
                MacroBody::Repeat(body) => {
                    let macro_repeat = if let Some(repeat) = repeats.next() {
                        repeat
                    } else {
                        repeats = repeat.repeats.iter();
                        repeats.next().expect("No more repeats")
                    };
                    for repeat in macro_repeat {
                        self.insert_macro_body(body, repeat);
                    }
                }
            }
        }
    }

    fn process_macro_call_with_extra(
        &mut self,
        name: String,
        extra_macros: &HashMap<&String, &Macro>,
    ) {
        if let Some(mac) = self.macros.remove(&name) {
            self.tokens.remove_back();
            self.process_macro_call(&mac.body, &mac.args);
            self.macros.insert(name, mac);
        } else if let Some(mac) = extra_macros.get(&name) {
            self.tokens.remove_back();
            self.process_macro_call(&mac.body, &mac.args);
        }
    }

    fn process_macro_call(&mut self, body: &Vec<MacroBody>, args: &Vec<MacroArg>) {
        self.consume_type(TokenType::Hash);
        self.consume_type(TokenType::LPar);

        let vars = self.process_macro_inner(args).unwrap();

        self.consume_type(TokenType::RPar);

        self.insert_macro_body(body, &vars);
    }

    fn process_macro_use(&mut self) -> MacroUse {
        self.consume_type(TokenType::Use);

        let path = match self.consume().t_type {
            TokenType::Path(path) => path,
            token => panic!("Expected path, got {}", token),
        };

        self.consume_type(TokenType::Dot);

        let id = match self.consume().t_type {
            TokenType::Var(id) => id,
            _ => panic!("Expected identifier"),
        };

        self.consume_type(TokenType::Semi);

        MacroUse { id, path }
    }

    pub fn preprocess_uses(&mut self) -> Vec<MacroUse> {
        let mut uses = Vec::new();

        while let Some(token) = self.tokens.next() {
            if let TokenType::Hash = token.t_type {
                if self.peek_type(&TokenType::Use) {
                    self.tokens.remove_back();
                    uses.push(self.process_macro_use());
                }
            }
        }

        self.tokens.reset();

        uses
    }

    pub fn preprocess_macros(&mut self) {
        while let Some(token) = self.tokens.next() {
            if let TokenType::Mac = token.t_type {
                self.tokens.remove_back();
                self.process_macro_def();
            }
        }

        self.tokens.reset();
    }

    pub fn preprocess_macro_calls(&mut self, macros: &HashMap<&String, &Macro>) {
        while let Some(token) = self.tokens.next() {
            if let TokenType::Var(name) = token.t_type {
                if self.peek_type(&TokenType::Hash) {
                    self.process_macro_call_with_extra(name, macros);
                }
            }
        }

        self.tokens.reset();

        // TODO make sure we can have macros inside macros
    }

    pub fn take_tokens(&mut self) -> TokenList {
        std::mem::replace(&mut self.tokens, TokenList::new())
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
        self.tokens.remove()
    }

    fn consume_type(&mut self, t_type: TokenType) -> Token {
        let token = self.consume();
        if token.t_type != t_type {
            panic!("Expected token of type {}, got {}", t_type, token.t_type);
        }
        token
    }
}
