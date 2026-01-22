use std::collections::HashMap;

use crate::{
    path::UsePath,
    tokenizer::tokenlist::TokenList,
    tokenizer::{Token, TokenType},
};

#[derive(Clone)]
pub struct MacroUse {
    pub id: String,
    pub path: UsePath,
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
    preprocessed: bool,
    pub macros: HashMap<String, Macro>,
}

impl Preproc {
    pub fn new(mut tokens: TokenList) -> Preproc {
        tokens.reset();
        Preproc {
            tokens,
            num_tokens_added: 0,
            preprocessed: false,
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
                        err => panic!(
                            "Expected identifier, got {}\nPrevious token was {}",
                            err, token.t_type
                        ),
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
                            TokenType::Var(ref var) => {
                                if let Some(mac) = self.macros.remove(var) {
                                    self.num_tokens_added = 0;
                                    self.process_macro_call(&mac.body, &mac.args);
                                    self.macros.insert(var.clone(), mac);
                                    self.tokens.back(self.num_tokens_added);
                                } else {
                                    body.push(MacroBody::Token(token));
                                }
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        body.push(MacroBody::Token(token));
                    }
                }
                Token {
                    t_type: TokenType::Hash,
                    line: _,
                } => match &self.peek().unwrap().t_type {
                    TokenType::Hash => body.push(MacroBody::Token(self.consume())),
                    TokenType::LPar => {
                        self.consume();
                        let mut repeat_body = Vec::new();
                        self.process_macro_body(&mut repeat_body);
                        body.push(MacroBody::Repeat(repeat_body));
                    }
                    TokenType::RPar => {
                        self.consume();
                        break;
                    }
                    TokenType::RBr => {
                        self.consume();
                        break;
                    }
                    TokenType::Var(var) => {
                        body.push(MacroBody::Var(var.to_string()));
                        self.consume();
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

        self.consume_type(TokenType::LBr);

        let mut body = Vec::new();

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

    fn insert_macro_body(&mut self, body: &Vec<MacroBody>, repeat: &MacroRepeat, i: usize) {
        let mut repeats = repeat.repeats.iter();
        for token in body {
            match token {
                MacroBody::Token(token) => {
                    self.tokens.push(token.clone());
                    self.num_tokens_added += 1;
                }
                MacroBody::Var(var) => {
                    if let Some(num) = var.strip_prefix("len") {
                        let repeat_index = num
                            .parse::<usize>()
                            .expect("Expected number for length index");
                        let len = repeat.repeats[repeat_index].len();
                        self.tokens.push(Token {
                            t_type: TokenType::Int(len.to_string()),
                            line: 0,
                        });
                        self.num_tokens_added += 1;
                    } else if var == "index" {
                        self.tokens.push(Token {
                            t_type: TokenType::Int(i.to_string()),
                            line: 0,
                        });
                        self.num_tokens_added += 1;
                    } else {
                        let var = repeat
                            .vars
                            .get(var)
                            .unwrap_or_else(|| panic!("Variable {} not found", var));
                        self.tokens.push(var.clone());
                        self.num_tokens_added += 1;
                    }
                }
                MacroBody::Repeat(body) => {
                    let macro_repeat = if let Some(repeat) = repeats.next() {
                        repeat
                    } else {
                        repeats = repeat.repeats.iter();
                        repeats.next().expect("No more repeats")
                    };
                    for (i, repeat) in macro_repeat.iter().enumerate() {
                        self.insert_macro_body(body, repeat, i);
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

        self.insert_macro_body(body, &vars, 0);
    }

    fn process_macro_use(&mut self) -> MacroUse {
        self.consume_type(TokenType::Use);

        let mut path = UsePath::new();

        while let TokenType::Var(var) = self.consume().t_type {
            match self.peek().unwrap().t_type {
                TokenType::Dot => {
                    path.add(&var);
                    self.consume();
                }
                _ => {
                    let id = var;

                    self.consume_type(TokenType::Semi);

                    return MacroUse { id, path };
                }
            }
        }
        panic!("Expected identifier");
    }

    pub fn preprocess_uses(&mut self) -> Vec<MacroUse> {
        // TODO this shouldn't process uses inside macros
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

    fn preprocess_macros(&mut self) {
        // TODO this will process inner macros before evaluating outer macros
        while let Some(token) = self.tokens.next() {
            if let TokenType::Mac = token.t_type {
                self.tokens.remove_back();
                self.process_macro_def();
            }
        }

        self.tokens.reset();
    }

    fn preprocess_macro_calls(&mut self, macros: &HashMap<&String, &Macro>) {
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

    pub fn preprocess(&mut self, macros: &HashMap<&String, &Macro>) {
        if self.preprocessed {
            return;
        }
        // TODO this is a hack but needs more robust logic
        self.preprocess_macros();
        self.preprocess_macro_calls(macros);
        self.preprocess_macros();
        self.preprocessed = true;
        // after preprocessing all that is left is to take the tokens or use the macros
    }

    pub fn is_preprocessed(&self) -> bool {
        self.preprocessed
    }

    pub fn take_tokens(&mut self) -> TokenList {
        // TODO make sure this is only called once
        std::mem::replace(&mut self.tokens, TokenList::new())
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_type(&self, t_type: &TokenType) -> bool {
        match self.peek() {
            Some(token) => &token.t_type == t_type,
            None => false,
        }
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
