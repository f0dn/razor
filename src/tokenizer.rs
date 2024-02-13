pub struct Token {
    pub t_type: TokenType,
    pub val: Option<String>,
    pub line: usize,
}

#[derive(PartialEq, Eq, Hash)]
pub enum TokenType {
    // Keywords
    Ret,
    Exit,
    Decl,
    If,
    Func,
    For,

    // Symbols
    Semi,
    Eq,
    DEq,
    DPipe,
    DAmp,
    Star,
    Plus,
    Dash,
    Slash,
    Per,
    Ex,
    LPar,
    RPar,
    LBr,
    RBr,

    // Literals
    Int,
    Asm,

    // Identifiers
    Var,

    // End of file
    Eof,
}

impl TokenType {
    pub fn val(&self) -> &str {
        use TokenType::*;
        match self {
            Ret => "return",
            Exit => "exit",
            Decl => "decl",
            If => "if",
            Func => "func",
            For => "for",
            Semi => ";",
            Eq => "=",
            DEq => "==",
            DPipe => "||",
            DAmp => "&&",
            Star => "*",
            Plus => "+",
            Dash => "-",
            Slash => "/",
            Per => "%",
            Ex => "!",
            LPar => "(",
            RPar => ")",
            LBr => "{",
            RBr => "}",
            Int => "int",
            Asm => "asm",
            Var => "variable",
            Eof => "EOF",
        }
    }
}

pub struct Tokenizer {
    text: String,
    pos: u32,
    line: usize,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(text: String) -> Tokenizer {
        return Tokenizer {
            text,
            pos: 0,
            line: 1,
            tokens: Vec::new(),
        };
    }

    fn tokenize_word(&mut self) {
        use TokenType::*;
        let mut token = String::new();
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                        self.next();
                        token.push(ch);
                    }
                    _ => break,
                },
                None => break,
            }
        }
        match token.as_str() {
            "return" => self.push_sym(Ret),
            "exit" => self.push_sym(Exit),
            "decl" => self.push_sym(Decl),
            "if" => self.push_sym(If),
            "func" => self.push_sym(Func),
            "for" => self.push_sym(For),
            _ => self.tokens.push(Token {
                t_type: Var,
                val: Some(token),
                line: self.line,
            }),
        }
    }

    fn tokenize_num(&mut self) {
        use TokenType::*;
        let mut val = String::new();
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    '0'..='9' => {
                        self.next();
                        val.push(ch);
                    }
                    _ => break,
                },
                None => break,
            }
        }
        self.tokens.push(Token {
            t_type: Int,
            val: Some(val),
            line: self.line,
        });
    }

    pub fn tokenize(&mut self) {
        use TokenType::*;
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    ';' => self.push_sym(Semi),
                    '=' => {
                        self.next();
                        match self.peek() {
                            Some('=') => self.push_sym(DEq),
                            _ => self.push_sym(Eq),
                        }
                    }
                    '|' => {
                        self.next();
                        match self.peek() {
                            Some('|') => self.push_sym(DPipe),
                            _ => panic!("Unexpected '|' at line {}", self.line),
                        }
                    }
                    '&' => {
                        self.next();
                        match self.peek() {
                            Some('&') => self.push_sym(DAmp),
                            _ => panic!("Unexpected '&' at line {}", self.line),
                        }
                    }
                    '*' => self.push_sym(Star),
                    '+' => self.push_sym(Plus),
                    '-' => self.push_sym(Dash),
                    '/' => {
                        self.next();
                        match self.peek() {
                            Some('/') => loop {
                                match self.peek() {
                                    Some('\n') | Some('\r') => break,
                                    Some(_) => self.next(),
                                    None => break,
                                }
                            }
                            _ => self.push_sym(Slash),
                        }
                    }
                    '%' => self.push_sym(Per),
                    '!' => {
                        self.tokens.push(Token {
                            t_type: Int,
                            val: Some(String::from("0")),
                            line: self.line,
                        });
                        self.push_sym(Ex);
                    }
                    '(' => self.push_sym(LPar),
                    ')' => self.push_sym(RPar),
                    '{' => self.push_sym(LBr),
                    '}' => self.push_sym(RBr),
                    ' ' => self.next(),
                    '\n' | '\r' => {
                        self.next();
                        self.line += 1;
                    }
                    '`' => {
                        self.next();
                        let mut asm = String::new();
                        loop {
                            match self.peek() {
                                Some('`') => {
                                    self.next();
                                    break;
                                }
                                Some(ch) => {
                                    if ch == '\n' || ch == '\r' {
                                        self.line += 1;
                                    }
                                    asm.push(ch);
                                    self.next();
                                }
                                None => panic!("Unexpected EOF at line {}", self.line),
                            }
                        }
                        self.tokens.push(Token {
                            t_type: Asm,
                            val: Some(asm),
                            line: self.line,
                        });
                    }
                    'a'..='z' | 'A'..='Z' => self.tokenize_word(),
                    '0'..='9' => self.tokenize_num(),
                    _ => panic!("Unexpected '{}' at line {}", ch, self.line),
                },
                None => {
                    self.push_sym(Eof);
                    break;
                }
            }
        }
    }

    fn peek(&self) -> Option<char> {
        return self.text.chars().nth(self.pos as usize);
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    fn push_sym(&mut self, t_type: TokenType) {
        self.tokens.push(Token {
            t_type,
            val: None,
            line: self.line,
        });
        self.next();
    }
}
