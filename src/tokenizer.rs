pub struct Token {
    pub t_type: TokenType,
    pub val: Option<String>,
    pub line: i32,
}

#[derive(PartialEq, Eq, Hash)]
pub enum TokenType {
    // Keywords
    Ret,
    Decl,
    If,

    // Symbols
    Semi,
    Eq,
    Star,
    Plus,
    Dash,
    Slash,
    LPar,
    RPar,
    LBr,
    RBr,

    // Literals
    Int,

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
            Decl => "decl",
            If => "if",
            Semi => ";",
            Eq => "=",
            Star => "*",
            Plus => "+",
            Dash => "-",
            Slash => "/",
            LPar => "(",
            RPar => ")",
            LBr => "{",
            RBr => "}",
            Int => "int",
            Var => "var",
            Eof => "EOF",
        }
    }
}

pub struct Tokenizer {
    text: String,
    pos: i32,
    line: i32,
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
            "decl" => self.push_sym(Decl),
            "if" => self.push_sym(If),
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
                    '=' => self.push_sym(Eq),
                    '*' => self.push_sym(Star),
                    '+' => self.push_sym(Plus),
                    '-' => self.push_sym(Dash),
                    '/' => self.push_sym(Slash),
                    '(' => self.push_sym(LPar),
                    ')' => self.push_sym(RPar),
                    '{' => self.push_sym(LBr),
                    '}' => self.push_sym(RBr),
                    ' ' => self.next(),
                    '\n' | '\r' => {
                        self.next();
                        self.line += 1;
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
