pub struct Token {
    pub t_type: TokenType,
    pub val: Option<String>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // Keywords
    Ret,
    Decl,

    // Symbols
    Semi,
    Eq,
    Star,
    Plus,
    Dash,
    Slash,
    LPar,
    RPar,

    // Literals
    Int,

    // Identifiers
    Var,
}

pub struct Tokenizer {
    text: String,
    pos: i32,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(text: String) -> Tokenizer {
        return Tokenizer {
            text,
            pos: 0,
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
            "return" => self.tokens.push(Token {
                t_type: Ret,
                val: None,
            }),
            "decl" => self.tokens.push(Token {
                t_type: Decl,
                val: None,
            }),
            _ => self.tokens.push(Token {
                t_type: Var,
                val: Some(token),
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
                    ' ' | '\n' | '\r' => self.next(),
                    'a'..='z' | 'A'..='Z' => self.tokenize_word(),
                    '0'..='9' => self.tokenize_num(),
                    _ => panic!("Not a valid token {:?}", ch),
                },
                None => break,
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
        });
        self.next();
    }
}
