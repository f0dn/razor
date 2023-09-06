pub struct Token {
    pub t_type: TokenType,
    pub val: Option<String>,
}

pub enum TokenType {
    Ret,
    Int,
    Semi,
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
            _ => panic!("Not a valid word"),
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
                    ';' => {
                        self.tokens.push(Token {
                            t_type: Semi,
                            val: None,
                        });
                        self.next();
                    }
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
}
