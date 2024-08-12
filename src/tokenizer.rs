use core::fmt;
use std::{iter::Peekable, str::Chars};

use crate::tokenlist::TokenList;

#[derive(Clone)]
pub struct Token {
    pub t_type: TokenType,
    pub line: usize,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // Keywords
    Ret,
    Exit,
    Decl,
    If,
    Func,
    For,
    Mac,
    Use,

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
    At,
    Amp,
    Hash,
    Dot,
    Lt,
    Gt,
    QMark,

    // Literals
    Int(String),
    Asm(String),
    Path(String),

    // Identifiers
    Var(String),

    // End of file
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        let str = match self {
            Ret => "return",
            Exit => "exit",
            Decl => "decl",
            If => "if",
            Func => "func",
            For => "for",
            Mac => "mac",
            Use => "use",
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
            At => "@",
            Amp => "&",
            Hash => "#",
            Dot => ".",
            Lt => "<",
            Gt => ">",
            QMark => "?",
            Int(val) => val,
            Asm(val) => val,
            Path(val) => val,
            Var(val) => val,
            Eof => "end of file",
        };
        write!(f, "{}", str)
    }
}

pub struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    pub tokens: TokenList,
}

impl<'a> Tokenizer<'a> {
    pub fn new(text: &'a String) -> Tokenizer<'a> {
        return Tokenizer {
            chars: text.chars().peekable(),
            line: 1,
            tokens: TokenList::new(),
        };
    }

    fn tokenize_word(&mut self) {
        use TokenType::*;
        let mut token = String::new();
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                        self.next();
                        token.push(ch);
                    }
                    _ => break,
                },
                None => break,
            }
        }
        match token.as_str() {
            "return" => self.push_token(Ret),
            "exit" => self.push_token(Exit),
            "decl" => self.push_token(Decl),
            "if" => self.push_token(If),
            "func" => self.push_token(Func),
            "for" => self.push_token(For),
            "mac" => self.push_token(Mac),
            "use" => self.push_token(Use),
            _ => self.push_token(Var(token)),
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
        self.push_token(Int(val));
    }

    fn tokenize_single_or_double(&mut self, ch: char, single: TokenType, double: TokenType) {
        self.next();
        if self.peek() == Some(ch) {
            self.next();
            self.push_token(double);
        } else {
            self.push_token(single);
        }
    }

    fn tokenize_double(&mut self, ch: char, double: TokenType) {
        self.next();
        if self.peek() == Some(ch) {
            self.next();
            self.push_token(double);
        } else {
            panic!("Unexpected '{}' at line {}", ch, self.line);
        }
    }

    fn tokenize_comment(&mut self) {
        loop {
            match self.peek() {
                Some('\n') | Some('\r') | None => break,
                Some(_) => self.next(),
            }
        }
    }

    pub fn tokenize(&mut self) {
        use TokenType::*;
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    '>' => self.push_token(Gt),
                    '.' => self.push_token(Dot),
                    '{' => self.push_token(LBr),
                    '}' => self.push_token(RBr),
                    '%' => self.push_token(Per),
                    ';' => self.push_token(Semi),
                    '#' => self.push_token(Hash),
                    '*' => self.push_token(Star),
                    '+' => self.push_token(Plus),
                    '-' => self.push_token(Dash),
                    '(' => self.push_token(LPar),
                    ')' => self.push_token(RPar),
                    '?' => self.push_token(QMark),
                    '|' => self.tokenize_double('|', DPipe),
                    '=' => self.tokenize_single_or_double('=', Eq, DEq),
                    '&' => self.tokenize_single_or_double('&', Amp, DAmp),
                    '/' => {
                        self.next();
                        match self.peek() {
                            Some('/') => self.tokenize_comment(),
                            _ => self.push_token(Slash),
                        }
                    }
                    '!' => {
                        // TODO this is horrible
                        self.push_token(Int(String::from("0")));
                        self.push_token(Ex);
                    }
                    '@' => {
                        // TODO this is horrible
                        self.push_token(Int(String::from("0")));
                        self.push_token(At);
                    }
                    '<' => {
                        if self.tokens.peek().unwrap().t_type == Use {
                            self.next();
                            let mut path = String::new();
                            loop {
                                match self.peek() {
                                    Some('>') => {
                                        self.next();
                                        break;
                                    }
                                    Some(ch) => {
                                        if ch == '\n' || ch == '\r' {
                                            self.line += 1;
                                        }
                                        path.push(ch);
                                        self.next();
                                    }
                                    None => panic!("Unexpected EOF at line {}", self.line),
                                }
                            }
                            self.push_token(Path(path));
                        } else {
                            self.push_token(Lt);
                        }
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
                        self.push_token(Asm(asm));
                    }
                    '"' => {
                        self.next();
                        let mut chars = Vec::new();
                        loop {
                            match self.peek() {
                                Some('\\') => {
                                    self.next();
                                    if self.peek() == Some('"') {
                                        self.next();
                                        chars.push('"');
                                    } else {
                                        chars.push('\\');
                                    }
                                }
                                Some('"') => {
                                    self.next();
                                    break;
                                }
                                Some(ch) => {
                                    if ch == '\n' || ch == '\r' {
                                        self.line += 1;
                                    }
                                    chars.push(ch);
                                    self.next();
                                }
                                None => panic!("Unexpected EOF at line {}", self.line),
                            }
                        }
                        let mut asm = format!(
                            "    \
    mov rax, 9
    mov rsi, {len1}
    mov rdx, 3
    mov r10, 33
    mov r8, 255
    mov r9, 0
    syscall
    mov QWORD [rax], {len2}\n",
                            len1 = chars.len() + 9,
                            len2 = chars.len()
                        );
                        for i in 0..chars.len() {
                            asm.push_str(&format!(
                                "    mov byte [rax + {offset}], {ch}\n",
                                offset = i + 8,
                                ch = chars[i] as u8
                            ));
                        }
                        asm.push_str(&format!(
                            "    mov byte [rax + {offset}], 0\n",
                            offset = chars.len() + 8
                        ));
                        self.push_token(Asm(asm));
                    }
                    ' ' => self.next(),
                    '\n' | '\r' => {
                        self.next();
                        self.line += 1;
                    }
                    'a'..='z' | 'A'..='Z' | '_' => self.tokenize_word(),
                    '0'..='9' => self.tokenize_num(),
                    _ => panic!("Unexpected '{}' at line {}", ch, self.line),
                },
                None => {
                    self.push_token(Eof);
                    break;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        return self.chars.peek().copied();
    }

    fn next(&mut self) {
        self.chars.next().unwrap();
    }

    fn push_token(&mut self, t_type: TokenType) {
        self.tokens.push(Token {
            t_type,
            line: self.line,
        });
    }
}
