use core::fmt;
use std::{hash::Hash, iter::Peekable, str::Chars};

pub mod tokenlist;

use crate::tokenizer::tokenlist::TokenList;

#[derive(Clone)]
pub struct Token {
    pub t_type: TokenType,
    pub line: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t_type)
    }
}

#[derive(Clone)]
pub enum TokenType {
    // Keywords
    Ret,
    Exit,
    Decl,
    If,
    Else,
    Func,
    For,
    Mac,
    Use,
    Const,
    Size,

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
    Comma,

    // Literals
    Int(String),
    Char(char),
    Str(String),
    Asm(String),

    // Identifiers
    Var(String),

    // End of file
    Eof,
}

impl Hash for TokenType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl Eq for TokenType {}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        write!(
            f,
            "{}",
            match self {
                Ret => "return",
                Exit => "exit",
                Decl => "decl",
                If => "if",
                Else => "else",
                Func => "func",
                For => "for",
                Mac => "mac",
                Use => "use",
                Const => "const",
                Size => "size",
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
                Comma => ",",
                Int(val) => val,
                Char(val) => return write!(f, "'{}'", val),
                Str(val) => return write!(f, "\"{}\"", val),
                Asm(val) => return write!(f, "`{}`", val),
                Var(val) => val,
                Eof => "end of file",
            }
        )
    }
}

pub struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    pub tokens: TokenList,
}

impl<'a> Tokenizer<'a> {
    pub fn new(text: &'a str) -> Tokenizer<'a> {
        return Tokenizer {
            chars: text.chars().peekable(),
            line: 1,
            tokens: TokenList::new(),
        };
    }

    fn tokenize_word(&mut self) {
        use TokenType::*;
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    self.next();
                    token.push(ch);
                }
                _ => break,
            }
        }
        match token.as_str() {
            "return" => self.push_token(Ret),
            "exit" => self.push_token(Exit),
            "decl" => self.push_token(Decl),
            "if" => self.push_token(If),
            "else" => self.push_token(Else),
            "func" => self.push_token(Func),
            "for" => self.push_token(For),
            "mac" => self.push_token(Mac),
            "use" => self.push_token(Use),
            "const" => self.push_token(Const),
            "size" => self.push_token(Size),
            _ => self.push_token(Var(token)),
        }
    }

    fn tokenize_num(&mut self) {
        use TokenType::*;
        let mut val = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    self.next();
                    val.push(ch);
                }
                _ => break,
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

    fn tokenize_char(&mut self) {
        self.next();
        match self.peek() {
            Some(ch) => {
                if ch == '\n' || ch == '\r' {
                    self.line += 1;
                }
                self.push_and_next(TokenType::Char(ch))
            }
            None => panic!("Unexpected EOF at line {}", self.line),
        };
        if self.peek() != Some('\'') {
            panic!("Expected ' at line {}", self.line);
        }
        self.next();
    }

    fn tokenize_string(&mut self) {
        self.next();
        let mut string = String::new();
        loop {
            match self.peek() {
                Some('\\') => {
                    self.next();
                    if self.peek() == Some('"') {
                        self.next();
                        string.push('"');
                    } else {
                        string.push('\\');
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
                    string.push(ch);
                    self.next();
                }
                None => panic!("Unexpected EOF at line {}", self.line),
            }
        }
        self.push_token(TokenType::Str(string));
    }

    fn tokenize_asm(&mut self) {
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
        self.push_token(TokenType::Asm(asm));
    }

    pub fn tokenize(&mut self) {
        use TokenType::*;
        loop {
            match self.peek() {
                Some(ch) => match ch {
                    '>' => self.push_and_next(Gt),
                    '<' => self.push_and_next(Lt),
                    '.' => self.push_and_next(Dot),
                    '{' => self.push_and_next(LBr),
                    '}' => self.push_and_next(RBr),
                    '%' => self.push_and_next(Per),
                    ';' => self.push_and_next(Semi),
                    '#' => self.push_and_next(Hash),
                    '*' => self.push_and_next(Star),
                    '+' => self.push_and_next(Plus),
                    '-' => self.push_and_next(Dash),
                    '(' => self.push_and_next(LPar),
                    ')' => self.push_and_next(RPar),
                    '?' => self.push_and_next(QMark),
                    ',' => self.push_and_next(Comma),
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
                        self.push_and_next(Ex);
                    }
                    '@' => {
                        // TODO this is horrible
                        self.push_token(Int(String::from("0")));
                        self.push_and_next(At);
                    }
                    '\'' => self.tokenize_char(),
                    '`' => self.tokenize_asm(),
                    '"' => self.tokenize_string(),
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

    fn push_and_next(&mut self, t_type: TokenType) {
        self.push_token(t_type);
        self.next();
    }

    fn push_token(&mut self, t_type: TokenType) {
        self.tokens.push(Token {
            t_type,
            line: self.line,
        });
    }
}
