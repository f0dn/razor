use crate::ERROR_SIZE;
use std::{collections::LinkedList, fmt::Debug};

use crate::tokenizer::{Token, TokenType};

pub struct TokenList {
    first: LinkedList<Token>,
    second: LinkedList<Token>,
}

impl Debug for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut last_line = 0;
        let mut indent = 0;

        for token in self
            .first
            .iter()
            .chain(self.second.iter().rev().skip(1).rev())
        {
            if token.line != last_line {
                write!(f, "\n{:4}: ", token.line)?;
                for _ in 0..indent {
                    write!(f, "    ")?;
                }
                last_line = token.line;
            }

            if token.t_type == TokenType::RBr {
                indent -= 1;
            } else if token.t_type == TokenType::LBr {
                indent += 1;
            }
            write!(f, "{:?} ", token)?;
        }

        Ok(())
    }
}

impl Iterator for TokenList {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.second.pop_front() {
            Some(token) => {
                self.first.push_back(token.clone());
                Some(token)
            }
            None => None,
        }
    }
}

impl TokenList {
    pub fn new() -> TokenList {
        TokenList {
            first: LinkedList::new(),
            second: LinkedList::new(),
        }
    }

    // TODO this is pretty ugly
    pub fn error_context(&mut self) -> String {
        let mut new_first = LinkedList::new();
        let mut num_lines = 0;
        let mut last_line = 0;
        while num_lines < ERROR_SIZE {
            if let Some(token) = self.first.pop_back() {
                if token.line != last_line {
                    num_lines += 1;
                    last_line = token.line;
                }
                new_first.push_front(token);
            } else {
                new_first.push_front(Token {
                    t_type: TokenType::Eof,
                    line: 0,
                });
                break;
            }
        }
        new_first.pop_front();

        let mut new_second = LinkedList::new();
        let mut num_lines = 0;
        let mut last_line = 0;
        while num_lines < ERROR_SIZE {
            if let Some(token) = self.second.pop_front() {
                if token.line != last_line {
                    num_lines += 1;
                    last_line = token.line;
                }
                new_second.push_back(token);
            } else {
                break;
            }
        }
        self.first = new_first;
        self.second = new_second;
        format!("{:?}", self)
    }

    pub fn back(&mut self, n: usize) {
        for _ in 0..n {
            if let Some(token) = self.first.pop_back() {
                self.second.push_front(token);
            }
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        return self.second.front();
    }

    pub fn peek_back(&self) -> Option<&Token> {
        return self.first.back();
    }

    pub fn peek_mult(&self, n: usize) -> Option<&Token> {
        return self.second.iter().nth(n - 1);
    }

    pub fn push(&mut self, token: Token) {
        self.first.push_back(token);
    }

    pub fn push_front(&mut self, token: Token) {
        self.second.push_front(token);
    }

    pub fn remove(&mut self) -> Token {
        self.second.pop_front().unwrap()
    }

    pub fn remove_back(&mut self) -> Token {
        self.first.pop_back().unwrap()
    }

    pub fn reset(&mut self) {
        while let Some(token) = self.second.pop_front() {
            self.first.push_back(token);
        }
        std::mem::swap(&mut self.first, &mut self.second);
    }
}
