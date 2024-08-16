use std::{collections::LinkedList, fmt::Debug};

use crate::tokenizer::Token;

pub struct TokenList {
    first: LinkedList<Token>,
    second: LinkedList<Token>,
}

impl Debug for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut last_line = 0;
        for token in self.first.iter() {
            if token.line != last_line {
                write!(f, "\n")?;
                last_line = token.line;
            }
            write!(f, "{:?} ", token)?;
        }

        for token in self.second.iter() {
            if token.line != last_line {
                write!(f, "\n")?;
                last_line = token.line;
            }
            write!(f, "{:?} ", token)?;
        }

        return Ok(());
    }
}

impl Iterator for TokenList {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.second.pop_front() {
            Some(token) => {
                self.first.push_back(token.clone());
                return Some(token);
            }
            None => None,
        }
    }
}

impl TokenList {
    pub fn new() -> TokenList {
        return TokenList {
            first: LinkedList::new(),
            second: LinkedList::new(),
        };
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
        return self.second.pop_front().unwrap();
    }

    pub fn remove_back(&mut self) -> Token {
        return self.first.pop_back().unwrap();
    }

    pub fn reset(&mut self) {
        while let Some(token) = self.second.pop_front() {
            self.first.push_back(token);
        }
        std::mem::swap(&mut self.first, &mut self.second);
    }
}
