use std::collections::LinkedList;

use crate::tokenizer::Token;

pub struct TokenList {
    first: LinkedList<Token>,
    second: LinkedList<Token>,
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

    pub fn peek(&self) -> Option<&Token> {
        match &self.second.front() {
            Some(token) => Some(&token),
            None => None,
        }
    }

    pub fn peek_mult(&self, n: usize) -> Option<&Token> {
        return self.second.iter().nth(n);
    }

    pub fn push(&mut self, token: Token) {
        self.second.push_back(token);
    }

    pub fn remove(&mut self) -> Token {
        return self.second.pop_front().unwrap();
    }

    pub fn reset(&mut self) {
        while let Some(token) = self.second.pop_front() {
            self.first.push_back(token);
        }
        std::mem::swap(&mut self.first, &mut self.second);
    }
}
