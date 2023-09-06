use crate::tokenizer::*;

pub struct Expr {
    pub int: String,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub enum Stmt {
    StmtRet(StmtRet),
}

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: i32,
    pub parse_tree: Prog,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens,
            pos: 0,
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_expr(&mut self) -> Expr {
        use TokenType::*;
        match self.peek() {
            Some(tk) => match tk.t_type {
                Int => {
                    let int = tk.val.clone().expect("Int with no value");
                    self.next();
                    return Expr { int };
                }
                _ => panic!("Not a valid expression"),
            },
            None => panic!("Not a valid expression"),
        }
    }

    fn parse_ret(&mut self) -> StmtRet {
        use TokenType::*;
        self.next();
        let expr = self.parse_expr();
        match self.peek() {
            Some(tk) => match tk.t_type {
                Semi => {
                    self.next();
                    return StmtRet { expr };
                }
                _ => panic!("Not a valid return"),
            },
            None => panic!("Not a valid return"),
        }
    }

    pub fn parse(&mut self) {
        use Stmt::*;
        use TokenType::*;
        loop {
            match self.peek() {
                Some(tk) => match tk.t_type {
                    Ret => {
                        let ret = self.parse_ret();
                        self.parse_tree.stmts.push(StmtRet(ret));
                    }
                    Semi => {
                        self.next();
                        continue;
                    }
                    _ => panic!("Not a valid statement"),
                },
                None => break,
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        return self.tokens.get(self.pos as usize);
    }

    fn peek_mult(&self, n: i32) -> Option<&Token> {
        return self.tokens.get((self.pos - 1 + n) as usize);
    }

    fn next(&mut self) {
        self.pos += 1;
    }
}
