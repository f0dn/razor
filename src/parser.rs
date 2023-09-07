use std::collections::HashMap;

use crate::tokenizer::*;

pub struct BinOp {
    pub op: TokenType,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub enum Expr {
    ExprInt(String),
    ExprId(String),
    ExprBinOp(BinOp),
}

pub struct StmtDecl {
    pub var: String,
    pub expr: Expr,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub enum Stmt {
    StmtRet(StmtRet),
    StmtDecl(StmtDecl),
}

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: i32,
    op: HashMap<TokenType, i8>,
    pub parse_tree: Prog,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        use TokenType::*;
        return Parser {
            tokens,
            pos: 0,
            op: HashMap::from([
                (Star, 2),
                (Slash, 2),
                (Plus, 1),
                (Dash, 1),
            ]),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
        use TokenType::*;
        let tk = self.peek().expect("Not a valid expression");
        match tk.t_type {
            Int => {
                let int = tk.val.clone().unwrap();
                self.next();
                return ExprInt(int);
            }
            Var => {
                let var = tk.val.clone().unwrap();
                self.next();
                return ExprId(var);
            }
            LPar => {
                self.next();
                let expr = self.parse_expr(1);
                match self.peek().expect("Not a valid expression").t_type {
                    RPar => {
                        self.next();
                        return expr;
                    }
                    _ => panic!("Missing ')'"),
                }
            }
            _ => panic!("Not a valid expression"),
        }
    }


    fn parse_expr(&mut self, min_prec: i8) -> Expr {
        let mut lhs = self.parse_atom();

        loop {
            let tk = self.peek().expect("Missing ';'");
            let prec = self.op.get(&tk.t_type).copied();
            if prec.is_none() || prec.unwrap() < min_prec {
                break;
            }

            let op = tk.t_type.clone();
            self.next();
            let rhs = self.parse_expr(prec.unwrap() + 1);
            lhs = Expr::ExprBinOp(BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }
        return lhs;
    }

    fn parse_ret(&mut self) -> StmtRet {
        use TokenType::*;
        self.next();
        let expr = self.parse_expr(1);
        match self.peek().expect("Not a valid return").t_type {
            Semi => {
                self.next();
                return StmtRet { expr };
            }
            _ => panic!("Not a valid return"),
        }
    }

    pub fn parse_decl(&mut self) -> StmtDecl {
        use TokenType::*;
        self.next();

        let tk = self.peek().expect("Not a valid declaration");
        let var = match tk.t_type {
            Var => tk.val.clone().unwrap(),
            _ => panic!("Not a valid declaration"),
        };
        self.next();

        let expr = match self.peek().expect("Not a valid declaration").t_type {
            Eq => {
                self.next();
                self.parse_expr(1)
            }
            _ => panic!("Not a valid declaration"),
        };

        match self.peek().expect("Not a valid declaration").t_type {
            Semi => {
                self.next();
                return StmtDecl { var, expr };
            }
            _ => panic!("Not a valid declaration"),
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
                    Decl => {
                        let decl = self.parse_decl();
                        self.parse_tree.stmts.push(StmtDecl(decl));
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

    fn next(&mut self) {
        self.pos += 1;
    }
}
