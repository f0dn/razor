use std::collections::HashMap;

use crate::tokenizer::*;
use crate::tokenizer::TokenType::*;

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

pub struct StmtIf {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
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
    StmtIf(StmtIf),
    StmtBlank,
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

macro_rules! parse_fn {
    ($name:ident -> $ret:ident {
        $($({$tk:pat}$( => $tk_field:ident)?)?,
          $($func:ident($($arg:expr)?) => $func_field:ident, )?
        )+}
    ) => {
        impl Parser {
            fn $name(&mut self) -> $ret {
                $(
                    $(
                        $(let $tk_field;)?
                        let tk = self.peek().expect("Not a valid if");
                        match &tk.t_type {
                            $tk => {
                                $(
                                    $tk_field = tk.val.clone().unwrap();
                                )?
                                self.next();
                            }
                            _ => panic!("Not a valid if"),
                        }
                    )?

                    $(
                        let $func_field = self.$func($($arg)?);
                    )?
                )+

                return $ret {
                    $(
                        $(
                            $(
                                $tk_field,
                            )?
                        )?
                        $(
                            $func_field,
                        )?
                    )+
                };
            }
        }
    };
}

parse_fn! {
    parse_if -> StmtIf {
        {If}, parse_expr() => expr, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_ret -> StmtRet {
        {Ret}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_decl -> StmtDecl {
        {Decl}, {Var} => var, {Eq}, parse_expr() => expr, {Semi},
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens,
            pos: 0,
            op: HashMap::from([(Star, 2), (Slash, 2), (Plus, 1), (Dash, 1)]),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
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
                let expr = self.parse_expr();
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

    fn parse_expr_prec(&mut self, min_prec: i8) -> Expr {
        let mut lhs = self.parse_atom();

        loop {
            let tk = self.peek().expect("Missing ';'");
            let prec = self.op.get(&tk.t_type).copied();
            if prec.is_none() || prec.unwrap() < min_prec {
                break;
            }

            let op = tk.t_type.clone();
            self.next();
            let rhs = self.parse_expr_prec(prec.unwrap() + 1);
            lhs = Expr::ExprBinOp(BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }
        return lhs;
    }

    fn parse_expr(&mut self) -> Expr {
        return self.parse_expr_prec(1);
    }

    fn parse_stmt(&mut self) -> Stmt {
        use Stmt::*;
        match self.peek().expect("Not a valid statement").t_type {
            Ret => return StmtRet(self.parse_ret()),
            Decl => return StmtDecl(self.parse_decl()),
            If => return StmtIf(self.parse_if()),
            Semi => {
                self.next();
                return StmtBlank;
            }
            _ => panic!("Not a valid statement"),
        }
    }

    fn parse_mult(&mut self, tk: TokenType) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        loop {
            if self.peek().expect("Not a valid statement").t_type == tk {
                break;
            } else {
                stmts.push(self.parse_stmt());
            }
        }
        return stmts;
    }

    pub fn parse(&mut self) {
        self.parse_tree.stmts = self.parse_mult(Eof);
    }

    fn peek(&self) -> Option<&Token> {
        return self.tokens.get(self.pos as usize);
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    fn error(err: &str, token: &Token) {
        panic!(
            "{} at line {}: {}",
            err,
            token.line,
            token.val.clone().unwrap()
        );
    }
}
