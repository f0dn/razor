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

pub struct StmtAssign {
    pub var: String,
    pub expr: Expr,
}

pub enum Stmt {
    StmtRet(StmtRet),
    StmtDecl(StmtDecl),
    StmtIf(StmtIf),
    StmtAssign(StmtAssign),
    StmtBlank,
}

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    op: HashMap<TokenType, i8>,
    pub parse_tree: Prog,
}

macro_rules! parse_fn {
    ($name:ident -> $ret:ident($e_msg:literal) {
        $($({$tk:pat}$( => $tk_field:ident)?)?,
          $($func:ident($($arg:expr)?) => $func_field:ident, )?
        )+}
    ) => {
        impl Parser {
            fn $name(&mut self) -> $ret {
                $(
                    $(
                        $(let $tk_field;)?
                        self.peek().expect($e_msg);
                        let tk = self.consume();
                        match &tk.t_type {
                            $tk => {
                                $(
                                    $tk_field = tk.val.unwrap();
                                )?
                            }
                            _ => Parser::error("Unexpected {}", &tk),
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
    parse_if -> StmtIf("Incomplete if statement") {
        {If}, parse_expr() => expr, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_ret -> StmtRet("Incomplete return statement") {
        {Ret}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_decl -> StmtDecl("Incomplete variable delcaration") {
        {Decl}, {Var} => var, {Eq}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_assign -> StmtAssign("Incomplete assignment") {
        {Var} => var, {Eq}, parse_expr() => expr, {Semi},
    }
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
        return Parser {
            tokens,
            op: HashMap::from([(Star, 2), (Slash, 2), (Plus, 1), (Dash, 1)]),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
        self.peek().expect("Incomplete expression");
        let tk = self.consume();
        match tk.t_type {
            Int => return ExprInt(tk.val.unwrap()),
            Var => return ExprId(tk.val.unwrap()),
            LPar => {
                let expr = self.parse_expr();
                self.peek().expect("Incomplete expression");
                let tk = self.consume();
                match tk.t_type {
                    RPar => return expr,
                    _ => Parser::error("Missing ')'", &tk),
                }
            }
            _ => Parser::error("Invalid expression", &tk),
        }
    }

    fn parse_expr_prec(&mut self, min_prec: i8) -> Expr {
        let mut lhs = self.parse_atom();

        loop {
            let tk = self.peek().expect("Missing ';'");
            let prec;
            match self.op.get(&tk.t_type) {
                Some(p) => prec = *p,
                None => break,
            }
            if prec < min_prec {
                break;
            }

            let op = self.consume().t_type;
            let rhs = self.parse_expr_prec(prec + 1);
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
        let tk = self.peek().expect("Incomplete statement");
        match tk.t_type {
            Ret => return StmtRet(self.parse_ret()),
            Decl => return StmtDecl(self.parse_decl()),
            If => return StmtIf(self.parse_if()),
            Var => return StmtAssign(self.parse_assign()),
            Semi => {
                self.consume();
                return StmtBlank;
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_mult(&mut self, tk: TokenType) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        loop {
            if self.peek().expect(&format!("Expected {}", tk.val())).t_type == tk {
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
        return self.tokens.last();
    }

    fn consume(&mut self) -> Token {
        return self.tokens.pop().unwrap();
    }

    fn error(err: &str, token: &Token) -> ! {
        panic!(
            "{} at line {}",
            err.replace("{}", &format!("'{}'", &token.t_type.val())),
            token.line,
        );
    }
}
