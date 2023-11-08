use std::collections::HashMap;

use crate::tokenizer::TokenType::*;
use crate::tokenizer::*;

pub struct Variable {
    pub name: String,
    pub line: u32,
}

pub struct BinOp {
    pub op: TokenType,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub enum Expr {
    ExprInt(String),
    ExprId(Variable),
    ExprBinOp(BinOp),
}

pub struct StmtIf {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
}

pub struct StmtDecl {
    pub var: Variable,
    pub expr: Expr,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub struct StmtAssign {
    pub var: Variable,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtFunc {
    pub name: Variable,
    pub arg: Variable,
    pub stmts: Vec<Stmt>,
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
    ($name:ident -> $ret:ident {
        $($({$($tk:pat_param)|+}$( => $tk_field:ident)?)?,
          $($func:ident($($arg:expr)?) => $func_field:ident,)?
        )+}
    ) => {
        impl Parser {
             fn $name(&mut self) -> $ret {
                $(
                    $(
                        $(let $tk_field;)?
                        let tk = self.consume();
                        match &tk.t_type {
                            $($tk )|+ => {
                                $(
                                    $tk_field = tk.t_type;
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
        {Decl}, parse_var() => var, {Eq}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_assign -> StmtAssign {
        , parse_var() => var, {Eq | PEq} => assign, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_func -> StmtFunc {
        {Func}, parse_var() => name, {LPar}, parse_var() => arg, {RPar}, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
        return Parser {
            tokens,
            op: HashMap::from([
                (Ex, 4),
                (Star, 3),
                (Slash, 3),
                (Per, 3),
                (Plus, 2),
                (Dash, 2),
                (DEq, 1),
                (DPipe, 0),
            ]),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_var(&mut self) -> Variable {
        let tk = self.consume();
        match tk.t_type {
            Var => {
                let name = tk.val.unwrap();
                return Variable {
                    name,
                    line: tk.line,
                };
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
        let tk = self.consume();
        match tk.t_type {
            Int => return ExprInt(tk.val.unwrap()),
            Var => {
                return ExprId(Variable {
                    name: tk.val.unwrap(),
                    line: tk.line,
                })
            }
            LPar => {
                let expr = self.parse_expr();
                let tk = self.consume();
                match tk.t_type {
                    RPar => return expr,
                    _ => Parser::error("Missing ')'", &tk),
                }
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_expr_prec(&mut self, min_prec: i8) -> Expr {
        let mut lhs = self.parse_atom();

        loop {
            let tk = self.peek();
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
        let tk = self.peek();
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
            if self.peek().t_type == tk {
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

    fn peek(&self) -> &Token {
        return self.tokens.last().unwrap();
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
