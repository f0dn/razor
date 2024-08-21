use std::collections::HashMap;

use crate::tokenizer::TokenType::*;
use crate::tokenizer::*;
use crate::tokenlist::TokenList;

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Identifier {
    pub name: String,
    pub line: usize,
    pub is_ref: bool,
}

pub enum Stmt {
    StmtAsm(StmtAsm),
    StmtAssign(StmtAssign),
    StmtAssignAt(StmtAssignAt),
    StmtDecl(StmtDecl),
    StmtExit(StmtExit),
    StmtExpr(StmtExpr),
    StmtFor(StmtFor),
    StmtFunc(StmtFunc),
    StmtIf(StmtIf),
    StmtRet(StmtRet),
    StmtUse(StmtUse),
    StmtBlank,
}

pub struct StmtAsm {
    pub code: String,
}

pub struct StmtAssign {
    pub var: Identifier,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtAssignAt {
    pub var: Expr,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtDecl {
    pub var: Identifier,
    pub expr: Expr,
}

pub struct StmtExit {
    pub expr: Expr,
}

pub struct StmtExpr {
    pub expr: Expr,
}

pub struct StmtFor {
    pub init: StmtDecl,
    pub cond: Expr,
    pub iter: StmtAssign,
    pub stmts: Vec<Stmt>,
}

pub struct StmtFunc {
    pub ident: Identifier,
    pub arg: Identifier,
    pub stmts: Vec<Stmt>,
}

pub struct StmtIf {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub struct StmtUse {
    pub path: String,
    pub ident: Identifier,
}

pub enum Expr {
    ExprAsm(String),
    ExprBinOp(BinOp),
    ExprCall(Box<ExprCall>),
    ExprId(Identifier),
    ExprInt(String),
    ExprStmts(Vec<Stmt>),
}

pub struct BinOp {
    pub op: TokenType,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct ExprCall {
    pub name: Identifier,
    pub arg: Expr,
}

pub struct Parser {
    tokens: TokenList,
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
    parse_exit -> StmtExit {
        {Exit}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_decl -> StmtDecl {
        {Decl}, parse_ident_name() => var, {Eq}, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_assign -> StmtAssign {
        , parse_ident_name() => var, {Eq} => assign, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_assign_at -> StmtAssignAt {
        {At}, parse_expr() => var, {Eq} => assign, parse_expr() => expr, {Semi},
    }
}

parse_fn! {
    parse_func -> StmtFunc {
        {Func}, parse_ident_name() => ident, {LPar}, parse_ident_name() => arg, {RPar}, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_call -> ExprCall {
        , parse_ident_name() => name, {LPar}, parse_expr() => arg, {RPar},
    }
}

parse_fn! {
    parse_for -> StmtFor {
        {For}, parse_decl() => init, , parse_expr() => cond, {Semi}, parse_assign() => iter, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_use -> StmtUse {
        {Use}, parse_path() => path, {Dot}, parse_ident_name() => ident, {Semi},
    }
}

impl<'a> Parser {
    pub fn new(mut tokens: TokenList) -> Parser {
        tokens.reset();
        return Parser {
            tokens,
            op: HashMap::from([
                (Ex, 4),
                (At, 4),
                (Star, 3),
                (Slash, 3),
                (Per, 3),
                (Plus, 2),
                (Dash, 2),
                (Gt, 1),
                (Lt, 1),
                (DEq, 1),
                (DAmp, 0),
                (DPipe, 0),
            ]),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_path(&mut self) -> String {
        let tk = self.consume();
        match tk.t_type {
            Path(val) => return val,
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_ident_name(&mut self) -> Identifier {
        let tk = self.consume();
        match tk.t_type {
            Var(name) => {
                return Identifier {
                    name,
                    line: tk.line,
                    is_ref: false,
                };
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_asm(&mut self) -> StmtAsm {
        let tk = self.consume();
        match tk.t_type {
            Asm(val) => {
                return StmtAsm { code: val };
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
        let mut tk = self.consume();
        match tk.t_type {
            Int(val) => return ExprInt(val),
            Var(_) | Amp => {
                if self.peek().t_type == LPar {
                    self.tokens.push_front(tk);
                    return ExprCall(Box::new(self.parse_call()));
                }

                let mut is_ref = false;
                if tk.t_type == Amp {
                    is_ref = true;
                    tk = self.consume();
                }

                match &tk.t_type {
                    Var(val) => {
                        return ExprId(Identifier {
                            name: val.to_string(),
                            line: tk.line,
                            is_ref,
                        });
                    }
                    _ => Parser::error("Unexpected {}", &tk),
                }
            }
            Asm(val) => return ExprAsm(val),
            LPar => {
                let expr = self.parse_expr();
                let tk = self.consume();
                match tk.t_type {
                    RPar => return expr,
                    _ => Parser::error("Missing ')'", &tk),
                }
            }
            LBr => {
                let stmts = self.parse_mult(RBr);
                self.consume();
                return ExprStmts(stmts);
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
        return self.parse_expr_prec(0);
    }

    fn parse_stmt(&mut self) -> Stmt {
        use Stmt::*;
        let tk = self.peek();
        match tk.t_type {
            Ret => return StmtRet(self.parse_ret()),
            Exit => return StmtExit(self.parse_exit()),
            Decl => return StmtDecl(self.parse_decl()),
            If => return StmtIf(self.parse_if()),
            Var(_) => {
                if self.peek_mult(2).t_type == Eq {
                    return StmtAssign(self.parse_assign());
                }
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                });
            }
            Func => return StmtFunc(self.parse_func()),
            For => return StmtFor(self.parse_for()),
            Asm(_) => return StmtAsm(self.parse_asm()),
            Semi => {
                self.consume();
                return StmtBlank;
            }
            Int(_) => {
                if self.peek_mult(2).t_type == At {
                    self.consume();
                    return StmtAssignAt(self.parse_assign_at());
                }
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                });
            }
            Use => return StmtUse(self.parse_use()),
            _ => {
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                })
            }
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
        return self.tokens.peek().unwrap();
    }

    fn peek_mult(&self, n: usize) -> &Token {
        return self.tokens.peek_mult(n).unwrap();
    }

    fn consume(&mut self) -> Token {
        return self.tokens.next().unwrap();
    }

    fn error(err: &str, token: &Token) -> ! {
        panic!(
            "{} at line {}",
            err.replace("{}", &format!("'{}'", token.t_type)),
            token.line,
        );
    }
}
