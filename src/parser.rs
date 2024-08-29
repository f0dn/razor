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
    Asm(StmtAsm),
    Assign(StmtAssign),
    AssignAt(StmtAssignAt),
    Decl(StmtDecl),
    Exit(StmtExit),
    Expr(StmtExpr),
    For(StmtFor),
    Func(StmtFunc),
    If(StmtIf),
    Ret(StmtRet),
    Use(StmtUse),
    Blank,
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
    pub params: Vec<Identifier>,
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
    Asm(String),
    BinOp(BinOp),
    Call(Box<ExprCall>),
    Id(Identifier),
    Int(String),
    Stmts(Vec<Stmt>),
}

pub struct BinOp {
    pub op: TokenType,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct ExprCall {
    pub name: Identifier,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Error {
    pub msg: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for Error {}

impl Error {
    fn with_context(&self, context: &str) -> Error {
        Error {
            msg: format!("{}\n {}", context, self.msg),
        }
    }
}

fn context(context: &str) -> impl FnOnce(Error) -> Error + '_ {
    |err| err.with_context(context)
}

pub struct Parser {
    tokens: TokenList,
    op: HashMap<TokenType, i8>,
    pub parse_tree: Prog,
}

macro_rules! error {
    ($tokens:expr, $($arg:tt)*) => {
        return Err(Error {
            msg: format!("{}\n{}", $tokens.error_context(), format!($($arg)*)),
        })
    };
}

macro_rules! parse_fn {
    ($name:ident -> $ret:ident {
        $($({$($tk:tt)|+}$( => $tk_field:ident)?)?,
          $($func:ident($($arg:expr)?) => $func_field:ident,)?
        )+}
    ) => {
        impl Parser {
             fn $name(&mut self) -> Result<$ret, Error> {
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
                            _ => error!(self.tokens, "Expected {}, got {}", $($tk)+, &tk.t_type),
                        }
                    )?

                    $(
                        let $func_field = self.$func($($arg)?)?;
                    )?
                )+

                return Ok($ret {
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
                });
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
        {Func}, parse_ident_name() => ident, {LPar}, parse_mult_ident(RPar) => params, {RPar}, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_call -> ExprCall {
        , parse_ident_name() => name, {LPar}, parse_mult_expr(RPar) => args, {RPar},
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

impl Parser {
    pub fn new(mut tokens: TokenList) -> Parser {
        tokens.reset();
        Parser {
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
        }
    }

    fn parse_path(&mut self) -> Result<String, Error> {
        let tk = self.consume();
        match tk.t_type {
            Path(val) => Ok(val),
            _ => error!(self.tokens, "Expected path, got {}", &tk.t_type),
        }
    }

    fn parse_ident_name(&mut self) -> Result<Identifier, Error> {
        let tk = self.consume();
        match tk.t_type {
            Var(name) => Ok(Identifier {
                name,
                line: tk.line,
                is_ref: false,
            }),
            _ => error!(self.tokens, "Expected identifier, got {}", &tk.t_type),
        }
    }

    fn parse_asm(&mut self) -> Result<StmtAsm, Error> {
        let tk = self.consume();
        match tk.t_type {
            Asm(val) => Ok(StmtAsm { code: val }),
            _ => error!(self.tokens, "Expected assembly, got {}", &tk.t_type),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, Error> {
        let err_context = context("while parsing expression");
        let mut tk = self.consume();
        match tk.t_type {
            Int(val) => Ok(Expr::Int(val)),
            Var(_) | Amp => {
                if self.peek().t_type == LPar {
                    self.tokens.push_front(tk);
                    return Ok(Expr::Call(Box::new(
                        self.parse_call().map_err(err_context)?,
                    )));
                }

                let mut is_ref = false;
                if tk.t_type == Amp {
                    is_ref = true;
                    tk = self.consume();
                }

                match &tk.t_type {
                    Var(val) => Ok(Expr::Id(Identifier {
                        name: val.to_string(),
                        line: tk.line,
                        is_ref,
                    })),
                    _ => error!(
                        self.tokens,
                        "Expected variable after {}, got {}", Amp, &tk.t_type
                    ),
                }
            }
            Asm(val) => Ok(Expr::Asm(val)),
            LPar => {
                let expr = self.parse_expr().map_err(err_context)?;
                let tk = self.consume();
                match tk.t_type {
                    RPar => Ok(expr),
                    _ => error!(
                        self.tokens,
                        "Expected {} to close parenthesis, got {}", RPar, &tk.t_type
                    ),
                }
            }
            LBr => {
                let stmts = self.parse_mult(RBr).map_err(err_context)?;
                self.consume();
                Ok(Expr::Stmts(stmts))
            }
            _ => error!(
                self.tokens,
                "Unexpected {} while parsing expression", &tk.t_type
            ),
        }
    }

    fn parse_expr_prec(&mut self, min_prec: i8) -> Result<Expr, Error> {
        let mut lhs = self.parse_atom()?;

        loop {
            let tk = self.peek();

            let prec = match self.op.get(&tk.t_type) {
                Some(p) => *p,
                None => break,
            };

            if prec < min_prec {
                break;
            }

            let op = self.consume().t_type;
            let rhs = self.parse_expr_prec(prec + 1)?;
            lhs = Expr::BinOp(BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }
        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_expr_prec(0)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        let tk = self.peek();
        match tk.t_type {
            Ret => Ok(Stmt::Ret(
                self.parse_ret()
                    .map_err(context("while parsing return statement"))?,
            )),
            Exit => Ok(Stmt::Exit(
                self.parse_exit()
                    .map_err(context("while parsing exit statement"))?,
            )),
            Decl => Ok(Stmt::Decl(
                self.parse_decl()
                    .map_err(context("while parsing declaration"))?,
            )),
            If => Ok(Stmt::If(
                self.parse_if()
                    .map_err(context("while parsing if statement"))?,
            )),
            Var(_) => {
                if self.peek_mult(2).t_type == Eq {
                    return Ok(Stmt::Assign(
                        self.parse_assign()
                            .map_err(context("while parsing assignment"))?,
                    ));
                }
                Ok(Stmt::Expr(StmtExpr {
                    expr: self.parse_expr()?,
                }))
            }
            Func => Ok(Stmt::Func(
                self.parse_func()
                    .map_err(context("while parsing function"))?,
            )),
            For => Ok(Stmt::For(
                self.parse_for()
                    .map_err(context("while parsing for loop"))?,
            )),
            Asm(_) => Ok(Stmt::Asm(self.parse_asm()?)),
            Semi => {
                self.consume();
                Ok(Stmt::Blank)
            }
            Int(_) => {
                if self.peek_mult(2).t_type == At {
                    self.consume();
                    return Ok(Stmt::AssignAt(
                        self.parse_assign_at()
                            .map_err(context("while parsing assignment"))?,
                    ));
                }
                Ok(Stmt::Expr(StmtExpr {
                    expr: self.parse_expr()?,
                }))
            }
            Use => Ok(Stmt::Use(
                self.parse_use()
                    .map_err(context("while parsing use statement"))?,
            )),
            _ => Ok(Stmt::Expr(StmtExpr {
                expr: self.parse_expr()?,
            })),
        }
    }

    fn parse_mult_ident(&mut self, tk: TokenType) -> Result<Vec<Identifier>, Error> {
        let mut idents = Vec::new();
        loop {
            if self.peek().t_type == tk {
                break;
            } else {
                idents.push(self.parse_ident_name()?);
                if self.peek().t_type == tk {
                    break;
                }
                if self.consume().t_type != Comma {
                    error!(self.tokens, "Expected comma or {}, got {}", tk, &self.peek().t_type);
                }
            }
        }
        Ok(idents)
    }

    fn parse_mult_expr(&mut self, tk: TokenType) -> Result<Vec<Expr>, Error> {
        let mut exprs = Vec::new();
        loop {
            if self.peek().t_type == tk {
                break;
            } else {
                exprs.push(self.parse_expr()?);
                if self.peek().t_type == tk {
                    break;
                }
                if self.consume().t_type != Comma {
                    error!(self.tokens, "Expected comma or {}, got {}", tk, &self.peek().t_type);
                }
            }
        }
        Ok(exprs)
    }

    fn parse_mult(&mut self, tk: TokenType) -> Result<Vec<Stmt>, Error> {
        let mut stmts = Vec::new();
        loop {
            if self.peek().t_type == tk {
                break;
            } else {
                stmts.push(self.parse_stmt()?);
            }
        }
        Ok(stmts)
    }

    pub fn parse(&mut self) {
        self.parse_tree.stmts = match self.parse_mult(Eof) {
            Ok(stmts) => stmts,
            Err(e) => {
                panic!("{}", e.msg);
            }
        };
    }

    fn peek(&self) -> &Token {
        // TODO unwrap here
        return self.tokens.peek().unwrap();
    }

    fn peek_mult(&self, n: usize) -> &Token {
        // TODO unwrap here
        return self.tokens.peek_mult(n).unwrap();
    }

    fn consume(&mut self) -> Token {
        // TODO unwrap here
        self.tokens.next().unwrap()
    }
}
