use std::collections::HashMap;

use crate::path::UsePath;
use crate::tokenizer::tokenlist::TokenList;
use crate::tokenizer::TokenType::*;
use crate::tokenizer::*;

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Identifier {
    pub name: String,
    pub is_ref: bool,
}

pub enum Stmt {
    Asm(StmtAsm),
    Assign(StmtAssign),
    AssignAt(StmtAssignAt),
    Decl(StmtDecl),
    DeclSize(StmtDeclSize),
    Const(StmtConst),
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
    pub var: String,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtAssignAt {
    pub var: Expr,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtDecl {
    pub var: String,
    pub expr: Expr,
}

pub struct StmtDeclSize {
    pub var: String,
    pub size: Expr,
}

pub struct StmtConst {
    pub var: String,
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
    pub ident: String,
    pub params: Vec<String>,
    pub stmts: Vec<Stmt>,
}

pub struct StmtIf {
    pub blocks: Vec<IfBlock>,
    pub else_block: Option<Vec<Stmt>>,
}

pub struct IfBlock {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub struct StmtUse {
    pub path: UsePath,
}

pub enum Expr {
    UnOp(UnOp),
    BinOp(BinOp),
    Call(Box<ExprCall>),
    Id(Identifier),
    Literal(ExprLiteral),
    Stmts(Vec<Stmt>),
}

pub enum ExprLiteral {
    Int(String),
    Str(String),
    Char(char),
    Asm(String),
}

pub struct BinOp {
    pub op: TokenType,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub struct UnOp {
    pub op: TokenType,
    pub expr: Box<Expr>,
}

pub struct ExprCall {
    pub path: UsePath,
    pub func: String,
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
    unops: HashMap<TokenType, i8>,
    op_prec: HashMap<TokenType, i8>,
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
          $($func:ident($($arg:expr),*) => $($func_field:ident);+,)?
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
                        #[allow(unused_parens)]
                        let ($($func_field),+) = self.$func($($arg),*)?;
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
                            $($func_field),+,
                        )?
                    )+
                });
            }
        }
    };
}

parse_fn! {
    parse_if_block -> IfBlock {
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
    parse_decl_size -> StmtDeclSize {
        {Decl}, parse_ident_name() => var, {LBr}, parse_expr() => size, {RBr}, {Semi},
    }
}

parse_fn! {
    parse_const -> StmtConst {
        {Const}, parse_ident_name() => var, {Eq}, parse_expr() => expr, {Semi},
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
        {Func}, parse_ident_name() => ident, {LPar}, parse_mult_ident(RPar, Comma) => params, {RPar}, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_call -> ExprCall {
        , parse_func_path() => path; func, {LPar}, parse_mult_expr(RPar) => args, {RPar},
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
        {Use}, parse_use_path() => path, {Semi},
    }
}

impl Parser {
    pub fn new(mut tokens: TokenList) -> Parser {
        tokens.reset();
        Parser {
            tokens,
            unops: HashMap::from([(Ex, 4), (At, 4)]),
            op_prec: HashMap::from([
                (Size, 5),
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

    fn parse_ident_name(&mut self) -> Result<String, Error> {
        let tk = self.consume();
        match tk.t_type {
            Var(name) => Ok(name),
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
            Int(val) => Ok(Expr::Literal(ExprLiteral::Int(val))),
            Asm(val) => Ok(Expr::Literal(ExprLiteral::Asm(val))),
            Str(val) => Ok(Expr::Literal(ExprLiteral::Str(val))),
            Char(val) => Ok(Expr::Literal(ExprLiteral::Char(val))),
            Var(_) | Amp => {
                if self.peek().t_type == LPar || self.peek().t_type == Dot {
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
                        is_ref,
                    })),
                    _ => error!(
                        self.tokens,
                        "Expected variable after {}, got {}", Amp, &tk.t_type
                    ),
                }
            }
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
        let mut lhs = if let Some(&prec) = self.unops.get(&self.peek().t_type) {
            let op = self.consume().t_type;
            let expr = self.parse_expr_prec(prec)?;
            Expr::UnOp(UnOp {
                op,
                expr: Box::new(expr),
            })
        } else {
            self.parse_atom()?
        };

        loop {
            let tk = self.peek();

            let prec = match self.op_prec.get(&tk.t_type) {
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
            Decl => {
                if self.peek_mult(3).t_type == LBr {
                    Ok(Stmt::DeclSize(
                        self.parse_decl_size()
                            .map_err(context("while parsing declaration"))?,
                    ))
                } else {
                    Ok(Stmt::Decl(
                        self.parse_decl()
                            .map_err(context("while parsing declaration"))?,
                    ))
                }
            }
            Const => Ok(Stmt::Const(
                self.parse_const()
                    .map_err(context("while parsing const declaration"))?,
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
            At => Ok(Stmt::AssignAt(
                self.parse_assign_at()
                    .map_err(context("while parsing assignment"))?,
            )),
            Use => Ok(Stmt::Use(
                self.parse_use()
                    .map_err(context("while parsing use statement"))?,
            )),
            _ => Ok(Stmt::Expr(StmtExpr {
                expr: self.parse_expr()?,
            })),
        }
    }

    fn parse_if(&mut self) -> Result<StmtIf, Error> {
        let mut blocks = Vec::new();
        let mut else_block = None;

        loop {
            blocks.push(self.parse_if_block()?);
            if self.peek().t_type != Else {
                break;
            }
            self.consume();
            if self.peek().t_type == If {
                continue;
            }
            if self.consume().t_type != LBr {
                error!(self.tokens, "Expected {}, got {}", LBr, &self.peek().t_type);
            }
            else_block = Some(self.parse_mult(RBr)?);
            if self.consume().t_type != RBr {
                error!(self.tokens, "Expected {}, got {}", RBr, &self.peek().t_type);
            }
            break;
        }

        Ok(StmtIf { blocks, else_block })
    }

    fn parse_mult_ident(&mut self, tk: TokenType, sep: TokenType) -> Result<Vec<String>, Error> {
        let mut idents = Vec::new();
        loop {
            if self.peek().t_type == tk {
                break;
            } else {
                idents.push(self.parse_ident_name()?);
                if self.peek().t_type == tk {
                    break;
                }
                if self.consume().t_type != sep {
                    error!(
                        self.tokens,
                        "Expected comma or {}, got {}",
                        tk,
                        &self.peek().t_type
                    );
                }
            }
        }
        Ok(idents)
    }

    fn parse_use_path(&mut self) -> Result<UsePath, Error> {
        Ok(UsePath::from_vec(self.parse_mult_ident(Semi, Dot)?))
    }

    fn parse_func_path(&mut self) -> Result<(UsePath, String), Error> {
        let mut vec = self.parse_mult_ident(LPar, Dot)?;
        let func_name = vec.pop().unwrap();
        Ok((UsePath::from_vec(vec), func_name))
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
                    error!(
                        self.tokens,
                        "Expected comma or {}, got {}",
                        tk,
                        &self.peek().t_type
                    );
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
        self.parse_tree.stmts = self.parse_mult(Eof).unwrap_or_else(|e| {
            panic!("{}", e.msg);
        });
    }

    fn peek(&self) -> &Token {
        // TODO unwrap here
        self.tokens.peek().unwrap()
    }

    fn peek_mult(&self, n: usize) -> &Token {
        // TODO unwrap here
        self.tokens.peek_mult(n).unwrap()
    }

    fn consume(&mut self) -> Token {
        // TODO unwrap here
        self.tokens.next().unwrap()
    }
}
