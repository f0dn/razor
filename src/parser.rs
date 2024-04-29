use std::collections::HashMap;

use crate::tokenizer::TokenType::*;
use crate::tokenizer::*;

pub struct Identifier {
    pub name: String,
    pub line: usize,
    pub is_ref: bool,
    pub is_macro: bool,
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

pub struct ExprMacro {
    pub mac: usize,
    pub cont: Vec<MacroRepeat>,
}

pub struct MacroRepeat {
    pub vars: HashMap<String, MacroVar>,
    pub repeats: Vec<Vec<MacroRepeat>>,
}

pub enum MacroVar {
    Asm(String),
    Ident(Identifier),
    Int(String),
    Expr(Expr),
}

pub enum Expr {
    ExprInt(String),
    ExprId(Identifier),
    ExprCall(Box<ExprCall>),
    ExprBinOp(BinOp),
    ExprAsm(String),
    ExprMacro(ExprMacro),
}

pub struct StmtIf {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
}

pub struct StmtDecl {
    pub var: Identifier,
    pub expr: Expr,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub struct StmtExit {
    pub expr: Expr,
}

pub struct StmtAssign {
    pub var: Identifier,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtAssignAt {
    pub var: Identifier,
    pub expr: Expr,
    pub assign: TokenType,
}

pub struct StmtFunc {
    pub ident: Identifier,
    pub arg: Identifier,
    pub stmts: Vec<Stmt>,
}

pub struct StmtFor {
    pub init: StmtDecl,
    pub cond: Expr,
    pub iter: StmtAssign,
    pub stmts: Vec<Stmt>,
}

pub struct StmtAsm {
    pub code: String,
}

pub struct StmtExpr {
    pub expr: Expr,
}

pub struct StmtMRepeat {
    pub stmts: Vec<Stmt>,
}

pub struct StmtUse {
    pub path: String,
    pub ident: Identifier,
}

pub enum Stmt {
    StmtRet(StmtRet),
    StmtExit(StmtExit),
    StmtDecl(StmtDecl),
    StmtIf(StmtIf),
    StmtAssign(StmtAssign),
    StmtAssignAt(StmtAssignAt),
    StmtFunc(StmtFunc),
    StmtFor(StmtFor),
    StmtAsm(StmtAsm),
    StmtExpr(StmtExpr),
    StmtMRepeat(StmtMRepeat),
    StmtUse(StmtUse),
    StmtBlank,
}

pub struct Macro {
    pub ident: Identifier,
    pub args: Vec<MacroArg>,
    pub stmts: Vec<Stmt>,
}

pub enum MacroArg {
    Ident(Identifier),
    Asm(Identifier),
    Int(Identifier),
    Expr(Identifier),
    Token(TokenType),
    Group(Vec<MacroArg>, TokenType, u32, u32),
}

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    op: HashMap<TokenType, i8>,
    pub macros: Vec<Macro>,
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
        {At}, parse_ident_name() => var, {Eq} => assign, parse_expr() => expr, {Semi},
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
    parse_macro -> Macro {
        {Mac}, parse_ident_name() => ident, {LPar}, parse_mult_macro() => args, {RPar}, {LBr},
            parse_mult(RBr) => stmts,
        {RBr},
    }
}

parse_fn! {
    parse_repeat -> StmtMRepeat {
        {Hash}, {LPar}, parse_mult(RPar) => stmts, {RPar}, {Hash},
    }
}

parse_fn! {
    parse_use -> StmtUse {
        {Use}, parse_path() => path, {Dot}, parse_ident_name() => ident, {Semi},
    }
}

impl<'a> Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
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
            macros: Vec::new(),
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_path(&mut self) -> String {
        let tk = self.consume();
        match tk.t_type {
            Path => return tk.val.unwrap(),
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_ident_name(&mut self) -> Identifier {
        let mut is_macro = false;
        if self.peek().t_type == Hash {
            is_macro = true;
            self.consume();
        }
        let tk = self.consume();
        match tk.t_type {
            Var => {
                let name = tk.val.unwrap();
                return Identifier {
                    name,
                    line: tk.line,
                    is_ref: false,
                    is_macro,
                };
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_asm(&mut self) -> StmtAsm {
        let tk = self.consume();
        match tk.t_type {
            Asm => {
                return StmtAsm {
                    code: tk.val.unwrap(),
                };
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_atom(&mut self) -> Expr {
        use Expr::*;
        let mut tk = self.consume();
        match tk.t_type {
            Int => return ExprInt(tk.val.unwrap()),
            Var | Amp | Hash => {
                if self.peek().t_type == LPar {
                    self.tokens.push(tk);
                    return ExprCall(Box::new(self.parse_call()));
                }

                let mut is_ref = false;
                if tk.t_type == Amp {
                    is_ref = true;
                    tk = self.consume();
                }

                if tk.t_type == Hash {
                    tk = self.consume();
                    if tk.t_type != Var {
                        Parser::error("Unexpected {}", &tk);
                    }
                    return ExprId(Identifier {
                        name: tk.val.unwrap(),
                        line: tk.line,
                        is_ref,
                        is_macro: true,
                    });
                }

                if tk.t_type != Var {
                    Parser::error("Unexpected {}", &tk);
                }

                if self.peek().t_type == Hash {
                    self.tokens.push(tk);
                    return self.parse_macro_call();
                }

                return ExprId(Identifier {
                    name: tk.val.unwrap(),
                    line: tk.line,
                    is_ref,
                    is_macro: false,
                });
            }
            Asm => return ExprAsm(tk.val.unwrap()),
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
        return self.parse_expr_prec(0);
    }

    fn get_args_from_vec(&self, vec: &Vec<usize>) -> &Vec<MacroArg> {
        let mut args = &self.macros[vec[0]].args;
        for i in 1..vec.len() {
            match &args[vec[i]] {
                MacroArg::Group(a, _, _, _) => {
                    args = a;
                }
                _ => unreachable!(),
            }
        }
        return args;
    }

    fn parse_macro_inner(&mut self, path: &mut Vec<usize>) -> MacroRepeat {
        let mut vars = HashMap::new();
        let mut repeats = Vec::new();
        for i in 0..self.get_args_from_vec(path).len() {
            let mut tk = Token {
                t_type: Eof,
                val: None,
                line: 0,
            };
            match &self.get_args_from_vec(path)[i] {
                MacroArg::Group(_, _, _, _) => (),
                MacroArg::Expr(_) => (),
                _ => tk = self.consume(),
            }
            match &self.get_args_from_vec(path)[i] {
                MacroArg::Ident(ident) => {
                    if tk.t_type != Var {
                        Parser::error("Unexpected {}", &tk);
                    }
                    vars.insert(
                        ident.name.clone(),
                        MacroVar::Ident(Identifier {
                            name: tk.val.unwrap(),
                            line: tk.line,
                            is_ref: false,
                            is_macro: false,
                        }),
                    );
                }
                MacroArg::Asm(ident) => {
                    if tk.t_type != Asm {
                        Parser::error("Unexpected {}", &tk);
                    }
                    vars.insert(ident.name.clone(), MacroVar::Asm(tk.val.unwrap()));
                }
                MacroArg::Int(ident) => {
                    if tk.t_type != Int {
                        Parser::error("Unexpected {}", &tk);
                    }
                    vars.insert(ident.name.clone(), MacroVar::Int(tk.val.unwrap()));
                }
                MacroArg::Expr(ident) => {
                    vars.insert(ident.name.clone(), MacroVar::Expr(self.parse_expr()));
                }
                MacroArg::Token(token) => {
                    if token != &tk.t_type {
                        Parser::error("Unexpected {}", &tk);
                    }
                }
                MacroArg::Group(_, end, lower, upper) => {
                    let mut count = 1;
                    let lower = *lower;
                    let upper = *upper;
                    let end = *end;

                    path.push(i);

                    let mut inner = Vec::new();
                    loop {
                        if self.peek().t_type == end {
                            break;
                        }

                        let repeat = self.parse_macro_inner(path);
                        inner.push(repeat);

                        count += 1;
                    }
                    if count < lower || count > upper {
                        Parser::error("Failed to parse macro", &self.peek());
                    }

                    repeats.push(inner);
                }
            }
        }
        return MacroRepeat { repeats, vars };
    }

    fn parse_macro_call(&mut self) -> Expr {
        let tk = self.consume();
        match tk.t_type {
            Var => {
                let name = tk;
                let tk = self.consume();
                if tk.t_type != Hash {
                    Parser::error("Unexpected {}", &tk);
                }
                let tk = self.consume();
                if tk.t_type != LPar {
                    Parser::error("Unexpected {}", &tk);
                }
                let mut idx = None;
                for i in 0..self.macros.len() {
                    if let Some(macro_name) = &name.val {
                        if self.macros[i].ident.name == *macro_name {
                            idx = Some(i);
                            break;
                        }
                    }
                }
                if idx.is_none() {
                    Parser::error("Undefined macro {}", &name);
                }
                let repeat = self.parse_macro_inner(&mut vec![idx.unwrap()]);
                let tk = self.consume();
                if tk.t_type != RPar {
                    Parser::error("Unexpected {}", &tk);
                }
                return Expr::ExprMacro(ExprMacro {
                    mac: idx.unwrap(),
                    cont: vec![repeat],
                });
            }
            _ => Parser::error("Unexpected {}", &tk),
        }
    }

    fn parse_stmt(&mut self) -> Stmt {
        use Stmt::*;
        let tk = self.peek();
        match tk.t_type {
            Ret => return StmtRet(self.parse_ret()),
            Exit => return StmtExit(self.parse_exit()),
            Decl => return StmtDecl(self.parse_decl()),
            If => return StmtIf(self.parse_if()),
            Var => {
                if self.peek_mult(2).t_type == Eq {
                    return StmtAssign(self.parse_assign());
                }
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                });
            }
            Hash => return StmtMRepeat(self.parse_repeat()),
            Func => return StmtFunc(self.parse_func()),
            For => return StmtFor(self.parse_for()),
            Asm => return StmtAsm(self.parse_asm()),
            Semi => {
                self.consume();
                return StmtBlank;
            }
            Int => {
                if self.peek_mult(2).t_type == At {
                    self.consume();
                    return StmtAssignAt(self.parse_assign_at());
                }
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                });
            }
            Mac => {
                let mac = self.parse_macro();
                self.macros.push(mac);
                return StmtBlank;
            }
            Use => return StmtUse(self.parse_use()),
            _ => {
                return StmtExpr(crate::parser::StmtExpr {
                    expr: self.parse_expr(),
                })
            }
        }
    }

    fn parse_macro_arg(&mut self) -> MacroArg {
        let tk = self.consume();
        match tk.t_type {
            Hash => {
                let tk = self.consume();
                let arg = match tk.t_type {
                    LPar => {
                        let args = self.parse_mult_macro();
                        let end = self.consume();

                        let tk = self.consume();
                        if tk.t_type != Int {
                            Parser::error("Unexpected {}", &tk);
                        }
                        let lower = tk.val.unwrap().parse::<u32>().unwrap();
                        let mut tk = self.consume();
                        let mut upper = u32::MAX;
                        if tk.t_type == Int {
                            upper = tk.val.unwrap().parse::<u32>().unwrap();
                            tk = self.consume();
                        }
                        if tk.t_type != RPar {
                            Parser::error("Unexpected {}", &tk);
                        }
                        MacroArg::Group(args, end.t_type, lower, upper)
                    }
                    Asm => MacroArg::Asm(self.parse_ident_name()),
                    Int => MacroArg::Int(self.parse_ident_name()),
                    Var => MacroArg::Ident(self.parse_ident_name()),
                    Dash => MacroArg::Expr(self.parse_ident_name()),
                    _ => Parser::error("Unexpected {}", &tk),
                };
                let tk = self.consume();
                if tk.t_type != Hash {
                    Parser::error("Unexpected {}", &tk);
                }
                return arg;
            }
            _ => return MacroArg::Token(tk.t_type),
        }
    }

    fn parse_mult_macro(&mut self) -> Vec<MacroArg> {
        let t = self.consume();
        if t.t_type != Hash {
            Parser::error("unexpected {}", &t);
        }
        let t = self.consume();
        if t.t_type != Hash {
            Parser::error("unexpected {}", &t);
        }
        let mut args = Vec::new();
        loop {
            if self.peek().t_type == Hash && self.peek_mult(2).t_type == Hash {
                self.consume();
                self.consume();
                break;
            } else {
                args.push(self.parse_macro_arg());
            }
        }
        return args;
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

    pub fn parse_macro_uses(&mut self) -> Vec<StmtUse> {
        let mut uses = Vec::new();
        loop {
            match self.peek().t_type {
                Hash => {
                    self.consume();
                    uses.push(self.parse_use());
                }
                _ => break,
            }
        }
        return uses;
    }

    pub fn add_macro(&mut self, mac: Macro) {
        self.macros.push(mac);
    }

    fn peek(&self) -> &Token {
        return self.tokens.last().unwrap();
    }

    fn peek_mult(&self, n: usize) -> &Token {
        return self.tokens.get(self.tokens.len() - n).unwrap();
    }

    fn consume(&mut self) -> Token {
        return self.tokens.pop().unwrap();
    }

    fn error(err: &str, token: &Token) -> ! {
        panic!(
            "{} at line {}",
            err.replace(
                "{}",
                &format!(
                    "'{}'",
                    match &token.val {
                        None => token.t_type.val(),
                        Some(val) => val,
                    }
                )
            ),
            token.line,
        );
    }
}
