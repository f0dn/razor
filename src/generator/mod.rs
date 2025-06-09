use std::collections::{HashMap, HashSet};

mod label;
mod stack;

use crate::parser::*;
use crate::path::UsePath;
use crate::tokenizer::TokenType::*;
use label::LabelGen;
use stack::Stack;

const REG_ARGS: [&str; 6] = ["rdi", "rsi", "rdx", "r10", "r8", "r9"];

type ExprRes = (String, usize);

pub struct Generator<'a> {
    pub text: String,
    path: &'a UsePath,
    functions: String,
    stack: Stack,
    for_counter: LabelGen,
    if_counter: LabelGen,
    if_end_counter: LabelGen,
    ext: HashSet<String>,
    global: String,
    imports: HashSet<UsePath>,
}

macro_rules! asm {
    (@expr_to_brackets $expr:expr) => {"{}"};
    ($($(> $indent_str:literal)? $($str:literal)? $({$expr:expr})? $(,$arg:expr)*);+) => {
        format!(
            concat!($(
                $("    ", $indent_str, "\n",)?
                $($str, "\n",)?
                $(asm!(@expr_to_brackets $expr),)?
            )+),
            $($($expr,)? $($arg,)*)+
        )
    };
}

impl<'a> Generator<'a> {
    pub fn new(file: &'a UsePath) -> Generator<'a> {
        Generator {
            text: String::from("section .text\n"),
            path: file,
            functions: String::new(),
            stack: Stack::new(),
            for_counter: LabelGen::new("for"),
            if_counter: LabelGen::new("if"),
            if_end_counter: LabelGen::new("if_end"),
            ext: HashSet::new(),
            global: String::new(),
            imports: HashSet::new(),
        }
    }

    pub fn links(&self) -> &HashSet<UsePath> {
        &self.imports
    }

    fn gen_literal(&mut self, literal: &'a ExprLiteral) -> ExprRes {
        match literal {
            ExprLiteral::Asm(asm) => (self.gen_asm(asm), 8),
            ExprLiteral::Int(int) => (asm!(> "mov eax, {}", int), 4),
            ExprLiteral::Char(char) => (asm!(> "mov al, {}", *char as u8), 1),
            // TODO use stack strings
            ExprLiteral::Str(string) => {
                let mut asm = asm!(
                    > "mov rax, 9";
                    > "mov rsi, {}", string.len() + 17;
                    > "mov rdx, 3";
                    > "mov r10, 33";
                    > "mov r8, 255";
                    > "mov r9, 0";
                    > "syscall";
                    > "mov QWORD [rax], {}", string.len();
                    > "mov QWORD [rax+8], {}", string.len();
                );
                for (i, c) in string.chars().enumerate() {
                    asm.push_str(&asm!(
                        > "mov byte [rax+{}], {}",
                        i + 16,
                        c as u8
                    ));
                }
                asm.push_str(&asm!(
                    > "mov byte [rax+{}], 0",
                    string.len() + 16
                ));
                (asm, 8)
            }
        }
    }

    fn gen_const_literal(&mut self, literal: &'a ExprLiteral) -> usize {
        match literal {
            ExprLiteral::Asm(_asm) => panic!("Cannot use asm in constant expression"),
            ExprLiteral::Int(int) => int.parse().unwrap(),
            ExprLiteral::Char(char) => *char as usize,
            // TODO support strings in constant expressions
            ExprLiteral::Str(_string) => panic!("Cannot use string in constant expression"),
        }
    }

    fn gen_expr(&mut self, expr: &'a Expr) -> ExprRes {
        let expr_string = match expr {
            Expr::Literal(literal) => self.gen_literal(literal),
            Expr::Id(ident) => {
                let var = self.stack.get(&ident.name);
                match var {
                    Some((var, size)) => {
                        if ident.is_ref {
                            (asm!(> "lea rax, {}", var), size)
                        } else {
                            (asm!(> "mov rax, {}", var), size)
                        }
                    }
                    None => {
                        let val = self.stack.get_const(&ident.name);
                        match val {
                            Some(val) => (asm!(> "mov rax, {}", val), 8),
                            None => panic!("Unknown identifier '{}'", ident.name),
                        }
                    }
                }
            }
            Expr::BinOp(bin_op) => {
                let op = match bin_op.op {
                    Plus => asm!(> "add rax, rcx"),
                    Dash => asm!(> "sub rax, rcx"),
                    Star => asm!(> "mul rcx"),
                    Slash => asm!(
                        > "mov edx, 0";
                        > "div rcx";
                    ),
                    Per => asm!(
                        > "mov edx, 0";
                        > "div rcx";
                        > "mov rax, rdx";
                    ),
                    DEq => asm!(
                        > "cmp rax, rcx";
                        > "setz al";
                        > "movzx rax, al";
                    ),
                    DPipe => asm!(
                        > "cmp rax, 0";
                        > "setz al";
                        > "cmp rcx, 0";
                        > "setz ah";
                        > "and al, ah";
                        > "xor al, 0b00000001";
                        > "movzx rax, al";
                    ),
                    DAmp => asm!(
                        > "cmp rax, 0";
                        > "setz al";
                        > "cmp rcx, 0";
                        > "setz ah";
                        > "or al, ah";
                        > "xor al, 0b00000001";
                        > "movzx rax, al";
                    ),
                    Lt => asm!(
                        > "cmp rax, rcx";
                        > "setc al";
                        > "movzx rax, al";
                    ),
                    Gt => asm!(
                        > "cmp rax, rcx";
                        > "setz al";
                        > "cmc";
                        > "setc ah";
                        > "xor al, ah";
                        > "movzx rax, al";
                    ),
                    Ex => asm!(
                        > "cmp rcx, 0";
                        > "setz al";
                        > "movzx rax, al";
                    ),
                    At => asm!(> "mov rax, [rcx]"),
                    _ => panic!("Unknown operator: {}", bin_op.op),
                };
                let lhs = self.gen_expr(&bin_op.lhs);
                let rhs = self.gen_expr(&bin_op.rhs);
                asm!(
                    {self.gen_expr(&bin_op.lhs)};
                    {self.push("rax")};
                    {self.gen_expr(&bin_op.rhs)};
                    > "mov rcx, rax";
                    {self.pop("rax")};
                    {op};
                )
            }
            Expr::Call(expr_call) => {
                let func_name = if expr_call.path.is_empty() {
                    format!("{}.{}", self.path, expr_call.func)
                } else {
                    let mut name = None;
                    for import in &self.imports {
                        if import.matches(&expr_call.path) {
                            name = Some(format!("{}.{}", import, expr_call.func));
                        }
                    }
                    match name {
                        Some(name) => name,
                        None => panic!("Unknown function '{}'", expr_call.func),
                    }
                };

                self.ext.insert(func_name.clone());
                let num_args = expr_call.args.len();
                if num_args > REG_ARGS.len() {
                    panic!("Too many arguments for function '{}'", func_name);
                }
                let mut args = String::new();
                for arg in &expr_call.args {
                    args.push_str(&self.gen_expr(arg));
                    args.push_str(&self.push("rax"));
                }
                for reg in REG_ARGS.iter().take(num_args).rev() {
                    args.push_str(&self.pop(reg));
                }
                asm!(
                    {args};
                    > "call {}", func_name;
                )
            }
            Expr::Stmts(stmts) => self.gen_scope(stmts),
        };
        asm!(
            "; Expression Start";
            {expr_string};
            "; Expression End";
        )
    }

    fn gen_const_expr(&mut self, expr: &'a Expr) -> usize {
        match expr {
            Expr::Literal(literal) => self.gen_const_literal(literal),
            Expr::Id(ident) => {
                assert!(!ident.is_ref, "Cannot reference in constant expression");
                self.stack
                    .get_const(&ident.name)
                    .unwrap_or_else(|| panic!("Unknown identifier {}", ident.name))
            }
            Expr::BinOp(bin_op) => {
                let left = self.gen_const_expr(&bin_op.lhs);
                let right = self.gen_const_expr(&bin_op.rhs);
                match bin_op.op {
                    Plus => left + right,
                    Dash => left - right,
                    Star => left * right,
                    Slash => left / right,
                    Per => left % right,
                    DEq => (left == right) as usize,
                    DPipe => (left != 0 || right != 0) as usize,
                    DAmp => (left != 0 && right != 0) as usize,
                    Lt => (left < right) as usize,
                    Gt => (left > right) as usize,
                    Ex => (right == 0) as usize,
                    At => panic!("Cannot use '@' in constant expression"),
                    _ => panic!("Unknown operator: {}", bin_op.op),
                }
            }
            _ => panic!("Invalid constant expression"),
        }
    }

    fn gen_asm(&self, asm: &'a str) -> String {
        let mut split = asm.split('#');
        let mut string = split.next().unwrap().to_string();
        for part in split {
            let mut var = String::new();
            let mut end = 0;
            for (i, c) in part.chars().enumerate() {
                end = i;
                match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                        var.push(c);
                    }
                    _ => break,
                }
            }
            let loc = self.stack.get(&var);
            match loc {
                Some(loc) => string.push_str(&format!("[rsp+{loc}]")),
                None => panic!("Unknown identifier '{}'", var),
            }
            string.push_str(&part[end..]);
        }
        string
    }

    fn gen_if(&mut self, stmt_if: &'a StmtIf) -> String {
        let mut if_stmt = String::from("; If Start\n");
        let final_label = self.if_end_counter.next();
        for block in stmt_if.blocks.iter() {
            let expr = self.gen_expr(&block.expr);
            let scope = self.gen_scope(&block.stmts);
            let curr_label = self.if_counter.next();
            if_stmt.push_str(&asm!(
                {expr};
                > "test rax, rax";
                > "jz {}", curr_label;
                {scope};
                > "jmp {}", final_label;
                "{}:", curr_label;
            ));
        }
        if let Some(stmts) = &stmt_if.else_block {
            if_stmt.push_str(&self.gen_scope(stmts));
        }
        if_stmt.push_str(&asm!(
            "{}:", final_label;
            "; If End";
        ));
        if_stmt
    }

    fn gen_use(&mut self, stmt_use: &'a StmtUse) -> String {
        if self.imports.contains(&stmt_use.path) {
            panic!("Duplicate import: {}", stmt_use.path);
        }
        self.imports.insert(stmt_use.path.clone());

        String::new()
    }

    fn gen_assign(&mut self, stmt_assign: &'a StmtAssign) -> String {
        let loc = self.stack.get(&stmt_assign.var);
        match loc {
            Some(loc) => {
                asm!(
                    "; Assign Start";
                    {self.gen_expr(&stmt_assign.expr)};
                    > "mov QWORD [rsp+{}], rax", loc;
                    "; Assign End";
                )
            }
            None => panic!("Unknown identifier '{}'", stmt_assign.var),
        }
    }

    fn gen_assign_at(&mut self, stmt_assign_at: &'a StmtAssignAt) -> String {
        asm!(
            "; Assign At Start";
            {self.gen_expr(&stmt_assign_at.var)};
            {self.push("rax")};
            {self.gen_expr(&stmt_assign_at.expr)};
            {self.pop("rcx")};
            > "mov QWORD [rcx], rax";
            "; Assign At End";
        )
    }

    fn gen_decl(&mut self, stmt_decl: &'a StmtDecl) -> String {
        let begin_string = asm!(
            "; Declaration Start";
            {self.gen_expr(&stmt_decl.expr)};
        );
        self.stack.push(stmt_decl.var.clone(), 8);
        asm!(
            {begin_string};
            > "push rax";
            "; Declaration End";
        )
    }

    fn gen_decl_size(&mut self, stmt_decl: &'a StmtDeclSize) -> String {
        let size = self.gen_const_expr(&stmt_decl.size);
        self.stack.push(stmt_decl.var.clone(), size);
        asm!(
            "; Size Declaration Start";
            > "sub rsp, {}", size;
            "; Size Declaration End";
        )
    }

    fn gen_const(&mut self, stmt_decl: &'a StmtConst) -> String {
        let val = self.gen_const_expr(&stmt_decl.expr);
        self.stack.push_const(stmt_decl.var.clone(), val);
        String::new()
    }

    fn gen_ret(&mut self, stmt_ret: &'a StmtRet) -> String {
        asm!(
            "; Return Start";
            {self.gen_expr(&stmt_ret.expr)};
            > "add rsp, {}", self.stack.get_func().unwrap();
            > "ret";
            "; Return End";
        )
    }

    fn gen_exit(&mut self, stmt_exit: &'a StmtExit) -> String {
        asm!(
            "; Exit Start";
            {self.gen_expr(&stmt_exit.expr)};
            > "mov rdi, rax";
            > "mov rax, 60";
            > "syscall";
            "; Exit End";
        )
    }

    fn gen_func(&mut self, stmt_func: &'a StmtFunc) -> String {
        let func_name = format!("{}.{}", self.path, &stmt_func.ident);
        self.global.push_str(&asm!("global {}", &func_name));
        let begin_string = asm!(
            "; Function Start";
            "{}:", func_name;
        );
        self.stack.push_func();
        let mut params = String::new();
        for (i, param) in stmt_func.params.iter().enumerate() {
            self.stack.push(param.clone(), 8);
            let reg = REG_ARGS[i];
            params.push_str(&asm!(> "push {}", reg));
        }
        let scope = self.gen_scope(&stmt_func.stmts);
        for _ in 0..stmt_func.params.len() {
            self.stack.pop();
        }
        self.stack.pop();
        asm!(
            {begin_string};
            {params};
            {scope};
            > "add rsp, {}", stmt_func.params.len() * 8;
            > "ret";
            "; Function End\n";
        )
    }

    fn gen_for(&mut self, stmt_for: &'a StmtFor) -> String {
        let start_label = self.for_counter.next();
        let end_label = self.for_counter.next();
        asm!(
            "; For Start";
            {self.gen_decl(&stmt_for.init)};
            "{}:", start_label;
            {self.gen_expr(&stmt_for.cond)};
            > "test rax, rax";
            > "jz {}", end_label;
            {self.gen_scope(&stmt_for.stmts)};
            {self.gen_assign(&stmt_for.iter)};
            > "jmp {}", start_label;
            "{}:", end_label;
            "; For End";
        )
    }

    fn gen_scope(&mut self, stmts: &'a Vec<Stmt>) -> String {
        self.stack.push_scope();
        let mut stmt_string = String::new();
        for stmt in stmts {
            let stmt = match stmt {
                Stmt::Ret(stmt_ret) => self.gen_ret(stmt_ret),
                Stmt::Exit(stmt_exit) => self.gen_exit(stmt_exit),
                Stmt::Decl(stmt_decl) => self.gen_decl(stmt_decl),
                Stmt::DeclSize(stmt_decl) => self.gen_decl_size(stmt_decl),
                Stmt::Const(stmt_decl) => self.gen_const(stmt_decl),
                Stmt::If(stmt_if) => self.gen_if(stmt_if),
                Stmt::Assign(stmt_assign) => self.gen_assign(stmt_assign),
                Stmt::Func(stmt_func) => {
                    let func = self.gen_func(stmt_func);
                    self.functions.push_str(&func);
                    String::new()
                }
                Stmt::For(stmt_for) => self.gen_for(stmt_for),
                Stmt::Asm(stmt_asm) => {
                    stmt_string.push_str(&self.gen_asm(&stmt_asm.code));
                    String::new()
                }
                Stmt::Expr(stmt_expr) => self.gen_expr(&stmt_expr.expr),
                Stmt::AssignAt(stmt_assign_at) => self.gen_assign_at(stmt_assign_at),
                Stmt::Use(stmt_use) => self.gen_use(stmt_use),
                Stmt::Blank => continue,
            };
            stmt_string.push_str(&stmt);
        }
        let scope_len = self.stack.pop_until_scope();
        asm!(
            "; Scope Start";
            {stmt_string};
            > "add rsp, {}", scope_len;
            "; Scope End";
        )
    }

    pub fn gen(&mut self, prog: &'a Prog, lib: bool) {
        let scope = self.gen_scope(&prog.stmts);
        self.text.push_str(&self.global);
        for ext in &self.ext {
            self.text.push_str(&asm!("extern {}", ext));
        }
        if !lib {
            self.text.push_str(&asm!(
                "global _start";
                "_start:";
            ));
        }
        self.text.push_str(&scope);
        self.text.push_str(&self.functions);
    }

    fn push(&mut self, reg: &str) -> String {
        self.stack.push_none();
        asm!(> "push {}", reg)
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stack.pop();
        asm!(> "pop {}", reg)
    }
}
