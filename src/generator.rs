use std::collections::HashSet;

use crate::parser::*;
use crate::path::UsePath;
use crate::tokenizer::TokenType::*;

const REG_ARGS: [&str; 6] = ["rdi", "rsi", "rdx", "r10", "r8", "r9"];

#[derive(PartialEq)]
enum Type {
    Var,
    Func,
}

struct Ident {
    name: String,
    t: Type,
}

pub struct Generator<'a> {
    pub text: String,
    path: &'a UsePath,
    functions: String,
    stack: Vec<Option<Ident>>,
    scopes: Vec<usize>,
    num_jmps: usize,
    ext: HashSet<String>,
    global: String,
    imports: HashSet<UsePath>,
}

impl<'a> Generator<'a> {
    pub fn new(file: &'a UsePath) -> Generator<'a> {
        Generator {
            text: String::from("section .text\n"),
            path: file,
            functions: String::new(),
            stack: Vec::new(),
            scopes: Vec::new(),
            num_jmps: 0,
            ext: HashSet::new(),
            global: String::new(),
            imports: HashSet::new(),
        }
    }

    pub fn links(&self) -> &HashSet<UsePath> {
        &self.imports
    }

    fn gen_literal(&mut self, literal: &'a ExprLiteral) -> String {
        match literal {
            ExprLiteral::Asm(asm) => self.gen_asm(asm),
            ExprLiteral::Int(int) => format!("    mov rax, {}\n", int),
            ExprLiteral::Char(char) => format!("    mov rax, {}\n", *char as u8),
            // TODO use stack strings
            ExprLiteral::Str(string) => {
                let mut asm = format!(
                    "    mov rax, 9
    mov rsi, {total_len}
    mov rdx, 3
    mov r10, 33
    mov r8, 255
    mov r9, 0
    syscall
    mov QWORD [rax], {string_len}
    mov QWORD [rax+8], {string_len}\n",
                    total_len = string.len() + 17,
                    string_len = string.len(),
                );
                for (i, c) in string.chars().enumerate() {
                    asm.push_str(&format!(
                        "    mov byte [rax+{offset}], {char}\n",
                        offset = i + 16,
                        char = c as u8
                    ));
                }
                asm.push_str(&format!(
                    "    mov byte [rax+{offset}], 0\n",
                    offset = string.len() + 16
                ));
                asm
            }
        }
    }

    fn gen_expr(&mut self, expr: &'a Expr) -> String {
        let expr_string = match expr {
            Expr::Literal(literal) => self.gen_literal(literal),
            Expr::Id(ident) => {
                let loc = self.get_loc(&ident.name, Type::Var);
                match loc {
                    Some(loc) => {
                        if ident.is_ref {
                            format!(
                                "    lea rax, [rsp+{offset}]\n",
                                offset = self.stack.len() * 8 - loc - 8
                            )
                        } else {
                            format!(
                                "    mov rax, QWORD [rsp+{offset}]\n",
                                offset = self.stack.len() * 8 - loc - 8
                            )
                        }
                    }
                    None => {
                        panic!("Unknown identifier '{}'", ident.name)
                    }
                }
            }
            Expr::BinOp(bin_op) => {
                let op = match bin_op.op {
                    Plus => "    add rax, rcx\n",
                    Dash => "    sub rax, rcx\n",
                    Star => "    mul rcx\n",
                    Slash => {
                        "    mov edx, 0
    div rcx\n"
                    }
                    Per => {
                        "    mov edx, 0
    div rcx
    mov rax, rdx\n"
                    }
                    DEq => {
                        "    cmp rax, rcx
    setz al
    movzx rax, al\n"
                    }
                    DPipe => {
                        "    cmp rax, 0
    setz al
    cmp rcx, 0
    setz ah
    and al, ah
    xor al, 0b00000001
    movzx rax, al\n"
                    }
                    DAmp => {
                        "    cmp rax, 0
    setz al
    cmp rcx, 0
    setz ah
    or al, ah
    xor al, 0b00000001
    movzx rax, al\n"
                    }
                    Lt => {
                        "    cmp rax, rcx
    setc al
    movzx rax, al\n"
                    }
                    Gt => {
                        "    cmp rax, rcx
    setz al
    cmc
    setc ah
    xor al, ah
    movzx rax, al\n"
                    }
                    Ex => {
                        "    cmp rcx, 0
    setz al
    movzx rax, al\n"
                    }
                    At => "    mov rax, [rcx]\n",
                    _ => panic!("Unknown operator: {}", bin_op.op),
                };
                format!(
                    "{lhs}{push}{rhs}    mov rcx, rax
{pop}{op}",
                    lhs = self.gen_expr(&bin_op.lhs),
                    push = self.push("rax"),
                    rhs = self.gen_expr(&bin_op.rhs),
                    pop = self.pop("rax")
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
                format!(
                    "{args}    call {func_name}\n"
                )
            }
            Expr::Stmts(stmts) => self.gen_scope(stmts),
        };
        format!(
            "; Expression Start
{expr_string}; Expression End\n",
        )
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
            let loc = self.get_loc(&var, Type::Var);
            match loc {
                Some(loc) => {
                    string.push_str(&format!(
                        "[rsp+{offset}]",
                        offset = self.stack.len() * 8 - loc - 8
                    ));
                }
                None => panic!("Unknown identifier '{}'", var),
            }
            string.push_str(&part[end..]);
        }
        string
    }

    fn gen_if(&mut self, stmt_if: &'a StmtIf) -> String {
        let mut if_stmt = String::from("; If Start\n");
        let final_jump = self.num_jmps + stmt_if.blocks.len();
        for block in stmt_if.blocks.iter() {
            if_stmt.push_str(&format!(
                "{expr}    test rax, rax
    jz .if_{jump_num}
{scope}    jmp .if_{final_jump}
.if_{jump_num}:\n",
                expr = self.gen_expr(&block.expr),
                scope = self.gen_scope(&block.stmts),
                jump_num = self.num_jmps,
            ));
            self.num_jmps += 1;
        }
        if let Some(stmts) = &stmt_if.else_block {
            if_stmt.push_str(&self.gen_scope(stmts));
        }
        if_stmt.push_str(&format!(
            ".if_{final_jump}:
; If End\n"
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
        let loc = self.get_loc(&stmt_assign.var, Type::Var);
        match loc {
            Some(loc) => {
                format!(
                    "; Assign Start
{expr}    mov QWORD [rsp+{offset}], rax
; Assign End\n",
                    expr = self.gen_expr(&stmt_assign.expr),
                    offset = self.stack.len() * 8 - loc - 8
                )
            }
            None => panic!("Unknown identifier '{}'", stmt_assign.var),
        }
    }

    fn gen_assign_at(&mut self, stmt_assign_at: &'a StmtAssignAt) -> String {
        let var = self.gen_expr(&stmt_assign_at.var);
        format!(
            "; Assign At Start
{var}{push}{expr}{pop}    mov QWORD [rcx], rax
; Assign At End\n",
            push = self.push("rax"),
            expr = self.gen_expr(&stmt_assign_at.expr),
            pop = self.pop("rcx")
        )
    }

    fn gen_decl(&mut self, stmt_decl: &'a StmtDecl) -> String {
        let begin_string = format!(
            "; Declaration Start
{expr}",
            expr = self.gen_expr(&stmt_decl.expr)
        );
        self.stack.push(Some(Ident {
            name: stmt_decl.var.clone(),
            t: Type::Var,
        }));
        format!(
            "{begin_string}    push rax
; Declaration End\n",
        )
    }

    fn gen_ret(&mut self, stmt_ret: &'a StmtRet) -> String {
        let expr = self.gen_expr(&stmt_ret.expr);
        format!(
            "; Return Start
{expr}    add rsp, {offset}
    ret
; Return End\n",
            offset = (self.stack.len() - self.get_func() - 1) * 8,
        )
    }

    fn gen_exit(&mut self, stmt_exit: &'a StmtExit) -> String {
        format!(
            "; Exit Start
{expr}    mov rdi, rax
    mov rax, 60
    syscall
; Exit End\n",
            expr = self.gen_expr(&stmt_exit.expr)
        )
    }

    fn gen_func(&mut self, stmt_func: &'a StmtFunc) -> String {
        let func_name = format!("{}.{}", self.path, &stmt_func.ident);
        self.global.push_str(&format!("global {}\n", &func_name));
        let begin_string = format!(
            "; Function Start
{func_name}:\n"
        );
        self.stack.push(Some(Ident {
            name: func_name,
            t: Type::Func,
        }));
        let mut params = String::new();
        for (i, param) in stmt_func.params.iter().enumerate() {
            self.stack.push(Some(Ident {
                name: param.clone(),
                t: Type::Var,
            }));
            let reg = REG_ARGS[i];
            params.push_str(&format!("    push {reg}\n"));
        }
        let (first, second) = self.gen_scope_split(&stmt_func.stmts);
        for _ in 0..stmt_func.params.len() {
            self.stack.pop();
        }
        self.stack.pop();
        format!(
            "{begin_string}{params}{first}{second}    add rsp, {offset}
    ret
; Function End\n",
            offset = stmt_func.params.len() * 8
        )
    }

    fn gen_for(&mut self, stmt_for: &'a StmtFor) -> String {
        let num_jmps = self.num_jmps;
        self.num_jmps += 2;
        let begin_string = format!(
            "; For Start
{init}.for_{start_num}:
{expr}    test rax, rax
    jz .for_{end_num}
{scope}{iter}    jmp .for_{start_num}
.for_{end_num}:
; For End\n",
            init = self.gen_decl(&stmt_for.init),
            expr = self.gen_expr(&stmt_for.cond),
            start_num = num_jmps,
            scope = self.gen_scope(&stmt_for.stmts),
            iter = self.gen_assign(&stmt_for.iter),
            end_num = num_jmps + 1
        );
        begin_string
    }

    fn gen_scope(&mut self, stmts: &'a Vec<Stmt>) -> String {
        let (first, second) = self.gen_scope_split(stmts);
        first + &second
    }

    fn gen_scope_split(&mut self, stmts: &'a Vec<Stmt>) -> (String, String) {
        self.scopes.push(self.stack.len());
        let mut stmt_string = String::new();
        for stmt in stmts {
            let stmt = match stmt {
                Stmt::Ret(stmt_ret) => self.gen_ret(stmt_ret),
                Stmt::Exit(stmt_exit) => self.gen_exit(stmt_exit),
                Stmt::Decl(stmt_decl) => self.gen_decl(stmt_decl),
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
        let scope_len = self.stack.len() - self.scopes.pop().unwrap();
        for _ in 0..scope_len {
            self.stack.pop();
        }
        (
            format!(
                "; Scope Start
{stmt_string}"
            ),
            format!(
                "    add rsp, {offset}
; Scope End\n",
                offset = scope_len * 8
            ),
        )
    }

    pub fn gen(&mut self, prog: &'a Prog, lib: bool) {
        let scope = self.gen_scope(&prog.stmts);
        self.text.push_str(&self.global);
        for ext in &self.ext {
            self.text.push_str(&format!("extern {}\n", ext));
        }
        if !lib {
            self.text.push_str(
                "global _start
_start:\n",
            );
        }
        self.text.push_str(&scope);
        self.text.push_str(&self.functions);
    }

    fn push(&mut self, reg: &str) -> String {
        self.stack.push(None);
        format!("    push {}\n", reg)
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stack.pop();
        format!("    pop {}\n", reg)
    }

    fn get_func(&self) -> usize {
        for i in (0..self.stack.len()).rev() {
            match &self.stack[i] {
                Some(id) => {
                    if id.t == Type::Func {
                        return i;
                    }
                }
                None => continue,
            }
        }
        0
    }

    fn get_loc(&self, ident: &String, t: Type) -> Option<usize> {
        let func = self.get_func();
        for i in (func..self.stack.len()).rev() {
            match &self.stack[i] {
                Some(id) => {
                    if id.name == *ident && id.t == t && id.name != "_" {
                        return Some(i * 8);
                    }
                }
                None => continue,
            }
        }
        None
    }
}
