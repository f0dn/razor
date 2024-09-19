use std::collections::HashSet;
use std::hash::{DefaultHasher, Hash, Hasher};

use crate::parser::*;
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
    file: &'a String,
    functions: String,
    stack: Vec<Option<Ident>>,
    scopes: Vec<usize>,
    num_jmps: usize,
    ext: HashSet<String>,
    global: String,
    imports: HashSet<Vec<String>>,
}

impl<'a> Generator<'a> {
    pub fn new(file: &'a String) -> Generator<'a> {
        Generator {
            text: String::from("section .text\n"),
            file,
            functions: String::new(),
            stack: Vec::new(),
            scopes: Vec::new(),
            num_jmps: 0,
            ext: HashSet::new(),
            global: String::new(),
            imports: HashSet::new(),
        }
    }

    pub fn links(&self) -> HashSet<String> {
        self.imports.iter().map(|path| self.to_path(path)).collect()
    }

    fn to_path(&self, path: &[String]) -> String {
        let mut path = path.join("/");
        path.push_str(".rz");
        path
    }

    fn hash_func(&self, path: &String, name: &String) -> String {
        let path = format!("{}.{}", path, name);
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        format!("func_{}", hasher.finish())
    }

    fn gen_literal(&mut self, literal: &'a ExprLiteral) -> String {
        match literal {
            ExprLiteral::Asm(asm) => self.gen_asm(asm),
            ExprLiteral::Int(int) => format!("    mov rax, {}", int),
            ExprLiteral::Char(char) => format!("    mov rax, {}", *char as u8),
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
                    "    mov byte [rax+{offset}], 0",
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
                                "    lea rax, [rsp+{offset}]",
                                offset = self.stack.len() * 8 - loc - 8
                            )
                        } else {
                            format!(
                                "    mov rax, QWORD [rsp+{offset}]",
                                offset = self.stack.len() * 8 - loc - 8
                            )
                        }
                    }
                    None => {
                        panic!("Unknown identifier '{}' at line {}", ident.name, ident.line)
                    }
                }
            }
            Expr::BinOp(bin_op) => {
                let op = match bin_op.op {
                    Plus => "    add rax, rcx",
                    Dash => "    sub rax, rcx",
                    Star => "    mul rcx",
                    Slash => {
                        "    mov edx, 0
    div rcx"
                    }
                    Per => {
                        "    mov edx, 0
    div rcx
    mov rax, rdx"
                    }
                    DEq => {
                        "    cmp rax, rcx
    setz al
    movzx rax, al"
                    }
                    DPipe => {
                        "    cmp rax, 0
    setz al
    cmp rcx, 0
    setz ah
    and al, ah
    xor al, 0b00000001
    movzx rax, al"
                    }
                    DAmp => {
                        "    cmp rax, 0
    setz al
    cmp rcx, 0
    setz ah
    or al, ah
    xor al, 0b00000001
    movzx rax, al"
                    }
                    Lt => {
                        "    cmp rax, rcx
    setc al
    movzx rax, al"
                    }
                    Gt => {
                        "    cmp rax, rcx
    setz al
    cmc
    setc ah
    xor al, ah
    movzx rax, al"
                    }
                    Ex => {
                        "    cmp rcx, 0
    setz al
    movzx rax, al"
                    }
                    At => "    mov rax, [rcx]",
                    _ => panic!("Unknown operator: {}", bin_op.op),
                };
                format!(
                    "{lhs}
{push}
{rhs}
    mov rcx, rax
{pop}
{op}",
                    lhs = self.gen_expr(&bin_op.lhs),
                    push = self.push("rax"),
                    rhs = self.gen_expr(&bin_op.rhs),
                    pop = self.pop("rax")
                )
            }
            Expr::Call(expr_call) => {
                let len = expr_call.path.len();
                let name = &expr_call.path[len - 1].name;
                let path = &expr_call
                    .path
                    .iter()
                    .map(|ident| ident.name.clone())
                    .collect::<Vec<String>>()[..len - 1];
                let hashed_name = if path.is_empty() {
                    self.hash_func(self.file, name)
                } else {
                    let mut hashed_name = None;
                    for import in &self.imports {
                        if import[import.len() - path.len()..] == *path {
                            hashed_name = Some(self.hash_func(&self.to_path(import), name));
                            break;
                        }
                    }
                    match hashed_name {
                        Some(hashed_name) => hashed_name,
                        None => panic!("Unknown function '{}'", name),
                    }
                };

                self.ext.insert(hashed_name.clone());
                let num_args = expr_call.args.len();
                if num_args > REG_ARGS.len() {
                    panic!("Too many arguments for function '{}'", name);
                }
                let mut args = String::new();
                for arg in &expr_call.args {
                    args.push_str(&self.gen_expr(arg));
                    args.push('\n');
                    args.push_str(&self.push("rax"));
                    args.push('\n');
                }
                for reg in REG_ARGS.iter().take(num_args).rev() {
                    args.push_str(&self.pop(reg));
                    args.push('\n');
                }
                format!(
                    "{args}
    call {hashed_name}"
                )
            }
            Expr::Stmts(stmts) => self.gen_scope(stmts),
        };
        format!(
            "\
; Expression Start
{expr_string}
; Expression End",
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
        let mut if_stmt = String::from("; If Start");
        let final_jump = self.num_jmps + stmt_if.blocks.len();
        for block in stmt_if.blocks.iter() {
            if_stmt.push_str(&format!(
                "
{expr}
    test rax, rax
    jz .if_{jump_num}
{scope}
    jmp .if_{final_jump}
.if_{jump_num}:",
                expr = self.gen_expr(&block.expr),
                scope = self.gen_scope(&block.stmts),
                jump_num = self.num_jmps,
            ));
            self.num_jmps += 1;
        }
        if let Some(stmts) = &stmt_if.else_block {
            if_stmt.push_str(&format!("\n{scope}", scope = self.gen_scope(stmts)));
        }
        if_stmt.push_str(&format!(
            "
.if_{final_jump}:
; If End"
        ));
        if_stmt
    }

    fn gen_use(&mut self, stmt_use: &'a StmtUse) -> String {
        let path = stmt_use
            .path
            .iter()
            .map(|id| id.name.clone())
            .collect::<Vec<String>>();
        if self.imports.contains(&path) {
            panic!("Duplicate import: {}", path.join("/"));
        }
        self.imports.insert(path);

        String::new()
    }

    fn gen_assign(&mut self, stmt_assign: &'a StmtAssign) -> String {
        let loc = self.get_loc(&stmt_assign.var.name, Type::Var);
        match loc {
            Some(loc) => {
                format!(
                    "; Assign Start
{expr}
    mov QWORD [rsp+{offset}], rax
; Assign End",
                    expr = self.gen_expr(&stmt_assign.expr),
                    offset = self.stack.len() * 8 - loc - 8
                )
            }
            None => panic!(
                "Unknown identifier '{}' at line {}",
                stmt_assign.var.name, stmt_assign.var.line
            ),
        }
    }

    fn gen_assign_at(&mut self, stmt_assign_at: &'a StmtAssignAt) -> String {
        let var = self.gen_expr(&stmt_assign_at.var);
        format!(
            "; Assign At Start
{var}
{push}
{expr}
{pop}
    mov QWORD [rcx], rax
; Assign At End",
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
            name: stmt_decl.var.name.clone(),
            t: Type::Var,
        }));
        format!(
            "{begin_string}
    push rax
; Declaration End",
        )
    }

    fn gen_ret(&mut self, stmt_ret: &'a StmtRet) -> String {
        let expr = self.gen_expr(&stmt_ret.expr);
        let name = &self.stack[self.get_func()].as_ref().unwrap().name;
        format!(
            "; Return Start
{expr}
    jmp .return_{name}
; Return End",
        )
    }

    fn gen_exit(&mut self, stmt_exit: &'a StmtExit) -> String {
        format!(
            "; Exit Start
{expr}
    mov rdi, rax
    mov rax, 60
    syscall
; Exit End",
            expr = self.gen_expr(&stmt_exit.expr)
        )
    }

    fn gen_func(&mut self, stmt_func: &'a StmtFunc) -> String {
        let hashed_name = self.hash_func(self.file, &stmt_func.ident.name);
        self.global.push_str(&format!("global {}\n", &hashed_name));
        let begin_string = format!(
            "; Function Start
{hashed_name}:"
        );
        self.stack.push(Some(Ident {
            name: hashed_name,
            t: Type::Func,
        }));
        let mut params = String::new();
        for (i, param) in stmt_func.params.iter().enumerate() {
            self.stack.push(Some(Ident {
                name: param.name.clone(),
                t: Type::Var,
            }));
            let reg = REG_ARGS[i];
            params.push_str(&format!("    push {reg}\n"));
        }
        let (first, second) = self.gen_scope_split(&stmt_func.stmts);
        for _ in 0..stmt_func.params.len() {
            self.stack.pop();
        }
        let hashed_name = self.stack.pop().unwrap().unwrap().name;
        format!(
            "{begin_string}
{params}
{first}
.return_{hashed_name}:
{second}
    add rsp, {offset}
    ret
; Function End",
            offset = stmt_func.params.len() * 8
        )
    }

    fn gen_for(&mut self, stmt_for: &'a StmtFor) -> String {
        let num_jmps = self.num_jmps;
        self.num_jmps += 2;
        let begin_string = format!(
            "; For Start
{init}
.for_{start_num}:
{expr}
    test rax, rax
    jz .for_{end_num}
{scope}
{iter}
    jmp .for_{start_num}
.for_{end_num}:
; For End",
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
            stmt_string.push('\n');
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
                "
add rsp, {offset}
; Scope End",
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
        self.text.push('\n');
        if !lib {
            self.text.push_str(
                "global _start
_start:\n",
            );
        }
        self.text.push_str(&scope);
        self.text.push('\n');
        self.text.push_str(&self.functions);
    }

    fn push(&mut self, reg: &str) -> String {
        self.stack.push(None);
        format!("    push {}", reg)
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stack.pop();
        format!("    pop {}", reg)
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
