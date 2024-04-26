use std::collections::HashMap;

use crate::parser::*;
use crate::tokenizer::TokenType::*;

#[derive(PartialEq)]
enum Type {
    Var,
    Func,
}

struct Ident<'a> {
    name: &'a String,
    t: Type,
}

pub struct Generator<'a> {
    pub text: String,
    functions: String,
    stack: Vec<Option<Ident<'a>>>,
    scopes: Vec<usize>,
    num_jmps: usize,
    macros: &'a Vec<Macro>,
    curr_repeat: usize,
    curr_macro_repeat: Option<&'a Vec<MacroRepeat>>,
    ext: String,
    pub links: Vec<&'a String>,
}

impl<'a> Generator<'a> {
    pub fn new(macros: &'a Vec<Macro>) -> Generator<'a> {
        return Generator {
            text: String::from("section .text\n"),
            functions: String::new(),
            stack: Vec::new(),
            scopes: Vec::new(),
            num_jmps: 0,
            macros,
            curr_repeat: 0,
            curr_macro_repeat: None,
            ext: String::new(),
            links: Vec::new(),
        };
    }

    fn gen_expr(&mut self, expr: &'a Expr) -> String {
        use Expr::*;
        let expr_string = match expr {
            ExprInt(int) => format!("    mov rax, {int}"),
            ExprId(expr_var) => {
                let mut ident = expr_var;
                let mut macro_var = None;
                if expr_var.is_macro {
                    macro_var = Some(self.get_macro_var(&expr_var.name));
                    if let Some(MacroVar::Ident(id)) = macro_var {
                        ident = id;
                        macro_var = None
                    }
                }
                if let Some(macro_var) = macro_var {
                    match macro_var {
                        MacroVar::Asm(s) => s.to_string(),
                        MacroVar::Int(int) => format!("    mov rax, {int}"),
                        _ => unreachable!(),
                    }
                } else {
                    let loc = self.get_loc(&ident.name, Type::Var);
                    match loc {
                        Some(loc) => {
                            if expr_var.is_ref {
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
            }
            ExprBinOp(bin_op) => {
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
                    Ex => {
                        "    cmp rcx, 0
    setz al
    movzx rax, al"
                    }
                    At => "    mov rax, [rcx]",
                    _ => panic!("Unknown operator: {}", bin_op.op.val()),
                };
                format!(
                    "{lhs}
{push}
{rhs}
    mov rcx, rax
{pop}
{op}",
                    lhs = self.gen_expr(&*bin_op.lhs),
                    push = self.push("rax"),
                    rhs = self.gen_expr(&*bin_op.rhs),
                    pop = self.pop("rax")
                )
            }
            ExprCall(expr_call) => {
                format!(
                    "{call}
    call {name}",
                    call = self.gen_expr(&expr_call.arg),
                    name = expr_call.name.name
                )
            }
            ExprAsm(asm) => asm.to_string(),
            ExprMacro(expr_macro) => {
                self.curr_macro_repeat = Some(&expr_macro.cont);
                let scope = self.gen_repeat(&self.macros[expr_macro.mac].stmts);
                self.curr_macro_repeat = None;
                return scope;
            }
        };
        return format!(
            "\
; Expression Start
{expr_string}
; Expression End",
        );
    }

    fn gen_repeat(&mut self, stmt_repeat: &'a Vec<Stmt>) -> String {
        let mut num_repeats = 0;
        match self.curr_macro_repeat {
            Some(repeat) => {
                let mut str = String::new();
                for i in 0..repeat.len() {
                    self.curr_repeat = i;
                    use Stmt::*;
                    for stmt in stmt_repeat {
                        str.push('\n');
                        let stmt = match stmt {
                            StmtRet(stmt_ret) => self.gen_ret(stmt_ret),
                            StmtExit(stmt_exit) => self.gen_exit(stmt_exit),
                            StmtDecl(stmt_decl) => self.gen_decl(stmt_decl),
                            StmtIf(stmt_if) => self.gen_if(stmt_if),
                            StmtAssign(stmt_assign) => self.gen_assign(stmt_assign),
                            StmtFunc(stmt_func) => {
                                let func = self.gen_func(stmt_func);
                                self.functions.push_str(&func);
                                String::new()
                            }
                            StmtFor(stmt_for) => self.gen_for(&stmt_for),
                            StmtAsm(stmt_asm) => {
                                str.push_str(&stmt_asm.code);
                                String::new()
                            }
                            StmtExpr(stmt_expr) => self.gen_expr(&stmt_expr.expr),
                            StmtAssignAt(stmt_assign_at) => self.gen_assign_at(stmt_assign_at),
                            StmtMRepeat(stmt_repeat) => {
                                self.curr_macro_repeat = Some(&repeat[i].repeats[num_repeats]);
                                num_repeats += 1;
                                self.gen_repeat(&stmt_repeat.stmts)
                            }
                            StmtUse(stmt_use) => self.gen_use(&stmt_use),
                            StmtBlank => continue,
                        };
                        str.push_str(&stmt);
                    }
                }
                self.curr_macro_repeat = Some(repeat);
                return str;
            }
            None => panic!("No current macro"),
        }
    }

    fn get_macro_var(&self, ident: &'a str) -> &'a MacroVar {
        let vars: &HashMap<String, MacroVar>;
        match &self.curr_macro_repeat {
            Some(map) => vars = &map[self.curr_repeat].vars,
            None => panic!("No current macro"),
        }
        match vars.get(ident) {
            Some(var) => return var,
            None => panic!("Unknown macro variable '{}'", ident),
        }
    }

    fn get_ident(&self, ident: &'a Identifier) -> &'a Identifier {
        if !ident.is_macro {
            return ident;
        }
        let macro_var = self.get_macro_var(&ident.name);
        match &macro_var {
            MacroVar::Ident(ident) => return ident,
            _ => panic!("Expected identifier"),
        };
    }

    fn gen_if(&mut self, stmt_if: &'a StmtIf) -> String {
        let expr = self.gen_expr(&stmt_if.expr);
        let scope = self.gen_scope(&stmt_if.stmts);
        let string = format!(
            "; If Start
{expr}
    test rax, rax
    jz .if_{num_jmps}
{scope}
.if_{num_jmps}:
; If End",
            num_jmps = self.num_jmps,
        );
        self.num_jmps += 1;
        return string;
    }

    fn gen_use(&mut self, stmt_use: &'a StmtUse) -> String {
        self.ext
            .push_str(&format!("extern {}\n", stmt_use.ident.name));
        if !self.links.contains(&&stmt_use.path) {
            self.links.push(&stmt_use.path);
        }
        return String::new();
    }

    fn gen_assign(&mut self, stmt_assign: &'a StmtAssign) -> String {
        let loc = self.get_loc(&self.get_ident(&stmt_assign.var).name, Type::Var);
        match loc {
            Some(loc) => {
                return format!(
                    "; Assign Start
{expr}
    mov QWORD [rsp+{offset}], rax
; Assign End",
                    expr = self.gen_expr(&stmt_assign.expr),
                    offset = self.stack.len() * 8 - loc - 8
                );
            }
            None => panic!(
                "Unknown identifier '{}' at line {}",
                stmt_assign.var.name, stmt_assign.var.line
            ),
        }
    }

    fn gen_assign_at(&mut self, stmt_assign_at: &'a StmtAssignAt) -> String {
        let loc = self.get_loc(&self.get_ident(&stmt_assign_at.var).name, Type::Var);
        match loc {
            Some(loc) => {
                return format!(
                    "; Assign At Start
{expr}
    mov rcx, QWORD [rsp+{offset}]
    mov [rcx], rax
; Assign At End",
                    expr = self.gen_expr(&stmt_assign_at.expr),
                    offset = self.stack.len() * 8 - loc - 8
                );
            }
            None => panic!(
                "Unknown identifier '{}' at line {}",
                stmt_assign_at.var.name, stmt_assign_at.var.line
            ),
        }
    }

    fn gen_decl(&mut self, stmt_decl: &'a StmtDecl) -> String {
        if self.get_loc(&stmt_decl.var.name, Type::Var).is_some() {
            panic!(
                "Variable '{}' redeclared at line {}",
                stmt_decl.var.name, stmt_decl.var.line
            );
        }
        let begin_string = format!(
            "; Declaration Start
{expr}",
            expr = self.gen_expr(&stmt_decl.expr)
        );
        self.stack.push(Some(Ident {
            name: &self.get_ident(&stmt_decl.var).name,
            t: Type::Var,
        }));
        return format!(
            "{begin_string}
    push rax
; Declaration End",
        );
    }

    fn gen_ret(&mut self, stmt_ret: &'a StmtRet) -> String {
        let expr = self.gen_expr(&stmt_ret.expr);
        let name = &self.stack[self.get_func()].as_ref().unwrap().name;
        return format!(
            "; Return Start
{expr}
    jmp .return_{name}
; Return End",
        );
    }

    fn gen_exit(&mut self, stmt_exit: &'a StmtExit) -> String {
        return format!(
            "; Exit Start
{expr}
    mov rdi, rax
    mov rax, 60
    syscall
; Exit End",
            expr = self.gen_expr(&stmt_exit.expr)
        );
    }

    fn gen_func(&mut self, stmt_func: &'a StmtFunc) -> String {
        self.ext.push_str(&format!(
            "global {}\n",
            self.get_ident(&stmt_func.ident).name
        ));
        let begin_string = format!(
            "; Function Start
{name}:",
            name = self.get_ident(&stmt_func.ident).name
        );
        self.stack.push(Some(Ident {
            name: &self.get_ident(&stmt_func.ident).name,
            t: Type::Func,
        }));
        self.stack.push(Some(Ident {
            name: &stmt_func.arg.name,
            t: Type::Var,
        }));
        let (first, second) = self.gen_scope_split(&stmt_func.stmts);
        self.stack.pop();
        let name = self.stack.pop().unwrap().unwrap().name;
        return format!(
            "{begin_string}
    push rax
{first}
.return_{name}:
{second}
    add rsp, 8
    ret
; Function End"
        );
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
        return begin_string;
    }

    fn gen_scope(&mut self, stmts: &'a Vec<Stmt>) -> String {
        let (first, second) = self.gen_scope_split(stmts);
        return first + &second;
    }

    fn gen_scope_split(&mut self, stmts: &'a Vec<Stmt>) -> (String, String) {
        self.scopes.push(self.stack.len());
        use Stmt::*;
        let mut stmt_string = String::new();
        for stmt in stmts {
            stmt_string.push('\n');
            let stmt = match stmt {
                StmtRet(stmt_ret) => self.gen_ret(stmt_ret),
                StmtExit(stmt_exit) => self.gen_exit(stmt_exit),
                StmtDecl(stmt_decl) => self.gen_decl(stmt_decl),
                StmtIf(stmt_if) => self.gen_if(stmt_if),
                StmtAssign(stmt_assign) => self.gen_assign(stmt_assign),
                StmtFunc(stmt_func) => {
                    let func = self.gen_func(stmt_func);
                    self.functions.push_str(&func);
                    String::new()
                }
                StmtFor(stmt_for) => self.gen_for(&stmt_for),
                StmtAsm(stmt_asm) => {
                    stmt_string.push_str(&stmt_asm.code);
                    String::new()
                }
                StmtExpr(stmt_expr) => self.gen_expr(&stmt_expr.expr),
                StmtAssignAt(stmt_assign_at) => self.gen_assign_at(stmt_assign_at),
                StmtMRepeat(stmt_repeat) => self.gen_repeat(&stmt_repeat.stmts),
                StmtUse(stmt_use) => self.gen_use(stmt_use),
                StmtBlank => continue,
            };
            stmt_string.push_str(&stmt);
        }
        let scope_len = self.stack.len() - self.scopes.pop().unwrap();
        for _ in 0..scope_len {
            self.stack.pop();
        }
        return (
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
        );
    }

    pub fn gen(&mut self, prog: &'a Prog, lib: bool) {
        let scope = self.gen_scope(&prog.stmts);
        self.text.push_str(&self.ext);
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
        return format!("    push {}", reg);
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stack.pop();
        return format!("    pop {}", reg);
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
        return 0;
    }

    fn get_loc(&self, ident: &String, t: Type) -> Option<usize> {
        let func = self.get_func();
        for i in func..self.stack.len() {
            match &self.stack[i] {
                Some(id) => {
                    if id.name == ident && &id.t == &t && id.name != "_" {
                        return Some(i * 8);
                    }
                }
                None => continue,
            }
        }
        return None;
    }
}
