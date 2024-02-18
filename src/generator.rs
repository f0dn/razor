use crate::parser::*;
use crate::tokenizer::TokenType::*;

#[derive(PartialEq)]
enum Type {
    Var,
    Func,
}

struct Ident {
    name: String,
    t: Type,
}

pub struct Generator {
    pub text: String,
    functions: String,
    stack: Vec<Option<Ident>>,
    scopes: Vec<usize>,
    num_jmps: usize,
}

impl Generator {
    pub fn new() -> Generator {
        let base = "\
global _start

section .text
_start:\n";
        return Generator {
            text: String::from(base),
            functions: String::new(),
            stack: Vec::new(),
            scopes: Vec::new(),
            num_jmps: 0,
        };
    }

    fn gen_expr(&mut self, expr: Expr) -> String {
        use Expr::*;
        let expr_string = match expr {
            ExprInt(int) => format!("    mov rax, {int}"),
            ExprId(expr_var) => {
                let loc = self.get_loc(&expr_var.name, Type::Var);
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
                    None => panic!(
                        "Unknown identifier '{}' at line {}",
                        expr_var.name, expr_var.line
                    ),
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
                    lhs = self.gen_expr(*bin_op.lhs),
                    push = self.push("rax"),
                    rhs = self.gen_expr(*bin_op.rhs),
                    pop = self.pop("rax")
                )
            }
            ExprCall(expr_call) => {
                format!(
                    "{call}
    call {name}",
                    call = self.gen_expr(expr_call.arg),
                    name = expr_call.name.name
                )
            }
        };
        return format!(
            "\
; Expression Start
{expr_string}
; Expression End",
        );
    }

    fn gen_if(&mut self, stmt_if: StmtIf) -> String {
        let expr = self.gen_expr(stmt_if.expr);
        let scope = self.gen_scope(stmt_if.stmts);
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

    fn gen_assign(&mut self, stmt_assign: StmtAssign) -> String {
        let loc = self.get_loc(&stmt_assign.var.name, Type::Var);
        match loc {
            Some(loc) => {
                return format!(
                    "; Assign Start
{expr}
    mov QWORD [rsp+{offset}], rax
; Assign End",
                    expr = self.gen_expr(stmt_assign.expr),
                    offset = self.stack.len() * 8 - loc - 8
                );
            }
            None => panic!(
                "Unknown identifier '{}' at line {}",
                stmt_assign.var.name, stmt_assign.var.line
            ),
        }
    }

    fn gen_assign_at(&mut self, stmt_assign_at: StmtAssignAt) -> String {
        let loc = self.get_loc(&stmt_assign_at.var.name, Type::Var);
        match loc {
            Some(loc) => {
                return format!(
                    "; Assign At Start
{expr}
    mov rcx, QWORD [rsp+{offset}]
    mov [rcx], rax
; Assign At End",
                    expr = self.gen_expr(stmt_assign_at.expr),
                    offset = self.stack.len() * 8 - loc - 8
                );
            }
            None => panic!(
                "Unknown identifier '{}' at line {}",
                stmt_assign_at.var.name, stmt_assign_at.var.line
            ),
        }
    }

    fn gen_decl(&mut self, stmt_decl: StmtDecl) -> String {
        if self.get_loc(&stmt_decl.var.name, Type::Var).is_some() {
            panic!(
                "Variable '{}' redeclared at line {}",
                stmt_decl.var.name, stmt_decl.var.line
            );
        }
        let begin_string = format!(
            "; Declaration Start
{expr}",
            expr = self.gen_expr(stmt_decl.expr)
        );
        self.stack.push(Some(Ident {
            name: stmt_decl.var.name,
            t: Type::Var,
        }));
        return format!(
            "{begin_string}
    push rax
; Declaration End",
        );
    }

    fn gen_ret(&mut self, stmt_ret: StmtRet) -> String {
        let expr = self.gen_expr(stmt_ret.expr);
        let name = &self.stack[self.get_func()].as_ref().unwrap().name;
        return format!(
            "; Return Start
{expr}
    jmp .return_{name}
; Return End",
        );
    }

    fn gen_exit(&mut self, stmt_exit: StmtExit) -> String {
        return format!(
            "; Exit Start
{expr}
    mov rdi, rax
    mov rax, 60
    syscall
; Exit End",
            expr = self.gen_expr(stmt_exit.expr)
        );
    }

    fn gen_func(&mut self, stmt_func: StmtFunc) -> String {
        let begin_string = format!(
            "; Function Start
{name}:",
            name = stmt_func.ident.name
        );
        self.stack.push(Some(Ident {
            name: stmt_func.ident.name,
            t: Type::Func,
        }));
        self.stack.push(Some(Ident {
            name: stmt_func.arg.name,
            t: Type::Var,
        }));
        let scope = self.gen_scope(stmt_func.stmts);
        self.stack.pop();
        let name = self.stack.pop().unwrap().unwrap().name;
        return format!(
            "{begin_string}
    push rax
{scope}
.return_{name}:
    add rsp, 8
    ret
; Function End"
        );
    }

    fn gen_for(&mut self, stmt_for: StmtFor) -> String {
        let num_jmps = self.num_jmps;
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
            init = self.gen_decl(stmt_for.init),
            expr = self.gen_expr(stmt_for.cond),
            start_num = num_jmps,
            scope = self.gen_scope(stmt_for.stmts),
            iter = self.gen_assign(stmt_for.iter),
            end_num = num_jmps + 1
        );
        self.num_jmps += 2;
        return begin_string;
    }

    fn gen_scope(&mut self, stmts: Vec<Stmt>) -> String {
        self.scopes.push(self.stack.len());
        use Stmt::*;
        let mut stmt_string = String::new();
        for stmt in stmts {
            stmt_string.push('\n');
            stmt_string.push_str(&match stmt {
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
                StmtFor(stmt_for) => self.gen_for(stmt_for),
                StmtAsm(stmt_asm) => stmt_asm.code,
                StmtExpr(stmt_expr) => self.gen_expr(stmt_expr.expr),
                StmtAssignAt(stmt_assign_at) => self.gen_assign_at(stmt_assign_at),
                StmtBlank => continue,
            });
        }
        let scope_len = self.stack.len() - self.scopes.pop().unwrap();
        for _ in 0..scope_len {
            self.stack.pop();
        }
        return format!(
            "; Scope Start
{stmt_string}
    add rsp, {offset}
; Scope End",
            offset = scope_len * 8
        );
    }

    pub fn gen(&mut self, prog: Prog) {
        let scope = self.gen_scope(prog.stmts);
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
                    if &id.name == ident && &id.t == &t {
                        return Some(i * 8);
                    }
                }
                None => continue,
            }
        }
        return None;
    }
}
