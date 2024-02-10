use crate::parser::*;
use crate::tokenizer::TokenType::*;

pub struct Generator {
    pub text: String,
    functions: String,
    vars: Vec<Identifier>,
    funcs: Vec<Identifier>,
    scopes: Vec<usize>,
    stack_size: usize,
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
            vars: Vec::new(),
            funcs: Vec::new(),
            scopes: Vec::new(),
            stack_size: 0,
            num_jmps: 0,
        };
    }

    fn gen_expr(&mut self, expr: Expr) -> String {
        use Expr::*;
        let expr_string = match expr {
            ExprInt(int) => format!("    mov rax, {int}"),
            ExprId(expr_var) => {
                let loc = *Generator::expr(&mut self.vars, &expr_var);
                format!(
                    "    mov rax, QWORD [rsp+{offset}]",
                    offset = self.stack_size - loc - 8
                )
            }
            ExprBinOp(bin_op) => {
                let op = match bin_op.op {
                    Plus => "    add rax, rcx",
                    Dash => "    sub rax, rcx",
                    Star => "    mul rcx",
                    Slash => "    mov edx, 0\n    div rcx",
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
                    Ex => {
                        "    cmp rcx, 0
    setz al
    movzx rax, al"
                    }
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
                let call_string = format!(
                    "{call}
    call {name}",
                    call = self.gen_expr(expr_call.arg),
                    name = expr_call.name.name
                );
                self.stack_size += 8;
                call_string
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
        let loc = *Generator::expr(&mut self.vars, &stmt_assign.var);
        return format!(
            "; Assign Start
{expr}
    mov QWORD [rsp+{offset}], rax
; Assign End",
            expr = self.gen_expr(stmt_assign.expr),
            offset = self.stack_size - loc - 8
        );
    }

    fn gen_decl(&mut self, stmt_decl: StmtDecl) -> String {
        self.decl(&self.vars, &stmt_decl.var);
        let begin_string = format!(
            "; Declaration Start
{expr}",
            expr = self.gen_expr(stmt_decl.expr)
        );
        self.vars.push(Identifier {
            name: stmt_decl.var.name,
            line: self.stack_size,
        });
        return format!(
            "{begin_string}
{push}
; Declaration End",
            push = self.push("rax")
        );
    }

    fn gen_ret(&mut self, stmt_ret: StmtRet) -> String {
        return format!(
            "; Return Start
{expr}
    jmp .return_{name}
; Return End",
            expr = self.gen_expr(stmt_ret.expr),
            name = self.funcs.last().unwrap().name
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
        self.vars.push(Identifier {
            name: stmt_func.arg.name,
            line: self.stack_size,
        });
        self.funcs.push(Identifier {
            name: stmt_func.ident.name.clone(),
            line: self.text.len(),
        });
        let end_string = format!(
            "{begin_string}
{push}
{scope}
.return_{name}:
    add rsp, 8
    ret
; Function End",
            push = self.push("rax"),
            scope = self.gen_scope(stmt_func.stmts),
            name = stmt_func.ident.name
        );
        self.vars.pop();
        self.stack_size -= 16;
        return end_string;
    }

    fn gen_scope(&mut self, stmts: Vec<Stmt>) -> String {
        self.scopes.push(self.vars.len());
        use Stmt::*;
        let mut stmt_string = String::new();
        for stmt in stmts {
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
                StmtBlank => continue,
            });
        }
        let scope_len = self.vars.len() - self.scopes.pop().unwrap();
        self.stack_size -= scope_len * 8;
        for _ in 0..scope_len {
            self.vars.pop();
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
        self.text.push_str(&self.functions);
    }

    fn push(&mut self, reg: &str) -> String {
        self.stack_size += 8;
        return format!("    push {}", reg);
    }

    fn pop(&mut self, reg: &str) -> String {
        self.stack_size -= 8;
        return format!("    pop {}", reg);
    }

    fn decl(&self, idents: &Vec<Identifier>, ident: &Identifier) {
        for declared in idents {
            if &declared.name == &ident.name {
                panic!(
                    "Identifier '{}' already declared at line {}",
                    ident.name, ident.line
                );
            }
        }
    }

    fn expr<'a>(idents: &'a mut Vec<Identifier>, ident: &Identifier) -> &'a mut usize {
        for declared in idents.iter_mut() {
            if &declared.name == &ident.name {
                return &mut declared.line;
            }
        }
        panic!("Unknown identifier '{}' at line {}", ident.name, ident.line);
    }
}
