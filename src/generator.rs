use crate::parser::*;
use crate::tokenizer::TokenType::*;

pub struct Generator {
    pub string: String,
    vars: Vec<(String, usize)>,
    scopes: Vec<usize>,
    stack_size: usize,
    num_jmps: usize,
}

macro_rules! push {
    ($self:ident, $($($str:literal)? $(#$expr:expr)?),+) => {
        $(
            $(
                $self.string.push_str($str);
            )?
            $(
                $self.string.push_str($expr.to_string().as_str());
            )?
        )+
        $self.string.push_str("\n");
    };
}

impl Generator {
    pub fn new() -> Generator {
        let base = "\
global _start

section .text
_start:\n";
        return Generator {
            string: String::from(base),
            vars: Vec::new(),
            scopes: Vec::new(),
            stack_size: 0,
            num_jmps: 0,
        };
    }

    fn gen_expr(&mut self, expr: Expr) {
        push!(self, "; Expression Start");
        use Expr::*;
        match expr {
            ExprInt(int) => {
                push!(self, "    mov rax, ", #int);
            }
            ExprId(expr_var) => {
                let loc = *self.expr(&expr_var);
                push!(self, "    mov rax, QWORD [rsp+", #self.stack_size - loc - 8, "]");
            }
            ExprBinOp(bin_op) => {
                self.gen_expr(*bin_op.lhs);
                self.push("rax");
                self.gen_expr(*bin_op.rhs);
                push!(self, "    mov rcx, rax");
                self.pop("rax");
                match bin_op.op {
                    Plus => {
                        push!(self, "    add rax, rcx");
                    }
                    Dash => {
                        push!(self, "    sub rax, rcx");
                    }
                    Star => {
                        push!(self, "    mul rcx");
                    }
                    Slash => {
                        push!(self, "    mov edx, 0");
                        push!(self, "    div rcx");
                    }
                    _ => panic!("Unknown operator: {}", bin_op.op.val()),
                }
            }
        }
        push!(self, "; Expression End");
    }

    fn gen_if(&mut self, stmt_if: StmtIf) {
        push!(self, "; If Start");
        self.gen_expr(stmt_if.expr);
        push!(self, "    test rax, rax");
        push!(self, "    jz .if_", #self.num_jmps);
        self.gen_scope(stmt_if.stmts);
        push!(self, ".if_", #self.num_jmps, ":");
        self.num_jmps += 1;
        push!(self, "; If End");
    }

    fn gen_assign(&mut self, stmt_assign: StmtAssign) {
        push!(self, "; Assign Start");
        let loc = *self.expr(&stmt_assign.var);
        self.gen_expr(stmt_assign.expr);
        push!(self, "    mov QWORD [rsp+", #self.stack_size - loc - 8, "], rax");
        push!(self, "; Assign End");
    }

    fn gen_decl(&mut self, stmt_decl: StmtDecl) {
        push!(self, "; Declaration Start");
        self.decl(&stmt_decl.var);
        self.gen_expr(stmt_decl.expr);
        self.vars.push((stmt_decl.var.name, self.stack_size));
        self.push("rax");
        push!(self, "; Declaration End");
    }

    fn gen_ret(&mut self, stmt_ret: StmtRet) {
        push!(self, "; Return Start");
        self.gen_expr(stmt_ret.expr);
        push!(self, "    mov rdi, rax");
        push!(self, "    mov rax, 60");
        push!(self, "    syscall");
        push!(self, "; Return End");
    }

    fn gen_scope(&mut self, stmts: Vec<Stmt>) {
        push!(self, "; Scope Start");
        self.scopes.push(self.vars.len());
        use Stmt::*;
        for stmt in stmts {
            match stmt {
                StmtRet(stmt_ret) => self.gen_ret(stmt_ret),
                StmtDecl(stmt_decl) => self.gen_decl(stmt_decl),
                StmtIf(stmt_if) => self.gen_if(stmt_if),
                StmtAssign(stmt_assign) => self.gen_assign(stmt_assign),
                StmtBlank => continue,
            }
        }
        let scope_len = self.vars.len() - self.scopes.pop().unwrap();
        push!(self, "    add rsp, ", #scope_len * 8);
        self.stack_size -= scope_len * 8;
        for _ in 0..scope_len {
            self.vars.pop();
        }
        push!(self, "; Scope End");
    }

    pub fn gen(&mut self, prog: Prog) {
        self.gen_scope(prog.stmts);
    }

    fn push(&mut self, reg: &str) {
        push!(self, "    push ", #reg);
        self.stack_size += 8;
    }

    fn pop(&mut self, reg: &str) {
        push!(self, "    pop ", #reg);
        self.stack_size -= 8;
    }

    fn decl(&self, var: &Variable) {
        for (declared, _) in self.vars.iter() {
            if declared == &var.name {
                panic!(
                    "Identifier '{}' already declared at line {}",
                    var.name, var.line
                );
            }
        }
    }

    fn expr(&mut self, var: &Variable) -> &mut usize {
        for (declared, loc) in self.vars.iter_mut() {
            if declared == &var.name {
                return loc;
            }
        }
        panic!("Unknown identifier '{}' at line {}", var.name, var.line);
    }
}
