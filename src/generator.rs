use crate::parser::*;
use crate::tokenizer::TokenType::*;

pub struct Generator {
    pub string: String,
    vars: Vec<(String, usize)>,
    stack_size: usize,
}

impl Generator {
    pub fn new() -> Generator {
        let base = "\
global main
extern ExitProcess

section .text
main:\n";
        return Generator {
            string: String::from(base),
            vars: Vec::new(),
            stack_size: 0,
        };
    }

    fn gen_expr(&mut self, expr: Expr) {
        use Expr::*;
        match expr {
            ExprInt(int) => self.string.push_str(&format!("    mov rax, {}\n", int)),
            ExprId(expr_var) => {
                let mut found = false;
                for (var, loc) in self.vars.iter() {
                    if var == &expr_var {
                        self.string.push_str(&format!(
                            "    mov rax, QWORD [rsp+{}]\n",
                            self.stack_size - loc - 8
                        ));
                        found = true;
                        break;
                    }
                }
                if !found {
                    panic!("Unknown identifier: {}", expr_var);
                }
            }
            ExprBinOp(bin_op) => {
                self.gen_expr(*bin_op.lhs);
                self.push("rax");
                self.gen_expr(*bin_op.rhs);
                self.string.push_str("    mov rcx, rax\n");
                self.pop("rax");
                match bin_op.op {
                    Plus => self.string.push_str("    add rax, rcx\n"),
                    Dash => self.string.push_str("    sub rax, rcx\n"),
                    Star => self.string.push_str("    mul rcx\n"),
                    Slash => {
                        self.string.push_str("    mov edx, 0\n");
                        self.string.push_str("    div rcx\n");
                    }
                    _ => panic!("Unknown operator"),
                }
            }
        }
    }

    fn gen_decl(&mut self, stmt_decl: StmtDecl) {
        for (var, _) in self.vars.iter() {
            if var == &stmt_decl.var {
                panic!("Identifier {} already used", stmt_decl.var);
            }
        }
        self.gen_expr(stmt_decl.expr);
        self.vars.push((stmt_decl.var, self.stack_size));
        self.push("rax");
    }

    fn gen_ret(&mut self, stmt_ret: StmtRet) {
        self.gen_expr(stmt_ret.expr);
        self.string.push_str("    mov rcx, rax\n");
        self.string.push_str("    call ExitProcess\n");
    }

    pub fn gen(&mut self, parse_tree: Prog) {
        use Stmt::*;
        for stmt in parse_tree.stmts {
            match stmt {
                StmtRet(stmt_ret) => self.gen_ret(stmt_ret),
                StmtDecl(stmt_decl) => self.gen_decl(stmt_decl),
            }
        }
    }

    fn push(&mut self, reg: &str) {
        self.string.push_str(&format!("    push {}\n", reg));
        self.stack_size += 8;
    }

    fn pop(&mut self, reg: &str) {
        self.string.push_str(&format!("    pop {}\n", reg));
        self.stack_size -= 8;
    }
}
