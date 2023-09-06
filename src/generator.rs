use crate::parser::*;

pub struct Generator {
    pub string: String,
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
        };
    }

    fn gen_expr(&mut self, expr: Expr) {
        self.string.push_str("    mov rdi, ");
        self.string.push_str(expr.int.as_str());
        self.string.push('\n');
    }

    fn gen_ret(&mut self, stmt_ret: StmtRet) {
        self.gen_expr(stmt_ret.expr);
        self.string.push_str("    push rdi\n");
        self.string.push_str("    call ExitProcess\n");
    }

    pub fn gen(&mut self, parse_tree: Prog) {
        use Stmt::*;
        for stmt in parse_tree.stmts {
            match stmt {
                StmtRet(stmt_ret) => self.gen_ret(stmt_ret),
            }
        }
    }
}
