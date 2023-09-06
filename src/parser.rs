use crate::tokenizer::*;

pub enum Expr {
    ExprInt(String),
    ExprId(String),
}

pub struct StmtDecl {
    pub id: String,
    pub expr: Expr,
}

pub struct StmtRet {
    pub expr: Expr,
}

pub enum Stmt {
    StmtRet(StmtRet),
    StmtDecl(StmtDecl),
}

pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: i32,
    pub parse_tree: Prog,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens,
            pos: 0,
            parse_tree: Prog { stmts: Vec::new() },
        };
    }

    fn parse_expr(&mut self) -> Expr {
        use TokenType::*;
        use Expr::*;
        let tk = self.peek().expect("Not a valid expression");
        match tk.t_type {
            Int => {
                let int = tk.val.clone().expect("Int with no value");
                self.next();
                return ExprInt(int);
            }
            Id => {
                let id = tk.val.clone().expect("Id with no value");
                self.next();
                return ExprId(id);
            }
            _ => panic!("Not a valid expression"),
        }
    }

    fn parse_ret(&mut self) -> StmtRet {
        use TokenType::*;
        self.next();
        let expr = self.parse_expr();
        match self.peek().expect("Not a valid return").t_type {
            Semi => {
                self.next();
                return StmtRet { expr };
            }
            _ => panic!("Not a valid return"),
        }
    }

    pub fn parse_decl(&mut self) -> StmtDecl {
        use TokenType::*;
        self.next();

        let tk = self.peek().expect("Not a valid declaration");
        let id = match tk.t_type {
            Id => tk.val.clone().expect("Id with no value"),
            _ => panic!("Not a valid declaration"),
        };
        self.next();

        let expr = match self.peek().expect("Not a valid declaration").t_type {
            Eq => {
                self.next();
                self.parse_expr()
            }
            _ => panic!("Not a valid declaration"),
        };

        match self.peek().expect("Not a valid declaration").t_type {
            Semi => {
                self.next();
                return StmtDecl { id, expr };
            }
            _ => panic!("Not a valid declaration"),
        }
    }

    pub fn parse(&mut self) {
        use Stmt::*;
        use TokenType::*;
        loop {
            match self.peek() {
                Some(tk) => match tk.t_type {
                    Ret => {
                        let ret = self.parse_ret();
                        self.parse_tree.stmts.push(StmtRet(ret));
                    }
                    Decl => {
                        let decl = self.parse_decl();
                        self.parse_tree.stmts.push(StmtDecl(decl));
                    }
                    Semi => {
                        self.next();
                        continue;
                    }
                    _ => panic!("Not a valid statement"),
                },
                None => break,
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        return self.tokens.get(self.pos as usize);
    }

    fn peek_mult(&self, n: i32) -> Option<&Token> {
        return self.tokens.get((self.pos - 1 + n) as usize);
    }

    fn next(&mut self) {
        self.pos += 1;
    }
}
