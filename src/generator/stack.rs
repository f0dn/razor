use std::{collections::HashMap, usize};

enum Type {
    Var(String),
    Const(String, usize),
    Func,
    Scope,
    None,
}

struct Var {
    t: Type,
    size: usize,
}

pub struct Stack {
    stack: Vec<Var>,
    current_offset: usize,
    asm_sizes: HashMap<usize, &'static str>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            stack: Vec::new(),
            current_offset: 0,
            asm_sizes: HashMap::from([(1, "byte"), (4, "dword"), (8, "qword")]),
        }
    }

    fn get_asm_size(&self, size: usize) -> &'static str {
        self.asm_sizes.get(&size).expect("Invalid size")
    }

    pub fn get(&self, name: &str) -> Option<(String, usize)> {
        let mut offset = 0;
        for var in self.stack.iter().rev() {
            match var.t {
                Type::Var(ref n) => {
                    if n == name {
                        return Some((
                            format!("{} [rbp+{}]", self.get_asm_size(var.size), offset),
                            var.size,
                        ));
                    }
                }
                Type::Func => break,
                _ => {}
            }
            offset += var.size;
        }
        None
    }

    pub fn get_const(&self, name: &str) -> Option<usize> {
        for var in self.stack.iter().rev() {
            if let Type::Const(ref n, val) = var.t {
                if n == name {
                    return Some(val);
                }
            }
        }
        None
    }

    pub fn get_func(&self) -> Option<usize> {
        let mut offset = 0;
        for var in self.stack.iter().rev() {
            if let Type::Func = &var.t {
                return Some(offset);
            }
            offset += var.size;
        }
        None
    }

    pub fn push(&mut self, name: String, size: usize) -> String {
        if size > 8 - self.current_offset {}
        self.stack.push(Var {
            t: Type::Var(name),
            size,
        });
    }

    pub fn push_const(&mut self, name: String, value: usize) {
        self.stack.push(Var {
            t: Type::Const(name, value),
            size: 0,
        });
    }

    pub fn push_func(&mut self) {
        self.stack.push(Var {
            t: Type::Func,
            size: 8,
        });
    }

    /*
    pub fn push_none(&mut self) {
        self.stack.push(Var {
            t: Type::None,
            size: 8,
        });
    }
    */

    pub fn push_scope(&mut self) {
        self.stack.push(Var {
            t: Type::Scope,
            size: 0,
        });
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn pop_until_scope(&mut self) -> usize {
        let mut offset = 0;
        while let Some(var) = self.stack.pop() {
            if let Type::Scope = var.t {
                self.stack.push(var);
                break;
            }
            offset += var.size;
        }
        offset
    }
}
