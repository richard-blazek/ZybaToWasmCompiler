use std::collections::HashMap;

use crate::midend::ir::{Func, Instr, Type};
use crate::frontend::Value;

pub struct Globals {
    funcs: Vec<Func>,
    instrs: HashMap<String, Instr>,
    func_ids: HashMap<String, usize>,
    labels: usize
}

impl Globals {
    pub fn new(values: &HashMap<String, Value>) -> Globals {
        let mut instrs = HashMap::new();
        let mut func_ids = HashMap::new();

        for (name, value) in values {
            use Instr::*;

            let name = name.clone();
            match value {
                Value::Int { value, .. } => {
                    instrs.insert(name, PushInt { value: *value });
                }
                Value::Real { value, .. } => {
                    instrs.insert(name, PushReal { value: *value });
                }
                Value::Text { value, .. } => {
                    instrs.insert(name, PushText { value: value.clone() });
                }
                Value::Bool { value, .. } => {
                    instrs.insert(name, PushBool { value: *value });
                }
                Value::Lambda { args, ret, .. } => {
                    let arg_types = args.iter().map(|(_, t)| {
                        Type::from(t)
                    }).collect();

                    let index = func_ids.len();
                    instrs.insert(name.clone(), BindFunc {
                        id: index,
                        args: arg_types,
                        ret: Type::from(ret),
                        captures: vec![]
                    });
                    func_ids.insert(name, index);
                }
                _ => unreachable!()
            }
        }

        let mut funcs = vec![];
        funcs.resize(func_ids.len(), Func {
            code: vec![],
            args: vec![],
            ret: Type::Tuple(vec![]),
            captures: vec![],
            locals: vec![]
        });

        Globals { instrs, funcs, func_ids, labels: 0 }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.instrs.contains_key(name)
    }

    pub fn fetch(&self, name: &str) -> Instr {
        self.instrs[name].clone()
    }

    pub fn func_id(&self, name: &str) -> usize {
        self.func_ids[name]
    }

    pub fn set_func(&mut self, i: usize, func: Func) {
        self.funcs[i] = func;
    }

    pub fn add_lambda(&mut self, func: Func) -> usize {
        self.funcs.push(func);
        self.funcs.len() - 1
    }

    pub fn new_label(&mut self) -> usize {
        self.labels += 1;
        self.labels - 1
    }

    pub fn to_funcs(self) -> Vec<Func> {
        self.funcs
    }
}
