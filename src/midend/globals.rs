use std::collections::HashMap;

use crate::midend::ir::{Func, Instr, Type};
use crate::typecheck::Value;

pub struct Globals {
    funcs: Vec<Func>,
    instrs: HashMap<String, Instr>,
    func_ids: HashMap<String, usize>,
    counter: usize,
    labels: usize
}

impl Globals {
    pub fn new(values: &HashMap<String, Value>) -> Globals {
        let mut instrs = HashMap::new();
        let mut func_ids = HashMap::new();
        let mut counter = 0;

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

                    instrs.insert(name.clone(), BindFunc {
                        id: counter,
                        args: arg_types,
                        ret: Type::from(ret),
                        capture: vec![]
                    });
                    func_ids.insert(name, counter);
                    counter += 1;
                }
                _ => unreachable!()
            }
        }

        let mut funcs = vec![];
        funcs.resize(counter, Func::new(vec![], vec![]));

        Globals { instrs, funcs, func_ids, counter, labels: 0 }
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
        self.funcs[self.counter - 1] = func;
        self.counter += 1;
        self.counter - 1
    }

    pub fn new_label(&mut self) -> usize {
        self.labels += 1;
        self.labels - 1
    }

    pub fn to_funcs(self) -> Vec<Func> {
        self.funcs
    }
}
