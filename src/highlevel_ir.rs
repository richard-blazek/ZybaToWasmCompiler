use std::collections::HashMap;

use crate::builtin;
use crate::typecheck::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Real,
    Text,
    Bool,
    Array(Box<Type>),
    Func(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cmp {
    Lt, Lte, Eq, Gte, Gt, Neq
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    // stack before: [any]
    // stack after:  [any, value]
    PushInt { value: i64 },
    PushReal { value: f64 },
    PushText { value: String },
    PushBool { value: bool },

    // stack before: [any, value of type=tpe]
    // stack after:  [any]
    Drop { tpe: Type },

    // defines a label
    Label { id: i64 },

    // jump to the label with the given id
    JumpAlways { id: i64 },

    // stack before: [any, bool]
    // stack after:  [any]
    // if bool == false, jump to the label with the given id
    JumpUnless { id: i64 },

    // stack before: [any, fields[0], .., fields[N-1]]
    // stack after:  [any, { fields[0], .., fields[N-1] }]
    NewTuple { fields: Vec<Type> },

    // stack before: [any, { fields[0], .., fields[N-1] }]
    // stack after:  [any, fields[i]]
    GetField { fields: Vec<Type>, i: usize },

    // stack before: [any, value of type=tpe]
    // stack after: [any]
    // new local with id=id and the given value created
    InitLocal { id: i64, tpe: Type },

    // the local with type=tpe, id=id dropped
    DropLocal { id: i64, tpe: Type },

    // the local with type=tpe, id=id and a value=value
    // stack before: [any]
    // stack after: [any, value]
    GetLocal { id: i64, tpe: Type },

    // stack before: [any, value]
    // stack after: [any]
    // the local with type=tpe, id=id will be set to the given value
    SetLocal { id: i64, tpe: Type },

    // stack before: [any, a1 of type=args[0], .., aN of type=args[N-1]]
    // stack after:  [any, result of type=ret]
    CallFunc { args: Vec<Type>, ret: Type },

    // global function fn with id=id (different from local variable id)
    // stack before: [any, c1 of type=capture[0], .., cN of type=capture[N-1]]
    // stack after:  [any, fn stored including the captured parameters]
    BindFunc { id: i64, args: Vec<Type>, ret: Type, capture: Vec<Type> },

    // stack before: [any, value1]
    // stack after:  [any, value2]
    BoolToInt,
    RealToInt,
    TextToInt,
    IntToReal,
    TextToReal,
    IntToBool,
    IntToText,
    RealToText,
    BoolToText,
    NotInt,
    NotBool,

    // stack before: [any, value]
    // stack after:  [any]
    // prints value of type=Text
    PrintText,

    // stack before: [any, len]
    // stack after:  [any, array of length=len]
    NewArray { item: Type },

    // stack before: [any, array of length=len]
    // stack after:  [any, len]
    LenArray { item: Type },

    // stack before: [any, array, i]
    // stack after:  [any, array[i]]
    GetArray { item: Type },

    // stack before: [any, array, i, value of type=item]
    // stack after:  [any]
    SetArray { item: Type },

    // stack before: [any, text of length=len]
    // stack after:  [any, len]
    LenText,

    // stack before: [any, text, i]
    // stack after:  [any, text[i] as Int]
    GetText,

    // stack before: [any, text1, text2]
    // stack after:  [any, text1 + text2]
    CatText,

    // stack before: [any, value1, value2]
    // stack after:  [any, value1 op value2]
    MulInt,
    MulReal,
    DivInt,
    DivReal,
    RemInt,
    AddInt,
    AddReal,
    SubInt,
    SubReal,
    CmpInt { op: Cmp },
    CmpReal { op: Cmp },
    CmpBool { op: Cmp },
    CmpText { op: Cmp },
    AndInt,
    AndBool,
    OrInt,
    OrBool,
    XorInt,
    XorBool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    code: Vec<Instr>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    funcs: Vec<Func>,
    entry: i64
}


fn sorted<A, T: IntoIterator<Item=A>, K: Ord, F: Fn(&A) -> &K>(items: T, key: F) -> Vec<A> {
    let mut v = Vec::from_iter(items);
    v.sort_by(|a, b| K::cmp(key(a), key(b)));
    v
}

fn conv_type(t: builtin::Type) -> Type {
    use builtin::Type::*;

    match t {
        Int => Type::Int,
        Real => Type::Real,
        Text => Type::Text,
        Bool => Type::Bool,
        List { item } => Type::Tuple(vec![
            Type::Array(Box::new(conv_type(*item))),
            Type::Int
        ]),
        Dict { key, value } => Type::Tuple(vec![
            Type::Array(Box::new(Type::Tuple(vec![
                conv_type(*key),
                conv_type(*value),
                Type::Bool
            ]))),
            Type::Int
        ]),
        Func { args, ret } => Type::Func(
            args.into_iter().map(conv_type).collect(),
            Box::new(conv_type(*ret))
        ),
        Record { fields } => {
            let vec: Vec<_> = sorted(fields, |(n, _)| n);

            Type::Tuple(vec.into_iter().map(|(_, t)| conv_type(t)).collect())
        }
    }
}

struct Env {
    local_name_to_id: HashMap<String, i64>,
    local_id_to_type: HashMap<i64, Type>,
    local_counter: i64,

    funcs: Vec<Func>,
    global_fetch: HashMap<String, Instr>,
    global_name_to_id: HashMap<String, i64>,
    global_counter: i64,

    label_counter: i64
}

impl Env {
    fn new(values: &HashMap<String, Value>) -> Env {
        use Instr::*;

        let mut instr_map = HashMap::new();
        let mut id_map = HashMap::new();
        let mut counter = 0;

        for (name, value) in values {
            let name = name.clone();
            match value {
                Value::Int { value, .. } => {
                    instr_map.insert(name, PushInt { value: *value });
                }
                Value::Real { value, .. } => {
                    instr_map.insert(name, PushReal { value: *value });
                }
                Value::Text { value, .. } => {
                    instr_map.insert(name, PushText { value: value.clone() });
                }
                Value::Bool { value, .. } => {
                    instr_map.insert(name, PushBool { value: *value });
                }
                Value::Lambda { args, ret, .. } => {
                    instr_map.insert(name.clone(), BindFunc {
                        id: counter,
                        args: args.iter().map(|(_, t)| conv_type(t.clone())).collect(),
                        ret: conv_type(ret.clone()),
                        capture: vec![]
                    });
                    id_map.insert(name, counter);
                    counter += 1;
                }
                _ => unreachable!(),
            }
        }

        let mut funcs = vec![];
        funcs.resize(counter as usize, Func { code: vec![] });

        Env {
            local_name_to_id: HashMap::new(),
            local_id_to_type: HashMap::new(),
            local_counter: 0,
            global_fetch: instr_map,
            funcs,
            global_name_to_id: id_map,
            global_counter: counter,
            label_counter: 0
        }
    }

    fn new_local(&mut self, name: String, tpe: Type) -> i64 {
        self.local_id_to_type.insert(self.local_counter, tpe);
        self.local_name_to_id.insert(name, self.local_counter);
        self.local_counter += 1;
        self.local_counter - 1
    }

    fn local_type_by_id(&self, id: i64) -> Type {
        self.local_id_to_type[&id].clone()
    }

    fn local_id_by_name(&self, name: &str) -> i64 {
        self.local_name_to_id[name]
    }

    fn local_type_by_name(&self, name: &str) -> Type {
        self.local_type_by_id(self.local_id_by_name(name))
    }

    fn is_local(&self, name: &str) -> bool {
        self.local_name_to_id.contains_key(name)
    }

    fn is_global(&self, name: &str) -> bool {
        self.global_fetch.contains_key(name)
    }

    fn fetch_global(&self, name: &str) -> Instr {
        self.global_fetch[name].clone()
    }

    fn global_id_by_name(&self, name: &str) -> i64 {
        self.global_name_to_id[name]
    }

    fn set_global(&mut self, name: &str, func: Func) {
        let i = self.global_id_by_name(name) as usize;
        self.funcs[i] = func;
    }

    fn add_lambda(&mut self, func: Func) -> i64 {
        self.funcs[self.global_counter as usize - 1] = func;
        self.global_counter += 1;
        self.global_counter - 1
    }

    fn new_label(&mut self) -> i64 {
        self.label_counter += 1;
        self.label_counter - 1
    }
}

fn code_value(value: Value, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    use Value::*;

    match value {
        Int { value, .. } => (vec![Instr::PushInt { value }], vec![]),
        Real { value, .. } => (vec![Instr::PushReal { value }], vec![]),
        Text { value, .. } => (vec![Instr::PushText { value }], vec![]),
        Bool { value, .. } => (vec![Instr::PushBool { value }], vec![]),
        Var { name, tpe } => {
            if env.is_global(&name) {
                (vec![env.fetch_global(&name)], vec![])
            } else {
                let tpe = conv_type(tpe);
                let id = env.local_id_by_name(&name);
                (vec![Instr::GetLocal { id, tpe }], vec![])
            }
        }
        Assign { name, value, .. } => {
            let tpe = conv_type(value.tpe());
            let id = env.local_id_by_name(&name);

            let (mut code, locals) = code_value(*value, env);
            code.push(Instr::SetLocal { id, tpe });
            (code, locals)
        }
        Init { name, value, .. } => {
            let tpe = conv_type(value.tpe());
            let id = env.new_local(name.clone(), tpe.clone());

            let (mut code, mut locals) = code_value(*value, env);
            code.push(Instr::InitLocal { id, tpe });
            locals.push(id);
            (code, locals)
        }
        Record { fields, .. } => {
            let fields = sorted(fields, |(n, _)| n);
            let values: Vec<_> = fields.into_iter().map(|(_, v)| v).collect();
            let types = values.iter().map(|v| conv_type(v.tpe())).collect();

            let (mut code, locals) = code_values(values, env);
            code.push(Instr::NewTuple { fields: types });
            (code, locals)
        }
        Call { func, args, tpe } => {
            let arg_types = args.iter().map(|a| conv_type(a.tpe())).collect();
            let ret_type = conv_type(tpe);

            let (mut code, mut locals) = code_values(args, env);
            let (func_code, func_locals) = code_value(*func, env);
            code.extend(func_code);
            locals.extend(func_locals);

            code.push(Instr::CallFunc { args: arg_types, ret: ret_type });
            (code, locals)
        }
        Access { object, field, .. } => {
            let fields = sorted(match object.tpe() {
                builtin::Type::Record { fields } => fields,
                _ => unreachable!(),
            }, |(n, _)| n);

            let i = fields.iter().position(|(n, _)| n == &field).unwrap();
            let types = fields.into_iter().map(|(_, v)| conv_type(v)).collect();

            let (mut code, locals) = code_value(*object, env);
            code.push(Instr::GetField { fields: types, i });
            (code, locals)
        }
        If { cond, then, elsë, .. } => {
            let if_not = env.new_label();
            let end_if = env.new_label();

            let (mut code, locals) = code_value(*cond, env);
            code.push(Instr::JumpUnless { id: if_not });
            code.extend(code_block(then, env));
            code.push(Instr::JumpAlways { id: end_if });
            code.push(Instr::Label { id: if_not });
            code.extend(code_block(elsë, env));
            code.push(Instr::Label { id: end_if });

            (code, locals)
        }
        While { cond, body, .. } => {
            let leave = env.new_label();
            let start = env.new_label();

            let mut code = vec![];
            let (cond_code, locals) = code_value(*cond, env);

            code.push(Instr::Label { id: start });
            code.extend(cond_code);
            code.push(Instr::JumpUnless { id: leave });
            code.extend(code_block(body, env));
            code.push(Instr::JumpAlways { id: start });;
            code.push(Instr::Label { id: leave });

            (code, locals)
        }
        Lambda { args, ret, body, tpe } => {
            todo!()
        }
        For { key, value, expr, body, tpe } => {
            todo!()
        }
        Builtin { op, args, tpe } => {
            todo!()
        }
    }
}

fn code_values(vals: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let mut code = vec![];
    let mut locals = vec![];
    for val in vals {
        let (new_code, new_locals) = code_value(val, env);
        code.extend(new_code);
        locals.extend(new_locals);
    }
    (code, locals)
}

fn code_block(body: Vec<Value>, env: &mut Env) -> Vec<Instr> {
    let (mut code, locals) = code_values(body, env);
    for local in locals {
        code.push(Instr::DropLocal {
            id: local,
            tpe: env.local_type_by_id(local)
        });
    }
    code
}

pub fn codegen(main_name: &str, globals: HashMap<String, Value>) -> Program {
    let mut env = Env::new(&globals);

    for (name, value) in globals {
        match value {
            Value::Lambda { args, body, .. } => {
                let mut code = vec![];
                for (arg_name, arg_tpe) in args {
                    let tpe = conv_type(arg_tpe);
                    let id = env.new_local(arg_name, tpe.clone());
                    code.push(Instr::InitLocal { id, tpe });
                }
                code.extend(code_block(body, &mut env));
                env.set_global(&name, Func { code });
            }
            _ => unreachable!()
        }
    }

    let main_id = env.global_id_by_name(main_name);
    Program { funcs: env.funcs, entry: main_id }
}
