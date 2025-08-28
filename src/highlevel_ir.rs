use std::collections::HashMap;

use crate::{builtin, typecheck::Value};

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
    NewLocal { id: i64, tpe: Type },

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

pub struct Func {
    code: Vec<Instr>
}

pub struct Program {
    funcs: Vec<Func>,
    entry: i64
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
            let mut vec: Vec<_> = fields.into_iter().collect();
            vec.sort_by(|(n1, _), (n2, _)| String::cmp(n1, n2));

            Type::Tuple(vec.into_iter().map(|(_, t)| conv_type(t)).collect())
        }
    }
}

struct Env {
    local_map: HashMap<String, i64>,
    local_types: HashMap<i64, Type>,
    local_count: i64,

    global_fetch: HashMap<String, Instr>,
    global_fn_count: i64,
    lambda_fn_count: i64,
    func_map: HashMap<i64, Func>,
}

impl Env {
    fn new(values: &HashMap<String, Value>) -> Env {
        use Instr::*;

        let mut map = HashMap::new();
        let mut fn_count = 0;

        for (name, value) in values {
            let name = name.clone();
            match value {
                Value::Int { value, .. } => {
                    map.insert(name, PushInt { value: *value });
                }
                Value::Real { value, .. } => {
                    map.insert(name, PushReal { value: *value });
                }
                Value::Text { value, .. } => {
                    map.insert(name, PushText { value: value.clone() });
                }
                Value::Bool { value, .. } => {
                    map.insert(name, PushBool { value: *value });
                }
                Value::Lambda { args, ret, .. } => {
                    map.insert(name, BindFunc {
                        id: fn_count,
                        args: args.iter().map(|(_, t)| conv_type(t.clone())).collect(),
                        ret: conv_type(ret.clone()),
                        capture: vec![]
                    });
                    fn_count += 1;
                }
                _ => unreachable!(),
            }
        }
        Env {
            local_map: HashMap::new(),
            local_types: HashMap::new(),
            local_count: 0,
            global_fetch: map,
            func_map: HashMap::new(),
            global_fn_count: fn_count,
            lambda_fn_count: 0
        }
    }

    fn new_local(&mut self, name: String, tpe: Type) -> i64 {
        self.local_types.insert(self.local_count, tpe);
        self.local_map.insert(name, self.local_count);
        self.local_count += 1;
        self.local_count - 1
    }

    fn local_type_by_id(&self, id: i64) -> Type {
        self.local_types[&id].clone()
    }

    fn local_id_by_name(&self, name: &str) -> i64 {
        self.local_map[name]
    }

    fn local_type_by_name(&self, name: &str) -> Type {
        self.local_type_by_id(self.local_id_by_name(name))
    }

    fn fetch_global(&self, name: &str) -> Instr {
        self.global_fetch[name].clone()
    }

    fn set_global(&mut self, i: i64, func: Func) {
        self.func_map.insert(i, func);
    }

    fn add_lambda(&mut self, func: Func) -> i64 {
        let i = self.lambda_fn_count + self.global_fn_count;
        self.lambda_fn_count += 1;
        self.func_map.insert(i, func);
        i
    }
}

pub fn codegen(globals: HashMap<String, Value>) -> Program {
    let mut env = Env::new(&globals);
    todo!()
}
