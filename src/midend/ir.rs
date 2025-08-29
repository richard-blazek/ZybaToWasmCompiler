use crate::builtin;

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

impl Type {
    pub fn from(t: &builtin::Type) -> Type {
        use crate::midend::utils::sorted;

        match t {
            builtin::Type::Int => Type::Int,
            builtin::Type::Real => Type::Real,
            builtin::Type::Text => Type::Text,
            builtin::Type::Bool => Type::Bool,
            builtin::Type::List { item } => Type::Tuple(vec![
                Type::Array(Box::new(Type::from(&**item))),
                Type::Int
            ]),
            builtin::Type::Dict { key, value } => Type::Tuple(vec![
                Type::Array(Box::new(Type::Tuple(vec![
                    Type::from(&**key),
                    Type::from(&**value),
                    Type::Bool
                ]))),
                Type::Int
            ]),
            builtin::Type::Func { args, ret } => Type::Func(
                args.into_iter().map(Type::from).collect(),
                Box::new(Type::from(&**ret))
            ),
            builtin::Type::Record { fields } => {
                let vec: Vec<_> = sorted(fields, |(n, _)| n);

                Type::Tuple(vec.into_iter().map(|(_, t)| {
                    Type::from(t)
                }).collect())
            }
        }
    }
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
    Label { id: usize },

    // jump to the label with the given id
    JumpAlways { id: usize },

    // stack before: [any, bool]
    // stack after:  [any]
    // if bool == false, jump to the label with the given id
    JumpUnless { id: usize },

    // stack before: [any, fields[0], .., fields[N-1]]
    // stack after:  [any, { fields[0], .., fields[N-1] }]
    NewTuple { fields: Vec<Type> },

    // stack before: [any, { fields[0], .., fields[N-1] }]
    // stack after:  [any, fields[i]]
    GetField { fields: Vec<Type>, i: usize },

    // the local with type=tpe, id=id and a value=value
    // stack before: [any]
    // stack after: [any, value]
    GetLocal { id: usize, tpe: Type },

    // stack before: [any, value]
    // stack after: [any]
    // the local with type=tpe, id=id will be set to the given value
    SetLocal { id: usize, tpe: Type },

    // stack before: [any, a1 of type=args[0], .., aN of type=args[N-1]]
    // stack after:  [any, result of type=ret]
    CallFunc { args: Vec<Type>, ret: Type },

    // global function fn with id=id (different from local variable id)
    // stack before: [any, c1 of type=capture[0], .., cN of type=capture[N-1]]
    // stack after:  [any, fn stored including the captured parameters]
    BindFunc { id: usize, args: Vec<Type>, ret: Type, capture: Vec<Type> },

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
    code: Vec<Instr>,
    locals: Vec<Type>
}

impl Func {
    pub fn new(code: Vec<Instr>, locals: Vec<Type>) -> Func {
        Func { code, locals }
    }

    pub fn code(&self) -> &Vec<Instr> {
        &self.code
    }

    pub fn locals(&self) -> &Vec<Type> {
        &self.locals
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    funcs: Vec<Func>,
    entry: usize
}

impl Program {
    pub fn new(funcs: Vec<Func>, entry: usize) -> Program {
        Program { funcs, entry }
    }

    pub fn funcs(&self) -> &Vec<Func> {
        &self.funcs
    }

    pub fn entry(&self) -> usize {
        self.entry
    }
}
