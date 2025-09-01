use crate::frontend;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub fn from(t: &frontend::Type) -> Type {
        use crate::midend::utils::sorted;

        match t {
            frontend::Type::Int => Type::Int,
            frontend::Type::Real => Type::Real,
            frontend::Type::Text => Type::Text,
            frontend::Type::Bool => Type::Bool,
            frontend::Type::Array { item } => Type::Array(
                Box::new(Type::from(&**item))
            ),
            frontend::Type::Func { args, ret } => Type::Func(
                args.into_iter().map(Type::from).collect(),
                Box::new(Type::from(&**ret))
            ),
            frontend::Type::Record { fields } => {
                let vec: Vec<_> = sorted(fields, |(n, _)| n);

                Type::Tuple(vec.into_iter().map(|(_, t)| {
                    Type::from(t)
                }).collect())
            }
        }
    }

    pub fn array_item(&self) -> Type {
        match self {
            Type::Array(item) => *item.clone(),
            _ => unreachable!()
        }
    }
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

    // defines a label above/below a block of instruction
    Loop { id: usize, inner: Vec<Instr> },
    Ifte { then: Vec<Instr>, elsë: Vec<Instr>, ret: Type },

    // jump to the label with the given id
    RepeatLoop { id: usize },

    // stack before: [any, bool]
    // stack after:  [any]
    // if bool == false, jump to the bottom label with the given id
    QuitUnless { id: usize },

    // stack before: [any, fields[0], .., fields[N-1]]
    // stack after:  [any, { fields[0], .., fields[N-1] }]
    NewTuple { fields: Vec<Type> },

    // stack before: [any, { fields[0], .., fields[N-1] }]
    // stack after:  [any, fields[i]]
    GetField { fields: Vec<Type>, i: usize },

    // stack before: [any, { fields[0], .., fields[N-1] }, value]
    // stack after:  [any]
    // fields[i] is set to value
    SetField { fields: Vec<Type>, i: usize },

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
    BindFunc { id: usize, args: Vec<Type>, ret: Type, captures: Vec<(usize, Type)> },

    // stack before: [any, value1]
    // stack after:  [any, value2]
    RealToInt,
    IntToReal,
    IntToTextAscii,
    NotInt,
    NotBool,

    // stack before: [any, value]
    // stack after:  [any]
    // prints value
    PrintText,
    PrintReal,
    PrintInt,

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
    LtInt,
    EqInt,
    LtReal,
    EqReal,
    LtText,
    EqText,
    AndInt,
    AndBool,
    OrInt,
    OrBool,
    XorInt,
    XorBool,

    Abort
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub code: Vec<Instr>,
    pub args: Vec<Type>,
    pub ret: Type,
    pub captures: Vec<(usize, Type)>,
    pub locals: Vec<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub entry: usize
}
