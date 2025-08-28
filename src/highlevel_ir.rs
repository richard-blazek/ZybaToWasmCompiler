#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Real,
    Text,
    Bool,
    Array { item: Box<Type> },
    Func { args: Vec<Type>, ret: Box<Type> },
    Record { fields: Vec<Type> }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cmp {
    Lt, Lte, Eq, Gte, Gt, Neq
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
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
    NewRecord { fields: Vec<Type> },

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

pub struct Subroutine {
    code: Vec<Instruction>
}

pub struct Program {
    funcs: Vec<Subroutine>,
    entry: i64
}
