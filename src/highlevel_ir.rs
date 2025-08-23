use crate::builtin::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cmp {
    Lt, Lte, Eq, Gte, Gt, Neq
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    PushInt { value: i64 },
    PushReal { value: f64 },
    PushText { value: String },
    PushBool { value: bool },
    MakeRecord { fields: Vec<Type> },
    GetGlobal { name: String, tpe: Type },
    GetLocal { var_id: i64, tpe: Type },
    SetLocal { var_id: i64, tpe: Type },
    GetField { fields: Vec<Type>, i: usize },
    CallFn,
    BindFn { capture: Vec<Type> },

    MakeInt { from: Type },
    MakeReal { from: Type },
    MakeText { from: Type },
    MakeBool { from: Type },
    MakeList { item: Type, count: usize },
    MakeDict { key: Type, value: Type, count: usize },
    NotInt,
    NotBool,
    Print,
    LenList { item: Type },
    LenDict { key: Type, value: Type },
    HasList { item: Type },
    HasDict { key: Type, value: Type },
    GetList { item: Type },
    GetDict { key: Type, value: Type },
    SetList { item: Type },
    SetDict { key: Type, value: Type },
    DelList { item: Type },
    DelDict { key: Type, value: Type },
    InsertList { item: Type },

    MulInt,
    MulReal,
    DivInt,
    DivReal,
    RemInt,
    AddInt,
    AddReal,
    ConcatText,
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

    Label { label_id: i64 },
    Goto { label_id: i64 },
    GotoIfFalse { label_id: i64 },
    Iter { end_label_id: i64 },

    Drop { tpe: Type },
}
