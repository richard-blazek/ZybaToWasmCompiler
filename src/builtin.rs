use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Real,
    Text,
    Bool,
    List { item: Box<Type> },
    Dict { key: Box<Type>, value: Box<Type> },
    Func { args: Vec<Type>, return_type: Box<Type> },
    Record { fields: HashMap<String, Type> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Builtin {
    Add, Sub, Mul, Div, Rem, And, Or, Xor,
    Eq, Neq, Lt, Gt, Le, Ge, Pow, Not,
    Int, Real, Bool, Text, Dict, List,
    Set, Get, Has, Len, Concat,
    Insert, Erase, Append
}

static BUILTIN_GLOBALS: [&str; 7] = [
    "Int", "Real", "Text", "Bool", "List", "Dict", "Func"
];

pub fn is_builtin_global(name: &str) -> bool {
    BUILTIN_GLOBALS.iter().any(|g| *g == name)
}
