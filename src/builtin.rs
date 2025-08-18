use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq, Eq)]
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

pub fn is_builtin_type(name: &str) -> bool {
    ["Int", "Real", "Text", "Bool", "List", "Dict", "Func"].contains(&name)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Mul, Div, Rem, And, Or, Xor, Add, Sub, Eq, Neq, Lt, Gt, Lte, Gte,
    LogicAnd, LogicOr, Int, Real, Bool, Text, Dict, List, Not, Print,
    Set, Get, Has, Len, Insert, Erase, Append
}

static FUNCTIONS : LazyLock<HashMap<&str, Builtin>> = LazyLock::new(||
    HashMap::from([
        ("Int", Builtin::Int),
        ("Real", Builtin::Real),
        ("Text", Builtin::Text),
        ("Bool", Builtin::Bool),
        ("List", Builtin::List),
        ("Dict", Builtin::Dict),
        ("not", Builtin::Not),
        ("print", Builtin::Print),
        ("set", Builtin::Set),
        ("get", Builtin::Get),
        ("has", Builtin::Has),
        ("len", Builtin::Len),
        ("insert", Builtin::Insert),
        ("erase", Builtin::Erase),
        ("append", Builtin::Append)
    ])
);

pub fn is_builtin_function(name: &str) -> bool {
    FUNCTIONS.contains_key(name)
}

static OPERATORS : LazyLock<Vec<HashMap<&str, Builtin>>> = LazyLock::new(||
    vec![
        HashMap::from([
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
            ("%", Builtin::Rem),
            ("&", Builtin::And),
            ("|", Builtin::Or),
            ("^", Builtin::Xor)
        ]),
        HashMap::from([
            ("+", Builtin::Add),
            ("-", Builtin::Sub)
        ]),
        HashMap::from([
            ("==", Builtin::Eq),
            ("!=", Builtin::Neq),
            ("<", Builtin::Lt),
            ("<=", Builtin::Lte),
            (">", Builtin::Gt),
            (">=", Builtin::Gte),
        ]),
        HashMap::from([
            ("&&", Builtin::LogicAnd),
            ("||", Builtin::LogicOr)
        ])
    ]
);

pub fn is_builtin_operator(name: &str) -> bool {
    OPERATORS.iter().any(|hash_map| hash_map.contains_key(name))
}
