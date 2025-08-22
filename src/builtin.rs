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

static SCALAR_TYPES : LazyLock<HashMap<&str, Type>> = LazyLock::new(|| {
    use Type::*;
    HashMap::from([
        ("Int", Int), ("Real", Real), ("Text", Text), ("Bool", Bool)
    ])
});

pub fn get_scalar_type(name: &str) -> Option<Type> {
    SCALAR_TYPES.get(name).cloned()
}

pub fn get_generic_type(name: &str, args: &[Type]) -> Option<Type> {
    match (name, args) {
        ("List", [item]) => Some(Type::List {
            item: Box::new(item.clone())
        }),
        ("Dict", [key, value]) => Some(Type::Dict {
            key: Box::new(key.clone()),
            value: Box::new(value.clone())
        }),
        ("Func", [.., last]) => Some(Type::Func {
            args: Vec::from(&args[0..args.len() - 1]),
            return_type: Box::new(last.clone())
        }),
        _ => None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Mul, Div, Rem, And, Or, Xor, Add, Sub, Eq, Neq, Lt, Gt, Lte, Gte,
    Int, Real, Bool, Text, Dict, List, Not, Print,
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

pub fn is_builtin_name(name: &str) -> bool {
    ["Int", "Real", "Text", "Bool", "List", "Dict", "Func"].contains(&name)
    || FUNCTIONS.contains_key(name)
}

static OPERATORS : LazyLock<Vec<HashMap<&str, Builtin>>> = LazyLock::new(|| {
    use Builtin::*;
    vec![
        HashMap::from([("*", Mul), ("/", Div), ("%", Rem)]),
        HashMap::from([("+", Add), ("-", Sub)]),
        HashMap::from([
            ("==", Eq), ("!=", Neq), ("<", Lt),
            ("<=", Lte), (">", Gt), (">=", Gte),
        ]),
        HashMap::from([("&", And), ("|", Or),  ("^", Xor)])
    ]
});

pub fn is_builtin_operator(name: &str) -> bool {
    OPERATORS.iter().any(|hash_map| hash_map.contains_key(name))
}
