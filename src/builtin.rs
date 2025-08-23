use std::collections::{HashMap, HashSet};
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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => f.write_str("Int"),
            Type::Real => f.write_str("Real"),
            Type::Text => f.write_str("Text"),
            Type::Bool => f.write_str("Bool"),
            Type::List { item } => {
                f.write_str("List[")?;
                item.fmt(f)?;
                f.write_str("]")
            }
            Type::Dict { key, value } => {
                f.write_str("Dict[")?;
                key.fmt(f)?;
                f.write_str(", ")?;
                value.fmt(f)?;
                f.write_str("]")
            }
            Type::Func { args, return_type } => {
                f.write_str("Func[")?;
                for arg in args {
                    arg.fmt(f)?;
                    f.write_str(", ")?;
                }
                return_type.fmt(f)?;
                f.write_str("]")
            }
            Type::Record { fields } => {
                f.write_str("(")?;
                for (i, (name, tpe)) in fields.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(&name)?;
                    f.write_str(": ")?;
                    tpe.fmt(f)?;
                }
                f.write_str(")")
            }
        }
    }
}

pub fn get_scalar_type(name: &str) -> Option<Type> {
    match name {
        "Int" => Some(Type::Int),
        "Real" => Some(Type::Real),
        "Text" => Some(Type::Text),
        "Bool" => Some(Type::Bool),
        _ => None
    }
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

static BUILTIN_NAMES : LazyLock<HashSet<&str>> = LazyLock::new(|| {
    HashSet::from([
        "Int", "Real", "Text", "Bool", "List", "Dict", "Func",
        "int", "real", "text", "bool", "list", "dict",
        "not", "print", "len", "has", "get", "set", "del", "insert"
    ])
});

pub fn is_builtin_name(name: &str) -> bool {
    BUILTIN_NAMES.contains(name)
}

fn void() -> Type {
    Type::Record { fields: HashMap::new() }
}

pub fn apply_builtin_fn(name: &str, type_args: &[Type], arg_types: &[Type]) -> Option<Type> {
    use Type::*;

    match (name, type_args, arg_types) {
        ("int", [], [Int | Bool | Real | Text]) => Some(Int),
        ("real", [], [Int | Real | Text]) => Some(Real),
        ("bool", [], [Int | Bool]) => Some(Bool),
        ("text", [], [Int | Bool | Real | Text]) => Some(Text),
        ("list", [item], _) => {
            if arg_types.iter().all(|t| t == item) {
                Some(List { item: Box::new(item.clone()) })
            } else {
                None
            }
        }
        ("dict", [key, value], _) => {
            let k_v = [key.clone(), value.clone()];
            if arg_types.chunks(2).all(|pair| pair == k_v) {
                Some(Dict {
                    key: Box::new(key.clone()),
                    value: Box::new(value.clone())
                })
            } else {
                None
            }
        }
        ("not", [], [Int]) => Some(Int),
        ("not", [], [Bool]) => Some(Bool),
        ("print", [], [Text]) => Some(void()),
        ("len", [], [List { .. } | Dict { .. }]) => Some(Int),
        ("has", [], [List { .. }, Int]) => Some(Bool),
        ("has", [], [Dict { key, .. }, k]) if **key == *k => Some(Bool),
        ("get", [], [List { item }, Int]) => Some(*item.clone()),
        ("get", [], [Dict { key, value }, k]) if **key == *k => Some(*value.clone()),
        ("set", [], [List { item }, Int, new_item]) => {
            if **item == *new_item {
                Some(void())
            } else {
                None
            }
        }
        ("set", [], [Dict { key, value }, k, v]) => {
            if **key == *k && **value == *v {
                Some(void())
            } else {
                None
            }
        }
        ("del", [], [List { item }, Int, new_item]) => {
            if **item == *new_item {
                Some(void())
            } else {
                None
            }
        }
        ("del", [], [Dict { key, value }, k, v]) => {
            if **key == *k && **value == *v {
                Some(void())
            } else {
                None
            }
        }
        ("insert", [], [List { item }, Int, new_item]) => {
            if **item == *new_item {
                Some(void())
            } else {
                None
            }
        }
        _ => None
    }
}

static OPERATORS : LazyLock<Vec<HashSet<&str>>> = LazyLock::new(|| {
    vec![
        HashSet::from(["*", "/", "%"]),
        HashSet::from(["+", "-"]),
        HashSet::from(["==", "!=", "<", "<=", ">", ">="]),
        HashSet::from(["&", "|", "^"])
    ]
});

pub fn is_builtin_operator(name: &str) -> bool {
    OPERATORS.iter().any(|set| set.contains(name))
}

pub fn apply_builtin_op(name: &str, lhs: Type, rhs: Type) -> Option<Type> {
    use Type::*;

    match (name, lhs, rhs) {
        ("*", Int, Int) => Some(Int),
        ("*", Real, Real) => Some(Real),
        ("/", Int, Int) => Some(Int),
        ("/", Real, Real) => Some(Real),
        ("%", Int, Int) => Some(Int),

        ("+", Int, Int) => Some(Int),
        ("+", Real, Real) => Some(Real),
        ("+", Text, Text) => Some(Text),
        ("-", Int, Int) => Some(Int),
        ("-", Real, Real) => Some(Real),

        ("==" | "!=" | "<" | "<=" | ">" | ">=", Int, Int) => Some(Bool),
        ("==" | "!=" | "<" | "<=" | ">" | ">=", Real, Real) => Some(Bool),
        ("==" | "!=" | "<" | "<=" | ">" | ">=", Text, Text) => Some(Bool),
        ("==" | "!=" | "<" | "<=" | ">" | ">=", Bool, Bool) => Some(Bool),

        ("&", Int, Int) => Some(Int),
        ("&", Bool, Bool) => Some(Bool),
        ("^", Int, Int) => Some(Int),
        ("^", Bool, Bool) => Some(Bool),
        ("|", Int, Int) => Some(Int),
        ("|", Bool, Bool) => Some(Bool),

        ("|", Record { fields: f1 }, Record { fields: f2 }) => Some(Record {
            fields: f1.into_iter().chain(f2).collect(),
        }),

        _ => None
    }
}
