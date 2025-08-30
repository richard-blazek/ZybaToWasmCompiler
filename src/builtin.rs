use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Real,
    Text,
    Bool,
    List { item: Box<Type> },
    Func { args: Vec<Type>, ret: Box<Type> },
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
            Type::Func { args, ret } => {
                f.write_str("Func[")?;
                for arg in args {
                    arg.fmt(f)?;
                    f.write_str(", ")?;
                }
                ret.fmt(f)?;
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
        ("Func", [.., last]) => Some(Type::Func {
            args: Vec::from(&args[0..args.len() - 1]),
            ret: Box::new(last.clone())
        }),
        _ => None
    }
}

static BUILTIN_NAMES : LazyLock<HashSet<&str>> = LazyLock::new(|| {
    HashSet::from([
        "Int", "Real", "Text", "Bool", "List", "Func",
        "int", "real", "text", "bool", "list",
        "not", "print", "len", "has", "get", "set", "del", "insert"
    ])
});

pub fn is_builtin_name(name: &str) -> bool {
    BUILTIN_NAMES.contains(name)
}

pub fn void() -> Type {
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
        ("not", [], [Int]) => Some(Int),
        ("not", [], [Bool]) => Some(Bool),
        ("print", [], [Text]) => Some(void()),
        ("len", [], [Text | List { .. }]) => Some(Int),
        ("get", [], [List { item }, Int]) => Some(*item.clone()),
        ("get", [], [Text, Int]) => Some(Int),
        ("set", [], [List { item }, Int, new_item]) => {
            if **item == *new_item {
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
        ("==" | "!=", Bool, Bool) => Some(Bool),

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
