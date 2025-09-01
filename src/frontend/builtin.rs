use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Real,
    Text,
    Bool,
    Array { item: Box<Type> },
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
            Type::Array { item } => {
                f.write_str("Array[")?;
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
        ("Array", [item]) => Some(Type::Array {
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
        "Int", "Real", "Text", "Bool", "Array", "Func",
        "int", "real", "text", "bool", "array",
        "not", "print", "len", "has", "get", "set", "chr"
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
        ("int", [], [Int | Bool | Real]) => Some(Int),
        ("real", [], [Int | Real]) => Some(Real),
        ("bool", [], [Int | Bool]) => Some(Bool),
        ("text", [], [Text]) => Some(Text),
        ("array", [item], [Int]) => {
            Some(Array { item: Box::new(item.clone()) })
        }
        ("not", [], [Int]) => Some(Int),
        ("not", [], [Bool]) => Some(Bool),
        ("print", [], ts) => {
            if ts.iter().all(|t| [Int, Bool, Text, Real].contains(t)) {
                Some(void())
            } else {
                None
            }
        }
        ("len", [], [Text | Array { .. }]) => Some(Int),
        ("get", [], [Array { item }, Int]) => Some(*item.clone()),
        ("get", [], [Text, Int]) => Some(Int),
        ("set", [], [Array { item }, Int, new_item]) => {
            if **item == *new_item {
                Some(void())
            } else {
                None
            }
        }
        ("chr", [], [Int]) => Some(Text),
        _ => None
    }
}

static OPERATORS : LazyLock<Vec<HashSet<&str>>> = LazyLock::new(|| {
    vec![
        HashSet::from(["*", "/", "%", "&", "|", "^"]),
        HashSet::from(["+", "-"]),
        HashSet::from(["==", "!=", "<", "<=", ">", ">="]),
        HashSet::from(["||", "&&"]),
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

        ("&" | "^" | "|", Int, Int) => Some(Int),
        ("&" | "^" | "|", Bool, Bool) => Some(Bool),
        ("&&" | "||", Bool, Bool) => Some(Bool),

        // ("|", Record { fields: f1 }, Record { fields: f2 }) => Some(Record {
        //     fields: f1.into_iter().chain(f2).collect(),
        // }),

        _ => None
    }
}
