use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

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
    Add, Sub, Mul, Div, Rem, And, Or, Xor, Not, Eq, Neq, Lt, Gt, Le, Ge,
    Int, Real, Bool, Text, Dict, List, Set, Get, Has, Len, Concat,
    Insert, Erase, Append
}

static CONSTRUCTORS : LazyLock<HashSet<&str>> = LazyLock::new(||
    HashSet::from_iter(["Int", "Real", "Text", "Bool", "List", "Dict"])
);

static TYPES : LazyLock<HashSet<&str>> = LazyLock::new(||
    CONSTRUCTORS.clone().into_iter().chain(["Func"]).collect()
);

static FUNCTIONS : LazyLock<HashSet<&str>> = LazyLock::new(||
    CONSTRUCTORS.clone().into_iter().chain(["print"]).collect()
);

pub fn is_builtin_type(name: &str) -> bool {
    TYPES.contains(name)
}

pub fn is_builtin_function(name: &str) -> bool {
    FUNCTIONS.contains(name)
}
