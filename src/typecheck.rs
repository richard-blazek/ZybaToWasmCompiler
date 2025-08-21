use std::collections::HashMap;
use crate::builtin;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { line: i64, value: i64, tpe: builtin::Type },
    Real { line: i64, value: f64, tpe: builtin::Type },
    Text { line: i64, value: String, tpe: builtin::Type },
    Bool { line: i64, value: bool, tpe: builtin::Type },
    Record { line: i64, fields: HashMap<String, Value>, tpe: builtin::Type },
    Var { line: i64, name: String, tpe: builtin::Type },
    Call { line: i64, func: Box<Value>, args: Vec<Value>, tpe: builtin::Type },
    Builtin { line: i64, op: builtin::Builtin, args: Vec<Value>, tpe: builtin::Type },
    Access { line: i64, object: Box<Value>, field: String, tpe: builtin::Type },
    Lambda { line: i64, args: Vec<(String, builtin::Type)>, return_type: builtin::Type, body: Vec<Statement>, tpe: builtin::Type }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Value { line: i64, value: Value },
    Init { line: i64, name: String, value: Value },
    Assign { line: i64, name: String, value: Value },
    If { line: i64, cond: Value, then: Vec<Statement>, otherwise: Vec<Statement> },
    While { line: i64, cond: Value, body: Vec<Statement> },
    For { line: i64, key: String, value: String, expr: Value, body: Vec<Statement> },
    Return { line: i64, value: Value }
}

