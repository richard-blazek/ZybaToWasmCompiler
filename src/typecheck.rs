use std::collections::HashMap;

use crate::builtin::*;
use crate::error::{err, Fallible};
use crate::scope;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { line: i64, value: i64, tpe: Type },
    Real { line: i64, value: f64, tpe: Type },
    Text { line: i64, value: String, tpe: Type },
    Bool { line: i64, value: bool, tpe: Type },
    Record { line: i64, fields: HashMap<String, Value>, tpe: Type },
    Var { line: i64, name: String, tpe: Type },
    Call { line: i64, func: Box<Value>, args: Vec<Value>, tpe: Type },
    Builtin { line: i64, op: Builtin, args: Vec<Value>, tpe: Type },
    Access { line: i64, object: Box<Value>, field: String, tpe: Type },
    Lambda { line: i64, args: Vec<(String, Type)>, return_type: Type, body: Vec<Value>, tpe: Type },
    Init { line: i64, name: String, value: Box<Value> },
    Assign { line: i64, name: String, value: Box<Value> },
    If { line: i64, cond: Box<Value>, then: Vec<Value>, otherwise: Vec<Value> },
    While { line: i64, cond: Box<Value>, body: Vec<Value> },
    For { line: i64, key: String, value: String, expr: Box<Value>, body: Vec<Value> },
}

fn parse_type(tpe: &scope::Type) -> Fallible<Type> {
    use scope::Type::*;
    match tpe {
        Scalar { line, name } => {
            if let Some(tpe) = get_scalar_type(name) {
                Ok(tpe)
            } else {
                err(*line, format!("Type {} is not valid", name))
            }
        },
        Generic { line, template, args } => {
            let args = args.iter().map(parse_type).collect::<Fallible<Vec<_>>>()?;
            if let Some(tpe) = get_generic_type(template, &args) {
                Ok(tpe)
            } else {
                err(*line, format!("Invalid type {} with {} arguments", template, args.len()))
            }
        }
        Record { fields, .. } => {
            let mut new_fields = HashMap::new();
            for (name, tpe) in fields {
                new_fields.insert(name.clone(), parse_type(tpe)?);
            }
            Ok(Type::Record { fields: new_fields })
        }
    }
}

fn global_env(globals: &HashMap<String, scope::Value>) -> Fallible<HashMap<String, Type>> {
    use scope::Value::*;

    let mut env = HashMap::new();
    for (name, value) in globals {
        match value {
            Int { .. } => env.insert(name.clone(), Type::Int),
            Real { .. } => env.insert(name.clone(), Type::Real),
            Text { .. } => env.insert(name.clone(), Type::Text),
            Bool { .. } => env.insert(name.clone(), Type::Bool),
            Lambda { args, return_type, .. } => {
                let return_type = Box::new(parse_type(return_type)?);
                let args = args.iter().map(|(_, t)| parse_type(t)).collect::<Fallible<Vec<_>>>()?;
                env.insert(name.clone(), Type::Func { args, return_type })
            }
            _ => err(value.line(), "Globals can only be constant literals or functions".into())?
        };
    }
    Ok(env)
}

fn check_value(value: scope::Value, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    use scope::Value::*;
    match value {
        Int { line, value } => Ok(Value::Int { line, value, tpe: Type::Int }),
        Real { line, value } => Ok(Value::Real { line, value, tpe: Type::Real }),
        Text { line, value } => Ok(Value::Text { line, value, tpe: Type::Text }),
        Bool { line, value } => Ok(Value::Bool { line, value, tpe: Type::Bool }),
        Record { line, fields } => todo!(),
        Var { line, name } => todo!(),
        Call { line, func, args } => todo!(),
        BinOp { line, name, lhs, rhs } => todo!(),
        Access { line, object, field } => todo!(),
        Lambda { line, args, return_type, body } => todo!(),
        Assign { line, name, value } => todo!(),
        If { line, cond, then, otherwise } => todo!(),
        While { line, cond, body } => todo!(),
        For { line, key, value, expr, body } => todo!(),
    }
}

pub fn check(globals: HashMap<String, scope::Value>) -> Fallible<HashMap<String, Value>> {
    let mut env = global_env(&globals)?;
    globals.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, &mut env)?))
    }).collect()
}
