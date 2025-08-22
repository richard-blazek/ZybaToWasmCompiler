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
    Init { line: i64, name: String, value: Box<Value>, tpe: Type },
    Assign { line: i64, name: String, value: Box<Value>, tpe: Type },
    If { line: i64, cond: Box<Value>, then: Vec<Value>, elsë: Vec<Value>, tpe: Type },
    While { line: i64, cond: Box<Value>, body: Vec<Value>, tpe: Type },
    For { line: i64, key: String, value: String, expr: Box<Value>, body: Vec<Value>, tpe: Type },
}

impl Value {
    pub fn tpe(&self) -> Type {
        use Value::*;
        match self {
            Int { tpe, .. } | Real { tpe, .. } | Text { tpe, .. }
            | Bool { tpe, .. } | Record { tpe, .. } | Var { tpe, .. }
            | Call { tpe, .. } | Builtin { tpe, .. } | Access { tpe, .. }
            | Lambda { tpe, .. } | Init { tpe, .. } | Assign { tpe, .. }
            | If { tpe, .. } | While { tpe, .. } | For { tpe, .. } => tpe.clone()
        }
    }

    pub fn line(&self) -> i64 {
        use Value::*;
        match self {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | Builtin { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Init { line, .. } | Assign { line, .. }
            | If { line, .. } | While { line, .. } | For { line, .. } => *line
        }
    }
}

fn check_type(tpe: &scope::Type) -> Fallible<Type> {
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
            let args = args.iter().map(check_type).collect::<Fallible<Vec<_>>>()?;
            if let Some(tpe) = get_generic_type(template, &args) {
                Ok(tpe)
            } else {
                err(*line, format!("Invalid type {} with {} arguments", template, args.len()))
            }
        }
        Record { fields, .. } => {
            let mut new_fields = HashMap::new();
            for (name, tpe) in fields {
                new_fields.insert(name.clone(), check_type(tpe)?);
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
                let return_type = Box::new(check_type(return_type)?);
                let args = args.iter().map(|(_, t)| check_type(t)).collect::<Fallible<Vec<_>>>()?;
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
        Var { line, name } => {
            let tpe = env.get(&name).unwrap().clone();
            Ok(Value::Var { line, name, tpe })
        }
        Assign { line, name, value } => {
            let value = check_value(*value, env)?;
            let tpe = value.tpe();
            if let Some(var_type) = env.get(&name) {
                if *var_type != tpe {
                    err(line, "Type mismatch - the variable and the assigned value must have the same type".into())
                } else {
                    Ok(Value::Assign { line, name, value: Box::new(value), tpe })
                }
            } else {
                Ok(Value::Init { line, name, value: Box::new(value), tpe })
            }
        }
        Record { line, fields } => {
            let fields = fields.into_iter().map(|(name, value)| {
                Ok((name, check_value(value, env)?))
            }).collect::<Fallible<HashMap<_, _>>>()?;
            let field_types = fields.iter().map(|(name, value)| {
                (name.clone(), value.tpe())
            }).collect::<HashMap<_, _>>();
            let tpe = Type::Record { fields: field_types };
            Ok(Value::Record { line, fields, tpe })
        }
        Access { line, object, field } => {
            let object = check_value(*object, env)?;
            if let Type::Record { fields } = object.tpe() {
                if let Some(tpe) = fields.get(&field).cloned() {
                    Ok(Value::Access { line, object: Box::new(object), field, tpe })
                } else {
                    err(line, format!("Object does not have a field {}", field))
                }
            } else {
                err(line, format!("Cannot access a field {}, value is not a record", field))
            }
        }
        If { line, cond, then, elsë } => {
            let cond = Box::new(check_value(*cond, env)?);
            let then = then.into_iter().map(|v| {
                check_value(v, env)
            }).collect::<Fallible<Vec<_>>>()?;
            let elsë = elsë.into_iter().map(|v| {
                check_value(v, env)
            }).collect::<Fallible<Vec<_>>>()?;
            let then_t = then.last().map(Value::tpe);
            let elsë_t = elsë.last().map(Value::tpe);
            let tpe = if then_t.is_some() && then_t == elsë_t {
                then_t.unwrap()
            } else {
                Type::Record { fields: HashMap::new() }
            };
            Ok(Value::If { line, cond, then, elsë, tpe })
        }
        While { line, cond, body } => {
            let cond = Box::new(check_value(*cond, env)?);
            let body = body.into_iter().map(|v| {
                check_value(v, env)
            }).collect::<Fallible<Vec<_>>>()?;
            let tpe = Type::Record { fields: HashMap::new() };
            Ok(Value::While { line, cond, body, tpe })
        }
        For { line, key, value, expr, body } => {
            let expr = Box::new(check_value(*expr, env)?);
            if let Type::List { item } = expr.tpe() {
                env.insert(key.clone(), Type::Int);
                env.insert(value.clone(), *item);
            } else if let Type::Dict { key: kt, value: vt } = expr.tpe() {
                env.insert(key.clone(), *kt);
                env.insert(value.clone(), *vt);
            } else {
                err(line, "Expected a List or Dict in the for loop".into())?;
            }
            let body = body.into_iter().map(|v| {
                check_value(v, env)
            }).collect::<Fallible<Vec<_>>>()?;
            let tpe = Type::Record { fields: HashMap::new() };
            Ok(Value::For { line, key, value, expr, body, tpe })
        }
        Lambda { line, args, return_type, body } => {
            let args = args.into_iter().map(|(name, tpe)| {
                Ok((name, check_type(&tpe)?))
            }).collect::<Fallible<Vec<(String, Type)>>>()?;
            env.extend(args.clone());

            let return_type = check_type(&return_type)?;
            let tpe = Type::Func {
                args: args.iter().map(|(_, t)| t.clone()).collect(),
                return_type: Box::new(return_type.clone())
            };
            let body = body.into_iter().map(|v| {
                check_value(v, env)
            }).collect::<Fallible<Vec<_>>>()?;
            let void = Type::Record { fields: HashMap::new() };
            let body_t = body.last().map(Value::tpe).unwrap_or(void.clone());

            if return_type == body_t || return_type == void {
                Ok(Value::Lambda { line, args, return_type, body, tpe })
            } else {
                err(line, format!("Return type is {:?}, but the function actually returns {:?}", return_type, body_t))
            }
        }
        Call { line, func, args } => {
            if let Var { name, .. } = &*func && is_builtin_function(name) {

            } else {
                let func = check_value(*func, env)?;
                if let Type::Func { args, return_type } = func.tpe() {
                    
                } else {
                    err(line, format!("{:?} is not a function", func.tpe()))?
                }
            }
            todo!()
        }
        BinOp { line, name, lhs, rhs } => todo!(),
    }
}

pub fn check(globals: HashMap<String, scope::Value>) -> Fallible<HashMap<String, Value>> {
    let mut env = global_env(&globals)?;
    globals.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, &mut env)?))
    }).collect()
}
