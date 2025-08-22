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
    Builtin { line: i64, op: String, args: Vec<Value>, tpe: Type },
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

fn check_type(tpe: &scope::Value) -> Fallible<Type> {
    use scope::Value::*;
    match tpe {
        Var { line, name } => {
            if let Some(tpe) = get_scalar_type(name) {
                Ok(tpe)
            } else {
                err(*line, format!("Type {} does not exist", name))
            }
        },
        Call { line, func, args } => {
            if let Var { name, .. } = *func.clone() {
                let args = args.iter().map(check_type).collect::<Fallible<Vec<_>>>()?;
                if let Some(tpe) = get_generic_type(&name, &args) {
                    Ok(tpe)
                } else {
                    err(*line, format!("Invalid generic type {}", name))
                }
            } else {
                err(*line, "Repeated brackets in type declaration".into())
            }
        }
        Record { fields, .. } => {
            let mut new_fields = HashMap::new();
            for (name, tpe) in fields {
                new_fields.insert(name.clone(), check_type(tpe)?);
            }
            Ok(Type::Record { fields: new_fields })
        }
        _ => err(tpe.line(), "Invalid type expression".into())
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
                let args = args.iter().map(|(_, t)| {
                    check_type(t)
                }).collect::<Fallible<Vec<_>>>()?;
                env.insert(name.clone(), Type::Func { args, return_type })
            }
            v => err(v.line(), "Global must be a literal or function".into())?
        };
    }
    Ok(env)
}

fn check_int(line: i64, value: i64) -> Fallible<Value> {
    Ok(Value::Int { line, value, tpe: Type::Int })
}

fn check_real(line: i64, value: f64) -> Fallible<Value> {
    Ok(Value::Real { line, value, tpe: Type::Real })
}

fn check_text(line: i64, value: String) -> Fallible<Value> {
    Ok(Value::Text { line, value, tpe: Type::Text })
}

fn check_bool(line: i64, value: bool) -> Fallible<Value> {
    Ok(Value::Bool { line, value, tpe: Type::Bool })
}

fn check_var(line: i64, name: String, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let var_type = env.get(&name).unwrap().clone();
    Ok(Value::Var { line, name, tpe: var_type })
}

fn check_init(line: i64, name: String, value: scope::Value, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    env.insert(name.clone(), value.tpe());
    let tpe = value.tpe();
    Ok(Value::Init { line, name, value, tpe })
}

fn check_assign(line: i64, name: String, value: scope::Value, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    let tpe = env.get(&name).unwrap().clone();
    if tpe != value.tpe() {
        err(line, format!("Assigning {:?} to {:?}", value.tpe(), tpe))?
    }
    Ok(Value::Assign { line, name, value, tpe })
}

fn check_record(line: i64, fields: HashMap<String, scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let fields = fields.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, env)?))
    }).collect::<Fallible<HashMap<_, _>>>()?;
    let record_tpe = Type::Record {
        fields: fields.iter().map(|(name, value)| {
            (name.clone(), value.tpe())
        }).collect()
    };
    Ok(Value::Record { line, fields, tpe: record_tpe })
}

fn check_access(line: i64, object: scope::Value, field: String, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let object = Box::new(check_value(object, env)?);
    if let Type::Record { fields } = object.tpe() {
        if let Some(field_tpe) = fields.get(&field) {
            Ok(Value::Access { line, object, field, tpe: field_tpe.clone() })
        } else {
            err(line, format!("Record does not have a field {}", field))
        }
    } else {
        err(line, format!("{:?} is not a record", object.tpe()))
    }
}

fn check_if(line: i64, cond: scope::Value, then: Vec<scope::Value>, elsë: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let cond = Box::new(check_value(cond, env)?);
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

fn check_while(line: i64, cond: scope::Value, body: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let cond = Box::new(check_value(cond, env)?);
    let body = body.into_iter().map(|v| {
        check_value(v, env)
    }).collect::<Fallible<Vec<_>>>()?;
    let tpe = Type::Record { fields: HashMap::new() };
    Ok(Value::While { line, cond, body, tpe })
}

fn check_for(line: i64, key: String, value: String, expr: scope::Value, body: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let expr = Box::new(check_value(expr, env)?);
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

fn check_lambda(line: i64, args: Vec<(String, scope::Value)>, return_type: scope::Value, body: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
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

fn check_call(line: i64, func: scope::Value, args: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let func = Box::new(check_value(func, env)?);
    if let Type::Func { args: expected, return_type } = func.tpe() {
        if args.len() != expected.len() {
            err(line, format!("Expected {} arguments but got {}", expected.len(), args.len()))?;
        }
        let args = args.into_iter().zip(expected).map(|(v, e_t)| {
            let v = check_value(v, env)?;
            if v.tpe() != e_t {
                err(line, format!("Expected {:?} but got {:?}", e_t, v.tpe()))?
            }
            Ok(v)
        }).collect::<Fallible<Vec<_>>>()?;
        Ok(Value::Call { line, func, args, tpe: *return_type })
    } else {
        err(line, format!("{:?} is not a function", func.tpe()))?
    }
}

fn check_builtin_call(line: i64, builtin: String, args: Vec<scope::Value>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let type_args = args.iter().map_while(|arg| {
        check_type(arg).ok()
    }).collect::<Vec<_>>();
    let args = args.into_iter().skip(type_args.len()).map(|arg| {
        check_value(arg, env)
    }).collect::<Fallible<Vec<_>>>()?;
    let arg_types = args.iter().map(Value::tpe).collect::<Vec<_>>();

    if let Some(tpe) = builtin_fn(&builtin, &type_args, &arg_types) {
        Ok(Value::Builtin { line, op: builtin, args, tpe })
    } else {
        err(line, format!("There is no built-in function {} accepting {:?} {:?} as arguments", builtin, type_args, arg_types))
    }
}

fn check_bin_op(line: i64, name: String, lhs: scope::Value, rhs: scope::Value, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    todo!()
}

fn check_value(value: scope::Value, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    use scope::Value::*;
    match value {
        Int { line, value } => check_int(line, value),
        Real { line, value } => check_real(line, value),
        Text { line, value } => check_text(line, value),
        Bool { line, value } => check_bool(line, value),
        Var { line, name } => check_var(line, name, env),
        Init { line, name, value } => check_init(line, name, *value, env),
        Assign { line, name, value } => check_assign(line, name, *value, env),
        Record { line, fields } => check_record(line, fields, env),
        Access { line, object, field } => check_access(line, *object, field, env),
        If { line, cond, then, elsë } => check_if(line, *cond, then, elsë, env),
        While { line, cond, body } => check_while(line, *cond, body, env),
        For { line, key, value, expr, body } => check_for(line, key, value, *expr, body, env),
        Lambda { line, args, return_type, body } => check_lambda(line, args, *return_type, body, env),
        Call { line, func, args } => {
            if let Var { name, .. } = &*func && is_builtin_name(name) {
                check_builtin_call(line, name.clone(), args, env)
            } else {
                check_call(line, *func, args, env)
            }
        }
        BinOp { line, name, lhs, rhs } => check_bin_op(line, name, *lhs, *rhs, env)
    }
}

pub fn check(globals: HashMap<String, scope::Value>) -> Fallible<HashMap<String, Value>> {
    let mut env = global_env(&globals)?;
    globals.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, &mut env)?))
    }).collect()
}
