use std::collections::HashMap;

use crate::builtin::*;
use crate::error::{err, Fallible};
use crate::nameres::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { value: i64, tpe: Type },
    Real { value: f64, tpe: Type },
    Text { value: String, tpe: Type },
    Bool { value: bool, tpe: Type },
    Record { fields: HashMap<String, Value>, tpe: Type },
    Var { name: String, tpe: Type },
    Call { func: Box<Value>, args: Vec<Value>, tpe: Type },
    Builtin { op: String, args: Vec<Value>, tpe: Type },
    Access { object: Box<Value>, field: String, tpe: Type },
    Lambda { args: Vec<(String, Type)>, ret: Type, body: Vec<Value>, tpe: Type },
    Init { name: String, value: Box<Value>, tpe: Type },
    Assign { name: String, value: Box<Value>, tpe: Type },
    If { cond: Box<Value>, then: Vec<Value>, elsë: Vec<Value>, tpe: Type },
    While { cond: Box<Value>, body: Vec<Value>, tpe: Type },
    For { key: String, value: String, expr: Box<Value>, body: Vec<Value>, tpe: Type },
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
}

fn check_type(tpe: &Expr) -> Fallible<Type> {
    match tpe {
        Expr::Var { line, name } => {
            if let Some(tpe) = get_scalar_type(name) {
                Ok(tpe)
            } else {
                err(*line, format!("Type {} does not exist", name))
            }
        },
        Expr::Call { line, func, args } => {
            if let Expr::Var { name, .. } = *func.clone() {
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
        Expr::Record { fields, .. } => {
            let mut new_fields = HashMap::new();
            for (name, tpe) in fields {
                new_fields.insert(name.clone(), check_type(tpe)?);
            }
            Ok(Type::Record { fields: new_fields })
        }
        _ => err(tpe.line(), "Invalid type expression".into())
    }
}

fn global_env(globals: &HashMap<String, Expr>) -> Fallible<HashMap<String, Type>> {
    let mut env = HashMap::new();
    for (name, value) in globals {
        match value {
            Expr::Int { .. } => env.insert(name.clone(), Type::Int),
            Expr::Real { .. } => env.insert(name.clone(), Type::Real),
            Expr::Text { .. } => env.insert(name.clone(), Type::Text),
            Expr::Bool { .. } => env.insert(name.clone(), Type::Bool),
            Expr::Lambda { args, ret, .. } => {
                let ret = Box::new(check_type(ret)?);
                let args = args.iter().map(|(_, t)| {
                    check_type(t)
                }).collect::<Fallible<Vec<_>>>()?;
                env.insert(name.clone(), Type::Func { args, ret })
            }
            v => err(v.line(), "Global must be a literal or function".into())?
        };
    }
    Ok(env)
}

fn check_int(value: i64) -> Fallible<Value> {
    Ok(Value::Int { value, tpe: Type::Int })
}

fn check_real(value: f64) -> Fallible<Value> {
    Ok(Value::Real { value, tpe: Type::Real })
}

fn check_text(value: String) -> Fallible<Value> {
    Ok(Value::Text { value, tpe: Type::Text })
}

fn check_bool(value: bool) -> Fallible<Value> {
    Ok(Value::Bool { value, tpe: Type::Bool })
}

fn check_var(name: String, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let var_type = env.get(&name).unwrap().clone();
    Ok(Value::Var { name, tpe: var_type })
}

fn check_init(name: String, value: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    env.insert(name.clone(), value.tpe());
    let tpe = value.tpe();
    Ok(Value::Init { name, value, tpe })
}

fn check_assign(line: i64, name: String, value: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    let tpe = env.get(&name).unwrap().clone();
    if tpe != value.tpe() {
        err(line, format!("Assigning {} to {}", value.tpe(), tpe))?
    }
    Ok(Value::Assign { name, value, tpe })
}

fn check_record(fields: HashMap<String, Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let fields = fields.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, env)?))
    }).collect::<Fallible<HashMap<_, _>>>()?;
    let record_tpe = Type::Record {
        fields: fields.iter().map(|(name, value)| {
            (name.clone(), value.tpe())
        }).collect()
    };
    Ok(Value::Record { fields, tpe: record_tpe })
}

fn check_access(line: i64, object: Expr, field: String, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let object = Box::new(check_value(object, env)?);
    if let Type::Record { fields } = object.tpe() {
        if let Some(field_tpe) = fields.get(&field) {
            Ok(Value::Access { object, field, tpe: field_tpe.clone() })
        } else {
            err(line, format!("Record does not have a field {}", field))
        }
    } else {
        err(line, format!("{} is not a record", object.tpe()))
    }
}

fn check_if(cond: Expr, then: Vec<Expr>, elsë: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
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
    Ok(Value::If { cond, then, elsë, tpe })
}

fn check_while(cond: Expr, body: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let cond = Box::new(check_value(cond, env)?);
    let body = body.into_iter().map(|v| {
        check_value(v, env)
    }).collect::<Fallible<Vec<_>>>()?;
    let tpe = Type::Record { fields: HashMap::new() };
    Ok(Value::While { cond, body, tpe })
}

fn check_for(line: i64, key: String, value: String, expr: Expr, body: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
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
    Ok(Value::For { key, value, expr, body, tpe })
}

fn check_lambda(line: i64, args: Vec<(String, Expr)>, ret: Expr, body: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let args = args.into_iter().map(|(name, tpe)| {
        Ok((name, check_type(&tpe)?))
    }).collect::<Fallible<Vec<(String, Type)>>>()?;
    env.extend(args.clone());

    let ret = check_type(&ret)?;
    let tpe = Type::Func {
        args: args.iter().map(|(_, t)| t.clone()).collect(),
        ret: Box::new(ret.clone())
    };
    let body = body.into_iter().map(|v| {
        check_value(v, env)
    }).collect::<Fallible<Vec<_>>>()?;
    let void = Type::Record { fields: HashMap::new() };
    let body_t = body.last().map(Value::tpe).unwrap_or(void.clone());

    if ret == body_t || ret == void {
        Ok(Value::Lambda { args, ret, body, tpe })
    } else {
        err(line, format!("Return type is {}, but the function returns {}", ret, body_t))
    }
}

fn check_call(line: i64, func: Expr, args: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let func = Box::new(check_value(func, env)?);
    if let Type::Func { args: expected, ret } = func.tpe() {
        if args.len() != expected.len() {
            err(line, format!("Expected {} arguments but got {}", expected.len(), args.len()))?;
        }
        let args = args.into_iter().zip(expected).map(|(v, e_t)| {
            let v = check_value(v, env)?;
            if v.tpe() != e_t {
                err(line, format!("Expected {} but got {}", e_t, v.tpe()))?
            }
            Ok(v)
        }).collect::<Fallible<Vec<_>>>()?;
        Ok(Value::Call { func, args, tpe: *ret })
    } else {
        err(line, format!("{} is not a function", func.tpe()))?
    }
}

fn check_builtin_call(line: i64, builtin: String, args: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let type_args = args.iter().map_while(|arg| {
        check_type(arg).ok()
    }).collect::<Vec<_>>();
    let args = args.into_iter().skip(type_args.len()).map(|arg| {
        check_value(arg, env)
    }).collect::<Fallible<Vec<_>>>()?;
    let arg_types = args.iter().map(Value::tpe).collect::<Vec<_>>();

    if let Some(tpe) = apply_builtin_fn(&builtin, &type_args, &arg_types) {
        Ok(Value::Builtin { op: builtin, args, tpe })
    } else {
        let arg_str = type_args.iter().chain(arg_types.iter()).map(|t| {
            format!("{}", t)
        }).collect::<Vec<_>>().join(", ");
        err(line, format!("There is no built-in function {}[{}]", builtin, arg_str))
    }
}

fn check_bin_op(line: i64, name: String, lhs: Expr, rhs: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let lhs = check_value(lhs, env)?;
    let rhs = check_value(rhs, env)?;
    if let Some(tpe) = apply_builtin_op(&name, lhs.tpe(), rhs.tpe()) {
        Ok(Value::Builtin { op: name, args: vec![lhs, rhs], tpe })
    } else {
        err(line, format!("Operator {} does not accept {} and {}", name, lhs.tpe(), rhs.tpe()))
    }
}

fn check_value(value: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    match value {
        Expr::Int { value, .. } => check_int(value),
        Expr::Real { value, .. } => check_real(value),
        Expr::Text { value, .. } => check_text(value),
        Expr::Bool { value, .. } => check_bool(value),
        Expr::Var { name, .. } => check_var(name, env),
        Expr::Init { name, value, .. } => {
            check_init(name, *value, env)
        }
        Expr::Assign { line, name, value } => {
            check_assign(line, name, *value, env)
        }
        Expr::Record { fields, .. } => {
            check_record(fields, env)
        }
        Expr::Access { line, object, field } => {
            check_access(line, *object, field, env)
        }
        Expr::If { cond, then, elsë, .. } => {
            check_if(*cond, then, elsë, env)
        }
        Expr::While { cond, body, .. } => {
            check_while(*cond, body, env)
        }
        Expr::For { line, key, value, expr, body } => {
            check_for(line, key, value, *expr, body, env)
        }
        Expr::Lambda { line, args, ret, body } => {
            check_lambda(line, args, *ret, body, env)
        }
        Expr::Call { line, func, args } => {
            if let Expr::Var { name, .. } = &*func && is_builtin_name(name) {
                check_builtin_call(line, name.clone(), args, env)
            } else {
                check_call(line, *func, args, env)
            }
        }
        Expr::BinOp { line, name, lhs, rhs } => {
            check_bin_op(line, name, *lhs, *rhs, env)
        }
    }
}

pub fn check(globals: HashMap<String, Expr>) -> Fallible<HashMap<String, Value>> {
    let mut env = global_env(&globals)?;
    globals.into_iter().map(|(name, value)| {
        Ok((name, check_value(value, &mut env)?))
    }).collect()
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    mod helpers {
        use super::*;

        pub fn int(value: i64) -> Expr { Expr::Int { line: 1, value } }
        pub fn real(value: f64) -> Expr { Expr::Real { line: 1, value } }
        pub fn text(value: &str) -> Expr { Expr::Text { line: 1, value: value.to_string() } }
        pub fn bool(value: bool) -> Expr { Expr::Bool { line: 1, value } }
        pub fn var(name: &str) -> Expr { Expr::Var { line: 1, name: name.to_string() } }
        pub fn call(func: &str, args: Vec<Expr>) -> Expr {
            Expr::Call { line: 1, func: Box::new(var(func)), args }
        }
        pub fn bin_op(name: &str, lhs: Expr, rhs: Expr) -> Expr {
            Expr::BinOp { line: 1, name: name.to_string(), lhs: Box::new(lhs), rhs: Box::new(rhs) }
        }
        pub fn record(fields: Vec<(&str, Expr)>) -> Expr {
            Expr::Record { line: 1, fields: fields.into_iter().map(|(k, v)| (k.to_string(), v)).collect() }
        }
        pub fn access(obj: Expr, field: &str) -> Expr {
            Expr::Access { line: 1, object: Box::new(obj), field: field.to_string() }
        }
        pub fn init(name: &str, value: Expr) -> Expr {
            Expr::Init { line: 1, name: name.to_string(), value: Box::new(value) }
        }
        pub fn assign(name: &str, value: Expr) -> Expr {
            Expr::Assign { line: 1, name: name.to_string(), value: Box::new(value) }
        }
        pub fn lambda(args: Vec<(&str, Expr)>, ret_type: Expr, body: Vec<Expr>) -> Expr {
            Expr::Lambda {
                line: 1,
                args: args.into_iter().map(|(n, t)| (n.to_string(), t)).collect(),
                ret: Box::new(ret_type),
                body,
            }
        }
        pub fn if_expr(cond: Expr, then: Vec<Expr>, elsë: Vec<Expr>) -> Expr {
            Expr::If { line: 1, cond: Box::new(cond), then, elsë }
        }
        pub fn while_loop(cond: Expr, body: Vec<Expr>) -> Expr {
            Expr::While { line: 1, cond: Box::new(cond), body }
        }
        pub fn for_loop(key: &str, value: &str, expr: Expr, body: Vec<Expr>) -> Expr {
            Expr::For { line: 1, key: key.to_string(), value: value.to_string(), expr: Box::new(expr), body }
        }

        pub fn wrap_in_lambda(value: Expr) -> Expr {
            lambda(vec![], record(vec![]), vec![value])
        }
    }

    use helpers::*;

    pub fn unwrap_lambda(value: &Value) -> Value {
        match value {
            Value::Lambda { body, .. } => body[0].clone(),
            _ => panic!("it should be lambda")
        }
    }

    #[test]
    fn test_literals_and_records() {
        let globals = HashMap::from([
            ("my_int".to_string(), int(10)),
            ("my_real".to_string(), real(3.14)),
            ("my_text".to_string(), text("hello")),
            ("my_bool".to_string(), bool(true)),
            ("my_record".to_string(), lambda(
                vec![],
                record(vec![("a", var("Int")), ("b", var("Bool"))]),
                vec![record(vec![("a", int(1)), ("b", bool(false))])]
            )),
        ]);

        let typed_globals = check(globals).unwrap();
        assert_eq!(typed_globals["my_int"].tpe(), Type::Int);
        assert_eq!(typed_globals["my_real"].tpe(), Type::Real);
        assert_eq!(typed_globals["my_text"].tpe(), Type::Text);
        assert_eq!(typed_globals["my_bool"].tpe(), Type::Bool);
        assert_eq!(typed_globals["my_record"].tpe(), Type::Func {
            args: vec![],
            ret: Box::new(Type::Record {
                fields: HashMap::from([
                    ("a".to_string(), Type::Int),
                    ("b".to_string(), Type::Bool),
                ])
            })
        });
    }

    #[test]
    fn test_assignment_and_vars() {
        let globals = HashMap::from([
            ("main".to_string(), lambda(vec![], var("Int"), vec![
                init("x", int(5)),
                assign("x", int(10)),
                var("x"),
            ])),
        ]);
        let typed_globals = check(globals).unwrap();
        if let Value::Lambda { body, .. } = &typed_globals["main"] {
            assert!(matches!(body[0], Value::Init { .. }));
            assert_eq!(body[0].tpe(), Type::Int);
            assert!(matches!(body[1], Value::Assign { .. }));
            assert_eq!(body[1].tpe(), Type::Int);
            assert!(matches!(body[2], Value::Var { .. }));
            assert_eq!(body[2].tpe(), Type::Int);
        } else {
            panic!("Expected lambda");
        }
    }

    #[test]
    fn test_binary_operators() {
        let ops = vec![
            ("*", int(1), int(2), Type::Int),
            ("+", real(1.0), real(2.0), Type::Real),
            ("*", real(1.0), real(2.0), Type::Real),
            ("/", real(1.0), real(2.0), Type::Real),
            ("%", int(1), int(2), Type::Int),
            ("+", text("a"), text("b"), Type::Text),
            ("-", real(1.0), real(2.0), Type::Real),
            ("==", bool(true), bool(false), Type::Bool),
            ("!=", text("a"), text("b"), Type::Bool),
            ("<", int(1), int(2), Type::Bool),
            ("<=", real(1.0), real(2.0), Type::Bool),
            (">", text("a"), text("b"), Type::Bool),
            (">=", int(1), int(2), Type::Bool),
            ("&", bool(true), bool(false), Type::Bool),
            ("|", bool(true), bool(false), Type::Bool),
            ("|", int(1), int(2), Type::Int),
            ("^", bool(true), bool(false), Type::Bool),
            ("|", record(vec![("a", int(1))]), record(vec![("b", int(2))]), Type::Record {
                fields: HashMap::from([("a".to_string(), Type::Int), ("b".to_string(), Type::Int)]),
            }),
        ];

        for (op, lhs, rhs, tpe) in ops {
            let globals = HashMap::from([("op".to_string(), wrap_in_lambda(bin_op(op, lhs, rhs)))]);
            let op = unwrap_lambda(&check(globals).unwrap()["op"]);
            assert_eq!(op.tpe(), tpe);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let calls = vec![
            (call("int", vec![real(1.0)]), Type::Int),
            (call("real", vec![int(1)]), Type::Real),
            (call("text", vec![bool(true)]), Type::Text),
            (call("bool", vec![int(0)]), Type::Bool),
            (call("list", vec![var("Int"), int(1), int(2)]), Type::List { item: Box::new(Type::Int) }),
            (call("dict", vec![var("Text"), var("Int"), text("a"), int(1)]), Type::Dict { key: Box::new(Type::Text), value: Box::new(Type::Int) }),
            (call("not", vec![bool(true)]), Type::Bool),
            (call("print", vec![text("a")]), Type::Record { fields: HashMap::new() }),
            (call("len", vec![call("list", vec![var("Int")])]), Type::Int),
            (call("has", vec![call("dict", vec![var("Text"), var("Int"), text("a"), int(1)]), text("a")]), Type::Bool),
            (call("get", vec![call("list", vec![var("Int"), int(1)]), int(0)]), Type::Int),
            (call("get", vec![call("dict", vec![var("Text"), var("Int"), text("a"), int(1)]), text("a")]), Type::Int),
            (call("set", vec![call("list", vec![var("Int"), int(1)]), int(0), int(2)]), Type::Record { fields: HashMap::new() }),
            (call("del", vec![call("dict", vec![var("Text"), var("Int"), text("a"), int(1)]), text("a"), int(1)]), Type::Record { fields: HashMap::new() }),
            (call("insert", vec![call("list", vec![var("Int"), int(1)]), int(0), int(2)]), Type::Record { fields: HashMap::new() }),
        ];

        for (c, tpe) in calls {
            let globals = HashMap::from([
                ("call".to_string(), wrap_in_lambda(c))
            ]);

            let call = unwrap_lambda(&check(globals).unwrap()["call"]);
            assert_eq!(call.tpe(), tpe);
        }
    }

    #[test]
    fn test_access() {
        let globals = HashMap::from([
            ("a".to_string(), wrap_in_lambda(access(record(vec![("b", int(1))]), "b")))
        ]);

        let a = unwrap_lambda(&check(globals).unwrap()["a"]);
        assert_eq!(a.tpe(), Type::Int);
    }

    #[test]
    fn test_control_flow() {
        // If with same types
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(if_expr(bool(true), vec![int(1)], vec![int(2)])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Int);

        // If with different types -> void
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(if_expr(bool(true), vec![int(1)], vec![text("a")])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Record { fields: HashMap::new() });
        
        // If with one empty branch -> void
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(if_expr(bool(true), vec![int(1)], vec![])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Record { fields: HashMap::new() });

        // While loop
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(while_loop(bool(true), vec![int(1)])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Record { fields: HashMap::new() });

        // For loop over list
        let list = call("list", vec![var("Int"), int(1)]);
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(for_loop("k", "v", list, vec![var("v")])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Record { fields: HashMap::new() });

        // For loop over dict
        let dict = call("dict", vec![var("Text"), var("Real"), text("a"), real(1.0)]);
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(for_loop("k", "v", dict, vec![var("k"), var("v")])))]);
        assert_eq!(unwrap_lambda(&check(globals).unwrap()["a"]).tpe(), Type::Record { fields: HashMap::new() });
    }

    #[test]
    fn test_lambda_and_calls() {
        let my_func = lambda(vec![("x", var("Int"))], var("Int"), vec![var("x")]);
        let globals = HashMap::from([
            ("my_func".to_string(), my_func),
            ("result".to_string(), lambda(vec![], var("Int"), vec![call("my_func", vec![int(42)])])) 
        ]);

        let typed = check(globals).unwrap();
        assert_eq!(typed["result"].tpe(), Type::Func {
            args: vec![],
            ret: Box::new(Type::Int)
        });
    }

    #[test]
    fn test_error_cases() {
        // Invalid type name
        check(HashMap::from([("a".to_string(), lambda(vec![], var("Foo"), vec![]))])).expect_err("Invalid type name");

        // Invalid generic type
        check(HashMap::from([("a".to_string(), lambda(vec![], call("Foo", vec![var("Int")]), vec![]))])).expect_err("Invalid generic type");
        
        // Returns (), but expects List[List[Int]]
        check(HashMap::from([("a".to_string(), lambda(vec![], call("List", vec![call("List", vec![var("Int")])]), vec![]))])).expect_err("Missing return");
        
        // Repeated brackets in type
        check(HashMap::from([("a".to_string(), lambda(vec![], Expr::Call { line: 1, func: Box::new(call("List", vec![var("Int")])), args: vec![] }, vec![]))])).expect_err("Repeated brackets");

        // Invalid type expression
        check(HashMap::from([("a".to_string(), lambda(vec![], int(1), vec![]))])).expect_err("Invalid type expression");

        // Global not a literal or function
        check(HashMap::from([("a".to_string(), bin_op("+", int(1), int(2)))])).expect_err("Global not literal/func");

        // Mismatched assignment
        let globals = HashMap::from([("a".to_string(), lambda(vec![], var("Int"), vec![init("x", int(1)), assign("x", bool(true))]))]);
        check(globals).expect_err("Mismatched assignment");

        // Accessing field on non-record
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(access(int(1), "field")))]);
        check(globals).expect_err("Access non-record");

        // Accessing non-existent field
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(access(record(vec![]), "field")))]);
        check(globals).expect_err("Access non-existent field");

        // For loop on non-iterable
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(for_loop("k", "v", int(1), vec![])))]);
        check(globals).expect_err("For on non-iterable");

        // Mismatched lambda return type
        let globals = HashMap::from([("a".to_string(), lambda(vec![], var("Int"), vec![bool(true)]))]);
        check(globals).expect_err("Mismatched return type");

        // Wrong number of arguments
        let globals = HashMap::from([("f".to_string(), lambda(vec![("x", var("Int"))], var("Int"), vec![])), ("c".to_string(), wrap_in_lambda(call("f", vec![])))]);
        check(globals).expect_err("Wrong arg count");
        
        // Wrong argument type
        let globals = HashMap::from([("f".to_string(), lambda(vec![("x", var("Int"))], var("Int"), vec![])), ("c".to_string(), wrap_in_lambda(call("f", vec![bool(true)])))]);
        check(globals).expect_err("Wrong arg type");

        // Calling a non-function
        let globals = HashMap::from([("f".to_string(), int(1)), ("c".to_string(), wrap_in_lambda(call("f", vec![])))]);
        check(globals).expect_err("Calling non-function");

        // Invalid builtin call
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(call("list", vec![var("Int"), bool(true)])))]);
        check(globals).expect_err("Invalid builtin call");

        // Invalid binary operation
        let globals = HashMap::from([("a".to_string(), wrap_in_lambda(bin_op("+", int(1), bool(true))))]);
        check(globals).expect_err("Invalid binary op");
    }
}
