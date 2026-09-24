use std::collections::HashMap;

use crate::frontend::builtin::*;
use crate::frontend::error::{err, Error, Fallible};
use crate::frontend::nameres::Expr;

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
    Lambda { args: Vec<(String, Type)>, ret: Type, body: Box<Value>, tpe: Type },
    Init { name: String, value: Box<Value>, tpe: Type },
    Assign { name: String, value: Box<Value>, tpe: Type },
    If { cond: Box<Value>, then: Box<Value>, elsë: Box<Value>, tpe: Type },
    While { cond: Box<Value>, body: Box<Value>, tpe: Type },
    For { key: String, value: String, expr: Box<Value>, body: Box<Value>, tpe: Type },
    Block { values: Vec<Value>, tpe: Type }
}

impl Value {
    pub fn tpe(&self) -> Type {
        use Value::*;
        match self {
            Int { tpe, .. } | Real { tpe, .. } | Text { tpe, .. }
            | Bool { tpe, .. } | Record { tpe, .. } | Var { tpe, .. }
            | Call { tpe, .. } | Builtin { tpe, .. } | Access { tpe, .. }
            | Lambda { tpe, .. } | Init { tpe, .. } | Assign { tpe, .. }
            | If { tpe, .. } | While { tpe, .. } | For { tpe, .. }
            | Block { tpe, .. } => tpe.clone()
        }
    }
}

fn check_type(tpe: &Expr) -> Fallible<Type> {
    match tpe {
        Expr::Var { line, name, source } => {
            if let Some(tpe) = get_scalar_type(name) {
                Ok(tpe)
            } else {
                err(*line, format!("Type {} does not exist", source))
            }
        },
        Expr::Call { line, func, args } => {
            if let Expr::Var { name, source, .. } = &**func {
                let args = args.iter().map(check_type).collect::<Fallible<Vec<_>>>()?;
                if let Some(tpe) = get_generic_type(name, &args) {
                    Ok(tpe)
                } else {
                    err(*line, format!("Invalid generic type {}", source))
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

fn looks_like_type(e: &Expr) -> bool {
    match e {
        Expr::Var { name, .. } => is_type_name(name),
        Expr::Call { func, .. } => matches!(&**func, Expr::Var { name, .. } if is_type_name(name)),
        Expr::Record { fields, .. } => fields.values().all(looks_like_type),
        _ => false
    }
}

fn global_env(globals: &HashMap<String, Expr>, paths: &HashMap<String, String>) -> Fallible<HashMap<String, Type>> {
    let mut env = HashMap::new();
    for (name, value) in globals {
        let in_module = |e| in_module_of(name, paths, e);
        match value {
            Expr::Int { .. } => env.insert(name.clone(), Type::Int),
            Expr::Real { .. } => env.insert(name.clone(), Type::Real),
            Expr::Text { .. } => env.insert(name.clone(), Type::Text),
            Expr::Bool { .. } => env.insert(name.clone(), Type::Bool),
            Expr::Lambda { args, ret, .. } => {
                let ret = Box::new(check_type(ret).map_err(in_module)?);
                let args = args.iter().map(|(_, t)| {
                    check_type(t)
                }).collect::<Fallible<Vec<_>>>().map_err(in_module)?;
                env.insert(name.clone(), Type::Func { args, ret })
            }
            v => err(v.line(), "Global must be a literal or function".into()).map_err(in_module)?
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

fn check_var(line: i64, name: String, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    if let Some(var_type) = env.get(&name) {
        Ok(Value::Var { tpe: var_type.clone(), name })
    } else {
        err(line, format!("{} cannot be used as a value", name))
    }
}

fn check_cond(line: i64, cond: Expr, env: &mut HashMap<String, Type>) -> Fallible<Box<Value>> {
    let cond = check_value(cond, env)?;
    if cond.tpe() != Type::Bool {
        err(line, format!("Condition must be Bool, but it is {}", cond.tpe()))?
    }
    Ok(Box::new(cond))
}

fn check_init(name: String, value: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    env.insert(name.clone(), value.tpe());
    Ok(Value::Init { name, value, tpe: void() })
}

fn check_assign(line: i64, name: String, value: Expr, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let value = Box::new(check_value(value, env)?);
    let tpe = env.get(&name).unwrap().clone();
    if tpe != value.tpe() {
        err(line, format!("Assigning {} to {}", value.tpe(), tpe))?
    }
    Ok(Value::Assign { name, value, tpe: void() })
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

fn check_block(block: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let mut values = block.into_iter().map(|v| {
        check_value(v, env)
    }).collect::<Fallible<Vec<_>>>()?;

    if values.is_empty() {
        values.push(Value::Record { fields: HashMap::new(), tpe: void() });
    }

    let tpe = values.last().unwrap().tpe();
    Ok(Value::Block { values, tpe })
}

fn check_if(line: i64, cond: Expr, then: Vec<Expr>, elsë: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let cond = check_cond(line, cond, env)?;
    let then = Box::new(check_block(then, env)?);
    let elsë = Box::new(check_block(elsë, env)?);
    let tpe = if then.tpe() == elsë.tpe() {
        then.tpe()
    } else {
        void()
    };
    Ok(Value::If { cond, then, elsë, tpe })
}

fn check_while(line: i64, cond: Expr, body: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let cond = check_cond(line, cond, env)?;
    let body = Box::new(check_block(body, env)?);
    Ok(Value::While { cond, body, tpe: void() })
}

fn check_for(line: i64, key: String, value: String, expr: Expr, body: Vec<Expr>, env: &mut HashMap<String, Type>) -> Fallible<Value> {
    let expr = Box::new(check_value(expr, env)?);
    if let Type::Array { item } = expr.tpe() {
        env.insert(key.clone(), Type::Int);
        env.insert(value.clone(), *item);
    } else {
        err(line, "Expected an Array in the for loop".into())?;
    }
    let body = Box::new(check_block(body, env)?);
    Ok(Value::For { key, value, expr, body, tpe: void() })
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

    let body = Box::new(check_block(body, env)?);
    if ret == body.tpe() || ret == void() {
        Ok(Value::Lambda { args, ret, body, tpe })
    } else {
        err(line, format!("Return type is {}, but the function returns {}", ret, body.tpe()))
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
    let type_args = args.iter().take_while(|arg| {
        looks_like_type(arg)
    }).map(check_type).collect::<Fallible<Vec<_>>>()?;
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
        Expr::Var { line, name, .. } => check_var(line, name, env),
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
        Expr::If { line, cond, then, elsë } => {
            check_if(line, *cond, then, elsë, env)
        }
        Expr::While { line, cond, body } => {
            check_while(line, *cond, body, env)
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

fn check_main(main: &str, globals: &HashMap<String, Expr>, env: &HashMap<String, Type>) -> Fallible<()> {
    match env.get(main) {
        Some(Type::Func { args, .. }) if args.is_empty() => Ok(()),
        _ => err(globals[main].line(), "main must be a function with no arguments".into())
    }
}

fn in_module_of(global: &str, paths: &HashMap<String, String>, e: Error) -> Error {
    match paths.get(global) {
        Some(path) => e.in_file(path),
        None => e
    }
}

pub fn check(main: &str, globals: HashMap<String, Expr>, paths: &HashMap<String, String>) -> Fallible<HashMap<String, Value>> {
    let mut env = global_env(&globals, paths)?;
    check_main(main, &globals, &env).map_err(|e| in_module_of(main, paths, e))?;
    globals.into_iter().map(|(name, value)| {
        let value = check_value(value, &mut env).map_err(|e| in_module_of(&name, paths, e))?;
        Ok((name, value))
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::error::Error;


    fn int(value: i64) -> Expr { Expr::Int { line: 1, value } }
    fn real(value: f64) -> Expr { Expr::Real { line: 1, value } }
    fn text(value: &str) -> Expr { Expr::Text { line: 1, value: value.into() } }
    fn bool(value: bool) -> Expr { Expr::Bool { line: 1, value } }
    fn var(name: &str) -> Expr { Expr::Var { line: 1, name: name.into(), source: name.into() } }

    fn call(func: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call { line: 1, func: Box::new(func), args }
    }

    fn builtin(name: &str, args: Vec<Expr>) -> Expr {
        call(var(name), args)
    }

    fn op(lhs: Expr, name: &str, rhs: Expr) -> Expr {
        Expr::BinOp { line: 1, name: name.into(), lhs: Box::new(lhs), rhs: Box::new(rhs) }
    }

    fn record(fields: Vec<(&str, Expr)>) -> Expr {
        Expr::Record { line: 1, fields: fields.into_iter().map(|(k, v)| (k.into(), v)).collect() }
    }

    fn access(object: Expr, field: &str) -> Expr {
        Expr::Access { line: 1, object: Box::new(object), field: field.into() }
    }

    fn init(name: &str, value: Expr) -> Expr {
        Expr::Init { line: 1, name: name.into(), value: Box::new(value) }
    }

    fn assign(name: &str, value: Expr) -> Expr {
        Expr::Assign { line: 1, name: name.into(), value: Box::new(value) }
    }

    fn lambda(args: Vec<(&str, Expr)>, ret: Expr, body: Vec<Expr>) -> Expr {
        Expr::Lambda {
            line: 1,
            args: args.into_iter().map(|(n, t)| (n.into(), t)).collect(),
            ret: Box::new(ret),
            body
        }
    }

    fn if_else(cond: Expr, then: Vec<Expr>, elsë: Vec<Expr>) -> Expr {
        Expr::If { line: 1, cond: Box::new(cond), then, elsë }
    }

    fn while_loop(cond: Expr, body: Vec<Expr>) -> Expr {
        Expr::While { line: 1, cond: Box::new(cond), body }
    }

    fn for_loop(key: &str, value: &str, expr: Expr, body: Vec<Expr>) -> Expr {
        Expr::For { line: 1, key: key.into(), value: value.into(), expr: Box::new(expr), body }
    }

    fn at(new_line: i64, mut e: Expr) -> Expr {
        use Expr::*;
        match &mut e {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | BinOp { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Assign { line, .. } | Init { line, .. }
            | If { line, .. } | While { line, .. } | For { line, .. } => *line = new_line
        }
        e
    }


    fn t(name: &str) -> Expr { var(name) }
    fn t_array(item: Expr) -> Expr { builtin("Array", vec![item]) }
    fn t_func(mut args: Vec<Expr>, ret: Expr) -> Expr {
        args.push(ret);
        builtin("Func", args)
    }
    fn t_void() -> Expr { record(vec![]) }


    use Type::{Int, Real, Text, Bool};

    fn array(item: Type) -> Type { Type::Array { item: Box::new(item) } }
    fn func(args: Vec<Type>, ret: Type) -> Type { Type::Func { args, ret: Box::new(ret) } }
    fn rec(fields: Vec<(&str, Type)>) -> Type {
        Type::Record { fields: fields.into_iter().map(|(k, v)| (k.into(), v)).collect() }
    }


    fn main_with(body: Vec<Expr>) -> Expr {
        lambda(vec![], t_void(), body)
    }

    fn check_program(globals: Vec<(&str, Expr)>) -> Fallible<HashMap<String, Value>> {
        check("main", globals.into_iter().map(|(k, v)| (k.into(), v)).collect(), &HashMap::new())
    }

    fn check_body_with(globals: Vec<(&str, Expr)>, body: Vec<Expr>) -> Fallible<Vec<Value>> {
        let mut globals = globals;
        globals.push(("main", main_with(body)));
        let mut checked = check_program(globals)?;
        match checked.remove("main") {
            Some(Value::Lambda { body, .. }) => match *body {
                Value::Block { values, .. } => Ok(values),
                other => panic!("Function body is not a block: {:?}", other)
            },
            other => panic!("main is not a function: {:?}", other)
        }
    }

    fn value_of(body: Vec<Expr>) -> Value {
        check_body_with(vec![], body).unwrap().pop().unwrap()
    }

    fn type_of(body: Vec<Expr>) -> Type {
        value_of(body).tpe()
    }

    fn type_of_expr(expr: Expr) -> Type {
        type_of(vec![expr])
    }

    fn error_of(body: Vec<Expr>) -> Error {
        check_body_with(vec![], body).unwrap_err()
    }

    fn error(line: i64, message: &str) -> Error {
        Error { file: String::new(), line, message: message.into() }
    }

    mod literals {
        use super::*;

        #[test]
        fn scalars_have_their_own_types() {
            assert_eq!(type_of_expr(int(1)), Int);
            assert_eq!(type_of_expr(real(1.5)), Real);
            assert_eq!(type_of_expr(text("a")), Text);
            assert_eq!(type_of_expr(bool(true)), Bool);
        }

        #[test]
        fn record_has_the_types_of_its_fields() {
            let r = record(vec![("a", int(1)), ("b", record(vec![("c", text("x"))]))]);
            assert_eq!(type_of_expr(r), rec(vec![("a", Int), ("b", rec(vec![("c", Text)]))]));
        }

        #[test]
        fn empty_record_is_void() {
            assert_eq!(type_of_expr(record(vec![])), void());
        }

        #[test]
        fn literal_keeps_its_value() {
            assert_eq!(value_of(vec![int(42)]), Value::Int { value: 42, tpe: Int });
        }
    }

    mod type_expressions {
        use super::*;

        fn type_of_annotation(tpe: Expr) -> Fallible<Type> {
            let f = lambda(vec![("x", tpe)], t_void(), vec![]);
            let checked = check_body_with(vec![("f", f)], vec![var("f")])?;
            match checked.last().unwrap().tpe() {
                Type::Func { args, .. } => Ok(args[0].clone()),
                other => panic!("Not a function: {}", other)
            }
        }

        #[test]
        fn scalar_types() {
            for (name, tpe) in [("Int", Int), ("Real", Real), ("Text", Text), ("Bool", Bool)] {
                assert_eq!(type_of_annotation(t(name)), Ok(tpe));
            }
        }

        #[test]
        fn array_type_takes_one_item_type() {
            assert_eq!(type_of_annotation(t_array(t("Int"))), Ok(array(Int)));
            assert_eq!(type_of_annotation(t_array(t_array(t("Text")))), Ok(array(array(Text))));
        }

        #[test]
        fn func_type_lists_arguments_then_return_type() {
            assert_eq!(
                type_of_annotation(t_func(vec![t("Int"), t("Text")], t("Bool"))),
                Ok(func(vec![Int, Text], Bool))
            );
            assert_eq!(type_of_annotation(t_func(vec![], t("Int"))), Ok(func(vec![], Int)));
        }

        #[test]
        fn record_type_maps_fields_to_types() {
            let tpe = record(vec![("a", t("Int")), ("b", t_array(t("Real")))]);
            assert_eq!(type_of_annotation(tpe), Ok(rec(vec![("a", Int), ("b", array(Real))])));
            assert_eq!(type_of_annotation(t_void()), Ok(void()));
        }

        #[test]
        fn unknown_type_name_is_rejected() {
            assert_eq!(type_of_annotation(at(3, var("x"))), Err(error(3, "Type x does not exist")));
            assert_eq!(type_of_annotation(at(3, var("Array"))), Err(error(3, "Type Array does not exist")));
        }

        #[test]
        fn generic_type_with_wrong_arguments_is_rejected() {
            let invalid = error(3, "Invalid generic type Array");
            assert_eq!(type_of_annotation(at(3, builtin("Array", vec![]))), Err(invalid.clone()));
            assert_eq!(type_of_annotation(at(3, builtin("Array", vec![t("Int"), t("Int")]))), Err(invalid));
            assert_eq!(
                type_of_annotation(at(3, builtin("Func", vec![]))),
                Err(error(3, "Invalid generic type Func"))
            );
            assert_eq!(
                type_of_annotation(at(3, builtin("Int", vec![t("Int")]))),
                Err(error(3, "Invalid generic type Int"))
            );
        }

        #[test]
        fn repeated_brackets_are_rejected() {
            let tpe = at(3, call(t_array(t("Int")), vec![t("Int")]));
            assert_eq!(type_of_annotation(tpe), Err(error(3, "Repeated brackets in type declaration")));
        }

        #[test]
        fn errors_show_the_name_from_the_source() {
            let x = Expr::Var { line: 3, name: "_m0_x".into(), source: "ns::x".into() };
            assert_eq!(type_of_annotation(x.clone()), Err(error(3, "Type ns::x does not exist")));
            assert_eq!(type_of_annotation(at(3, call(x, vec![t("Int")]))), Err(error(3, "Invalid generic type ns::x")));
        }

        #[test]
        fn value_is_not_a_type() {
            assert_eq!(type_of_annotation(at(3, int(1))), Err(error(3, "Invalid type expression")));
            let field = record(vec![("a", at(4, int(1)))]);
            assert_eq!(type_of_annotation(field), Err(error(4, "Invalid type expression")));
        }
    }

    mod globals {
        use super::*;

        #[test]
        fn every_global_is_returned_typed() {
            let checked = check_program(vec![
                ("n", int(1)),
                ("x", real(1.0)),
                ("s", text("a")),
                ("b", bool(false)),
                ("f", lambda(vec![("a", t("Int"))], t("Int"), vec![var("a")])),
                ("main", main_with(vec![])),
            ]).unwrap();
            let mut types: Vec<_> = checked.iter().map(|(k, v)| (k.as_str(), v.tpe())).collect();
            types.sort_by_key(|(k, _)| *k);
            assert_eq!(types, vec![
                ("b", Bool), ("f", func(vec![Int], Int)), ("main", func(vec![], void())),
                ("n", Int), ("s", Text), ("x", Real),
            ]);
        }

        #[test]
        fn globals_are_visible_from_functions() {
            let checked = check_body_with(vec![("n", int(1)), ("s", text("a"))], vec![var("n"), var("s")]).unwrap();
            assert_eq!(checked[0].tpe(), Int);
            assert_eq!(checked[1].tpe(), Text);
        }

        #[test]
        fn functions_can_call_each_other_and_themselves() {
            let even = lambda(vec![("n", t("Int"))], t("Bool"), vec![
                call(var("odd"), vec![var("n")])
            ]);
            let odd = lambda(vec![("m", t("Int"))], t("Bool"), vec![
                call(var("even"), vec![var("m")])
            ]);
            let fact = lambda(vec![("k", t("Int"))], t("Int"), vec![
                op(var("k"), "*", call(var("fact"), vec![var("k")]))
            ]);
            let body = vec![call(var("even"), vec![int(1)]), call(var("fact"), vec![int(3)])];
            let checked = check_body_with(vec![("even", even), ("odd", odd), ("fact", fact)], body).unwrap();
            assert_eq!(checked[0].tpe(), Bool);
            assert_eq!(checked[1].tpe(), Int);
        }

        #[test]
        fn global_must_be_a_literal_or_a_function() {
            let message = "Global must be a literal or function";
            for value in [
                op(int(1), "+", int(2)),
                record(vec![]),
                builtin("len", vec![text("a")]),
                var("n"),
            ] {
                let result = check_program(vec![("n", int(1)), ("g", at(5, value)), ("main", main_with(vec![]))]);
                assert_eq!(result.unwrap_err(), error(5, message));
            }
        }

        #[test]
        fn function_signature_must_be_valid() {
            let f = lambda(vec![("a", at(4, var("Foo")))], t("Int"), vec![int(1)]);
            let result = check_program(vec![("f", f), ("main", main_with(vec![]))]);
            assert_eq!(result.unwrap_err(), error(4, "Type Foo does not exist"));

            let g = lambda(vec![], at(4, var("Foo")), vec![int(1)]);
            let result = check_program(vec![("g", g), ("main", main_with(vec![]))]);
            assert_eq!(result.unwrap_err(), error(4, "Type Foo does not exist"));
        }
    }

    mod main_function {
        use super::*;

        #[test]
        fn main_may_return_anything() {
            for (ret, body) in [(t_void(), vec![]), (t("Int"), vec![int(0)]), (t("Text"), vec![text("")])] {
                assert!(check_program(vec![("main", lambda(vec![], ret, body))]).is_ok());
            }
        }

        #[test]
        fn main_must_not_take_arguments() {
            let main = at(2, lambda(vec![("a", t("Int"))], t_void(), vec![]));
            assert_eq!(
                check_program(vec![("main", main)]).unwrap_err(),
                error(2, "main must be a function with no arguments")
            );
        }

        #[test]
        fn main_must_be_a_function() {
            assert_eq!(
                check_program(vec![("main", at(2, int(5)))]).unwrap_err(),
                error(2, "main must be a function with no arguments")
            );
        }
    }

    mod variables {
        use super::*;

        #[test]
        fn initialization_is_void_and_defines_the_type() {
            let checked = check_body_with(vec![], vec![init("x", text("a")), var("x")]).unwrap();
            assert_eq!(checked[0].tpe(), void());
            assert_eq!(checked[1], Value::Var { name: "x".into(), tpe: Text });
        }

        #[test]
        fn assignment_of_the_same_type_is_void() {
            let body = vec![init("x", int(1)), assign("x", op(var("x"), "+", int(1)))];
            assert_eq!(type_of(body), void());
        }

        #[test]
        fn assignment_cannot_change_the_type() {
            let body = vec![init("x", int(1)), at(3, assign("x", real(1.0)))];
            assert_eq!(error_of(body), error(3, "Assigning Real to Int"));
        }

        #[test]
        fn builtin_cannot_be_used_as_a_value() {
            assert_eq!(error_of(vec![at(3, var("len"))]), error(3, "len cannot be used as a value"));
            assert_eq!(error_of(vec![at(3, var("Int"))]), error(3, "Int cannot be used as a value"));
            assert_eq!(error_of(vec![init("f", at(3, var("print")))]), error(3, "print cannot be used as a value"));
        }
    }

    mod records {
        use super::*;

        #[test]
        fn access_gives_the_field_type() {
            let r = record(vec![("a", int(1)), ("b", text("x"))]);
            assert_eq!(type_of_expr(access(r.clone(), "b")), Text);
            let nested = record(vec![("inner", r)]);
            assert_eq!(type_of_expr(access(access(nested, "inner"), "a")), Int);
        }

        #[test]
        fn access_to_a_missing_field_is_rejected() {
            let r = record(vec![("a", int(1))]);
            assert_eq!(error_of(vec![at(3, access(r, "b"))]), error(3, "Record does not have a field b"));
        }

        #[test]
        fn access_on_a_non_record_is_rejected() {
            assert_eq!(error_of(vec![at(3, access(int(1), "a"))]), error(3, "Int is not a record"));
        }

        #[test]
        fn merge_takes_the_union_and_the_right_side_wins() {
            let lhs = record(vec![("a", int(1)), ("b", int(2))]);
            let rhs = record(vec![("b", text("x")), ("c", bool(true))]);
            assert_eq!(type_of_expr(op(lhs, "|", rhs)), rec(vec![("a", Int), ("b", Text), ("c", Bool)]));
            assert_eq!(type_of_expr(op(record(vec![]), "|", record(vec![]))), void());
        }
    }

    mod blocks {
        use super::*;

        fn block_type(body: Vec<Expr>) -> Type {
            type_of_expr(if_else(bool(true), body.clone(), body))
        }

        #[test]
        fn block_has_the_type_of_its_last_expression() {
            assert_eq!(block_type(vec![text("a"), int(1)]), Int);
            assert_eq!(block_type(vec![int(1), init("x", int(1))]), void());
        }

        #[test]
        fn empty_block_is_void() {
            assert_eq!(block_type(vec![]), void());
        }

        #[test]
        fn error_anywhere_in_a_block_is_reported() {
            let body = vec![int(1), at(4, op(int(1), "+", text("a"))), int(2)];
            assert_eq!(error_of(body), error(4, "Operator + does not accept Int and Text"));
        }
    }

    mod conditions {
        use super::*;

        #[test]
        fn if_with_equal_branch_types_has_that_type() {
            assert_eq!(type_of_expr(if_else(bool(true), vec![int(1)], vec![int(2)])), Int);
            let r = || record(vec![("a", text("x"))]);
            assert_eq!(type_of_expr(if_else(bool(true), vec![r()], vec![r()])), rec(vec![("a", Text)]));
        }

        #[test]
        fn if_with_different_branch_types_is_void() {
            assert_eq!(type_of_expr(if_else(bool(true), vec![int(1)], vec![real(2.0)])), void());
            assert_eq!(type_of_expr(if_else(bool(true), vec![int(1)], vec![])), void());
        }

        #[test]
        fn if_condition_must_be_bool() {
            for cond in [int(1), text("a"), record(vec![])] {
                let tpe = type_of_expr(cond.clone());
                assert_eq!(
                    error_of(vec![at(3, if_else(cond, vec![], vec![]))]),
                    error(3, &format!("Condition must be Bool, but it is {}", tpe))
                );
            }
        }

        #[test]
        fn while_is_void_whatever_the_body() {
            assert_eq!(type_of_expr(while_loop(bool(false), vec![int(1)])), void());
            assert_eq!(type_of_expr(while_loop(bool(false), vec![])), void());
        }

        #[test]
        fn while_condition_must_be_bool() {
            assert_eq!(
                error_of(vec![at(3, while_loop(int(1), vec![]))]),
                error(3, "Condition must be Bool, but it is Int")
            );
        }

        #[test]
        fn errors_in_branches_are_reported() {
            let bad = || at(4, access(int(1), "a"));
            assert_eq!(error_of(vec![if_else(bool(true), vec![bad()], vec![])]), error(4, "Int is not a record"));
            assert_eq!(error_of(vec![if_else(bool(true), vec![], vec![bad()])]), error(4, "Int is not a record"));
            assert_eq!(error_of(vec![while_loop(bool(true), vec![bad()])]), error(4, "Int is not a record"));
        }
    }

    mod for_loops {
        use super::*;

        fn texts() -> Expr {
            builtin("array", vec![t("Text"), int(3)])
        }

        #[test]
        fn key_is_the_index_and_value_is_the_item() {
            let body = vec![for_loop("i", "s", texts(), vec![
                init("a", var("i")),
                init("b", var("s")),
                assign("a", int(0)),
                assign("b", text("")),
            ])];
            assert_eq!(type_of(body), void());
        }

        #[test]
        fn for_is_void_whatever_the_body() {
            assert_eq!(type_of_expr(for_loop("i", "s", texts(), vec![var("s")])), void());
        }

        #[test]
        fn only_arrays_can_be_iterated() {
            for expr in [text("abc"), int(3)] {
                assert_eq!(
                    error_of(vec![at(3, for_loop("i", "x", expr, vec![]))]),
                    error(3, "Expected an Array in the for loop")
                );
            }
        }

        #[test]
        fn key_has_type_int() {
            let body = vec![for_loop("i", "s", texts(), vec![
                at(4, op(var("i"), "+", var("s")))
            ])];
            assert_eq!(error_of(body), error(4, "Operator + does not accept Int and Text"));
        }
    }

    mod lambdas {
        use super::*;

        #[test]
        fn lambda_has_a_function_type() {
            let f = lambda(vec![("a", t("Int")), ("b", t("Text"))], t("Bool"), vec![bool(true)]);
            assert_eq!(type_of_expr(f), func(vec![Int, Text], Bool));
        }

        #[test]
        fn arguments_are_visible_in_the_body() {
            let f = lambda(vec![("a", t("Int"))], t("Int"), vec![op(var("a"), "*", int(2))]);
            assert_eq!(type_of_expr(f), func(vec![Int], Int));
        }

        #[test]
        fn outer_variables_can_be_captured() {
            let f = lambda(vec![], t("Text"), vec![var("s")]);
            assert_eq!(type_of(vec![init("s", text("a")), f]), func(vec![], Text));
        }

        #[test]
        fn body_must_match_the_return_type() {
            let f = at(3, lambda(vec![], t("Int"), vec![text("a")]));
            assert_eq!(error_of(vec![f]), error(3, "Return type is Int, but the function returns Text"));
            let g = at(3, lambda(vec![], t("Int"), vec![]));
            assert_eq!(error_of(vec![g]), error(3, "Return type is Int, but the function returns ()"));
        }

        #[test]
        fn void_return_type_discards_the_body() {
            let f = lambda(vec![], t_void(), vec![int(1)]);
            assert_eq!(type_of_expr(f), func(vec![], void()));
        }

        #[test]
        fn invalid_argument_type_is_rejected() {
            let f = lambda(vec![("a", at(3, int(1)))], t_void(), vec![]);
            assert_eq!(error_of(vec![f]), error(3, "Invalid type expression"));
        }
    }

    mod calls {
        use super::*;

        fn with_f(body: Vec<Expr>) -> Fallible<Vec<Value>> {
            let f = lambda(vec![("a", t("Int")), ("b", t("Text"))], t("Real"), vec![real(1.0)]);
            check_body_with(vec![("f", f)], body)
        }

        #[test]
        fn call_has_the_return_type() {
            let checked = with_f(vec![call(var("f"), vec![int(1), text("a")])]).unwrap();
            assert_eq!(checked[0].tpe(), Real);
        }

        #[test]
        fn number_of_arguments_must_match() {
            assert_eq!(
                with_f(vec![at(3, call(var("f"), vec![int(1)]))]).unwrap_err(),
                error(3, "Expected 2 arguments but got 1")
            );
            assert_eq!(
                with_f(vec![at(3, call(var("f"), vec![int(1), text("a"), int(2)]))]).unwrap_err(),
                error(3, "Expected 2 arguments but got 3")
            );
        }

        #[test]
        fn argument_types_must_match() {
            assert_eq!(
                with_f(vec![at(3, call(var("f"), vec![int(1), int(2)]))]).unwrap_err(),
                error(3, "Expected Text but got Int")
            );
        }

        #[test]
        fn only_functions_can_be_called() {
            assert_eq!(error_of(vec![at(3, call(int(1), vec![]))]), error(3, "Int is not a function"));
        }

        #[test]
        fn functions_are_first_class() {
            let apply = lambda(
                vec![("g", t_func(vec![t("Int")], t("Int"))), ("x", t("Int"))],
                t("Int"),
                vec![call(var("g"), vec![var("x")])]
            );
            let double = lambda(vec![("y", t("Int"))], t("Int"), vec![op(var("y"), "*", int(2))]);
            let body = vec![call(var("apply"), vec![double, int(3)])];
            assert_eq!(check_body_with(vec![("apply", apply)], body).unwrap()[0].tpe(), Int);

            let direct = call(lambda(vec![], t("Text"), vec![text("a")]), vec![]);
            assert_eq!(type_of_expr(direct), Text);
        }
    }

    mod builtin_functions {
        use super::*;

        fn type_of_builtin(name: &str, args: Vec<Expr>) -> Fallible<Type> {
            check_body_with(vec![], vec![at(3, builtin(name, args))]).map(|v| v[0].tpe())
        }

        fn ints() -> Expr {
            builtin("array", vec![t("Int"), int(2)])
        }

        #[test]
        fn array_takes_the_item_type_and_the_length() {
            assert_eq!(type_of_builtin("array", vec![t("Int"), int(2)]), Ok(array(Int)));
            assert_eq!(type_of_builtin("array", vec![t_array(t("Real")), int(2)]), Ok(array(array(Real))));
            assert_eq!(type_of_builtin("array", vec![t_void(), int(2)]), Ok(array(void())));
        }

        #[test]
        fn type_arguments_are_not_values() {
            let checked = value_of(vec![builtin("array", vec![t("Text"), int(2)])]);
            assert_eq!(checked, Value::Builtin {
                op: "array".into(),
                args: vec![Value::Int { value: 2, tpe: Int }],
                tpe: array(Text)
            });
        }

        #[test]
        fn array_needs_exactly_one_type_and_an_int() {
            let err = |sig: &str| Err(error(3, &format!("There is no built-in function array[{}]", sig)));
            assert_eq!(type_of_builtin("array", vec![int(2)]), err("Int"));
            assert_eq!(type_of_builtin("array", vec![t("Int"), t("Int"), int(2)]), err("Int, Int, Int"));
            assert_eq!(type_of_builtin("array", vec![t("Int"), real(2.0)]), err("Int, Real"));
        }

        #[test]
        fn conversions() {
            for (name, arg, result) in [
                ("int", int(1), Int), ("int", real(1.0), Int), ("int", bool(true), Int),
                ("real", int(1), Real), ("real", real(1.0), Real),
                ("bool", int(1), Bool), ("bool", bool(true), Bool),
                ("text", text("a"), Text),
                ("chr", int(65), Text),
            ] {
                assert_eq!(type_of_builtin(name, vec![arg]), Ok(result), "{}", name);
            }
        }

        #[test]
        fn unsupported_conversions_are_rejected() {
            for (name, arg, sig) in [
                ("int", text("1"), "Text"),
                ("real", bool(true), "Bool"),
                ("real", text("1"), "Text"),
                ("bool", real(1.0), "Real"),
                ("text", int(1), "Int"),
                ("chr", real(1.0), "Real"),
                ("int", record(vec![("a", int(1))]), "(a: Int)"),
            ] {
                assert_eq!(
                    type_of_builtin(name, vec![arg]),
                    Err(error(3, &format!("There is no built-in function {}[{}]", name, sig)))
                );
            }
            assert_eq!(
                type_of_builtin("int", vec![int(1), int(2)]),
                Err(error(3, "There is no built-in function int[Int, Int]"))
            );
        }

        #[test]
        fn not_is_bitwise_on_int_and_logical_on_bool() {
            assert_eq!(type_of_builtin("not", vec![int(1)]), Ok(Int));
            assert_eq!(type_of_builtin("not", vec![bool(true)]), Ok(Bool));
            assert_eq!(type_of_builtin("not", vec![real(1.0)]), Err(error(3, "There is no built-in function not[Real]")));
        }

        #[test]
        fn print_takes_any_number_of_scalars() {
            assert_eq!(type_of_builtin("print", vec![]), Ok(void()));
            assert_eq!(type_of_builtin("print", vec![int(1), real(1.0), text("a"), bool(true)]), Ok(void()));
            assert_eq!(
                type_of_builtin("print", vec![int(1), ints()]),
                Err(error(3, "There is no built-in function print[Int, Array[Int]]"))
            );
            assert_eq!(
                type_of_builtin("print", vec![record(vec![("a", int(1))])]),
                Err(error(3, "There is no built-in function print[(a: Int)]"))
            );
        }

        #[test]
        fn len_of_text_and_arrays() {
            assert_eq!(type_of_builtin("len", vec![text("abc")]), Ok(Int));
            assert_eq!(type_of_builtin("len", vec![ints()]), Ok(Int));
            assert_eq!(type_of_builtin("len", vec![int(1)]), Err(error(3, "There is no built-in function len[Int]")));
        }

        #[test]
        fn get_gives_the_item_or_the_character_code() {
            assert_eq!(type_of_builtin("get", vec![ints(), int(0)]), Ok(Int));
            let texts = builtin("array", vec![t("Text"), int(2)]);
            assert_eq!(type_of_builtin("get", vec![texts, int(0)]), Ok(Text));
            assert_eq!(type_of_builtin("get", vec![text("abc"), int(0)]), Ok(Int));
            assert_eq!(
                type_of_builtin("get", vec![ints(), real(0.0)]),
                Err(error(3, "There is no built-in function get[Array[Int], Real]"))
            );
        }

        #[test]
        fn set_needs_an_item_of_the_array_type() {
            assert_eq!(type_of_builtin("set", vec![ints(), int(0), int(5)]), Ok(void()));
            assert_eq!(
                type_of_builtin("set", vec![ints(), int(0), text("a")]),
                Err(error(3, "There is no built-in function set[Array[Int], Int, Text]"))
            );
            assert_eq!(
                type_of_builtin("set", vec![text("abc"), int(0), int(65)]),
                Err(error(3, "There is no built-in function set[Text, Int, Int]"))
            );
        }

        #[test]
        fn types_are_not_callable_as_functions() {
            assert_eq!(type_of_builtin("Int", vec![int(1)]), Err(error(3, "There is no built-in function Int[Int]")));
        }

        #[test]
        fn errors_in_type_arguments_are_reported() {
            let bad = t_array(at(4, var("x")));
            assert_eq!(type_of_builtin("array", vec![bad, int(2)]), Err(error(4, "Type x does not exist")));
            let bad = record(vec![("a", at(4, var("Array")))]);
            assert_eq!(type_of_builtin("array", vec![bad, int(2)]), Err(error(4, "Type Array does not exist")));
        }

        #[test]
        fn errors_in_arguments_are_reported() {
            let bad = at(4, access(int(1), "a"));
            assert_eq!(type_of_builtin("print", vec![bad]), Err(error(4, "Int is not a record")));
        }
    }

    mod operators {
        use super::*;

        fn type_of_op(lhs: Expr, name: &str, rhs: Expr) -> Fallible<Type> {
            check_body_with(vec![], vec![at(3, op(lhs, name, rhs))]).map(|v| v[0].tpe())
        }

        fn sample(tpe: &Type) -> Expr {
            match tpe {
                Int => int(1),
                Real => real(1.0),
                Text => text("a"),
                Bool => bool(true),
                _ => record(vec![]),
            }
        }

        const TABLE: &[(&str, &[(Type, Type)])] = &[
            ("*", &[(Int, Int), (Real, Real)]),
            ("/", &[(Int, Int), (Real, Real)]),
            ("%", &[(Int, Int)]),
            ("+", &[(Int, Int), (Real, Real), (Text, Text)]),
            ("-", &[(Int, Int), (Real, Real)]),
            ("<<", &[(Int, Int)]),
            (">>", &[(Int, Int)]),
            (">>>", &[(Int, Int)]),
            ("&", &[(Int, Int), (Bool, Bool)]),
            ("|", &[(Int, Int), (Bool, Bool)]),
            ("^", &[(Int, Int), (Bool, Bool)]),
            ("==", &[(Int, Bool), (Real, Bool), (Text, Bool), (Bool, Bool)]),
            ("!=", &[(Int, Bool), (Real, Bool), (Text, Bool), (Bool, Bool)]),
            ("<", &[(Int, Bool), (Real, Bool), (Text, Bool)]),
            ("<=", &[(Int, Bool), (Real, Bool), (Text, Bool)]),
            (">", &[(Int, Bool), (Real, Bool), (Text, Bool)]),
            (">=", &[(Int, Bool), (Real, Bool), (Text, Bool)]),
            ("&&", &[(Bool, Bool)]),
            ("||", &[(Bool, Bool)]),
        ];

        #[test]
        fn operators_accept_exactly_the_listed_scalar_types() {
            let scalars = [Int, Real, Text, Bool];
            for (name, accepted) in TABLE {
                for lhs in &scalars {
                    for rhs in &scalars {
                        let result = type_of_op(sample(lhs), name, sample(rhs));
                        let expected = accepted.iter()
                            .find(|(operand, _)| operand == lhs && lhs == rhs)
                            .map(|(_, result)| result.clone())
                            .ok_or_else(|| error(3, &format!("Operator {} does not accept {} and {}", name, lhs, rhs)));
                        assert_eq!(result, expected, "{} {} {}", lhs, name, rhs);
                    }
                }
            }
        }

        #[test]
        fn operands_keep_their_order() {
            let checked = value_of(vec![op(int(1), "-", int(2))]);
            assert_eq!(checked, Value::Builtin {
                op: "-".into(),
                args: vec![Value::Int { value: 1, tpe: Int }, Value::Int { value: 2, tpe: Int }],
                tpe: Int
            });
        }

        #[test]
        fn only_merge_accepts_records() {
            let r = || record(vec![("a", int(1))]);
            for (name, _) in TABLE.iter().filter(|(name, _)| *name != "|") {
                assert_eq!(
                    type_of_op(r(), name, r()),
                    Err(error(3, &format!("Operator {} does not accept (a: Int) and (a: Int)", name)))
                );
            }
            assert_eq!(
                type_of_op(r(), "|", int(1)),
                Err(error(3, "Operator | does not accept (a: Int) and Int"))
            );
        }

        #[test]
        fn arrays_and_functions_are_not_comparable() {
            let arr = || builtin("array", vec![t("Int"), int(1)]);
            assert_eq!(
                type_of_op(arr(), "==", arr()),
                Err(error(3, "Operator == does not accept Array[Int] and Array[Int]"))
            );
            let f = || lambda(vec![], t("Int"), vec![int(1)]);
            assert_eq!(
                type_of_op(f(), "==", f()),
                Err(error(3, "Operator == does not accept Func[Int] and Func[Int]"))
            );
        }

        #[test]
        fn errors_in_operands_are_reported() {
            let bad = || at(4, access(int(1), "a"));
            assert_eq!(type_of_op(bad(), "+", int(1)), Err(error(4, "Int is not a record")));
            assert_eq!(type_of_op(int(1), "+", bad()), Err(error(4, "Int is not a record")));
        }
    }
}
