use std::collections::HashMap;

use crate::builtin::{is_builtin_type, is_builtin_function};
use crate::error::{err, Fallible};
use crate::parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Scalar { line: i64, name: String },
    Generic { line: i64, template: String, args: Vec<Type> },
    Record { line: i64, fields: HashMap<String, Type> }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Value> },
    Var { line: i64, name: String },
    Call { line: i64, func: Box<Value>, args: Vec<Value> },
    BinOp { line: i64, name: String, lhs: Box<Value>, rhs: Box<Value> },
    Access { line: i64, object: Box<Value>, field: String },
    Lambda { line: i64, args: Vec<(String, Type)>, return_type: Type, body: Vec<Value> },
    Assign { line: i64, name: String, value: Box<Value> },
    If { line: i64, cond: Box<Value>, then: Vec<Value>, otherwise: Vec<Value> },
    While { line: i64, cond: Box<Value>, body: Vec<Value> },
    For { line: i64, key: String, value: String, expr: Box<Value>, body: Vec<Value> },
}

impl Value {
    pub fn line(&self) -> i64 {
        use Value::*;
        match self {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | BinOp { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Assign { line, .. } | If { line, .. }
            | While { line, .. } | For { line, .. } => *line
        }
    }
}

trait Environment {
    fn new_unique_id(&mut self) -> String;
    fn var_name(&self, module_path: &str, name: &str) -> Option<String>;
    fn ns_path(&self, module_path: &str, ns_name: &str) -> Option<String>;
}

struct GlobalEnv {
    consts: HashMap<(String, String), String>,
    imports: HashMap<(String, String), String>,
    counter: i64
}

impl GlobalEnv {
    fn new(files: &HashMap<String, Vec<parser::Decl>>) -> Fallible<GlobalEnv> {
        let mut consts = HashMap::new();
        let mut imports = HashMap::new();
        let mut i = 0;
        for (module_path, decls) in files {
            for decl in decls {
                if let parser::Decl::Const { line, name, private: false, .. } = decl {
                    let uniq = format!("_m{}_{}", i, name);
                    if consts.insert((module_path.clone(), name.clone()), uniq).is_some() {
                        err(*line, format!("Cannot declare {} twice", name))?;
                    }
                } else if let parser::Decl::Import { line, path, .. } = decl {
                    let ns = path.split('/').last().unwrap().replace(".zyba", "");
                    if imports.insert((module_path.clone(), ns.clone()), path.clone()).is_some() {
                        err(*line, format!("Cannot import '{}', duplicate namespace {}", path, ns))?;
                    }
                }
            }
            i += 1;
        }
        Ok(GlobalEnv { consts, imports, counter: 0 })
    }
}

impl Environment for GlobalEnv {
    fn new_unique_id(&mut self) -> String {
        self.counter += 1;
        format!("_v{}", self.counter)
    }

    fn var_name(&self, module_path: &str, name: &str) -> Option<String> {
        if let Some(name) = self.consts.get(&(module_path.to_string(), name.to_string())) {
            Some(name.clone())
        } else if is_builtin_function(name) {
            Some(name.to_string())
        } else {
            None
        }
    }

    fn ns_path(&self, module_path: &str, ns_name: &str) -> Option<String> {
        self.imports.get(&(module_path.to_string(), ns_name.to_string())).cloned()
    }
}

struct LocalEnv<'a> {
    parent: &'a mut dyn Environment,
    locals: HashMap<String, String>,
    module_path: &'a str
}

impl<'a> LocalEnv<'a> {
    fn new_var(&mut self, name: &str, shadow_parent: bool) -> Option<String> {
        let exists = if shadow_parent {
            self.locals.contains_key(name)
        } else {
            self.var_name(self.module_path, name).is_some()
        };
        if !exists {
            let uniq = self.new_unique_id();
            self.locals.insert(name.to_string(), uniq.clone());
            Some(uniq)
        } else {
            None
        }
    }

    fn new_module(parent: &'a mut GlobalEnv, module_path: &'a str, decls: &Vec<parser::Decl>) -> Fallible<LocalEnv<'a>> {
        let mut env = LocalEnv { parent, module_path, locals: HashMap::new() };
        for decl in decls {
            if let parser::Decl::Const { line, name, private: true, .. } = decl {
                if env.new_var(name, true).is_none() {
                    err(*line, format!("Cannot declare {} twice", name))?;
                }
            }
        }
        Ok(env)
    }

    fn new_scope(parent: &'a mut LocalEnv) -> LocalEnv<'a> {
        let module_path = parent.module_path;
        LocalEnv { parent, locals: HashMap::new(), module_path }
    }
}

impl<'a> Environment for LocalEnv<'a> {
    fn new_unique_id(&mut self) -> String {
        self.parent.new_unique_id()
    }

    fn var_name(&self, module_path: &str, name: &str) -> Option<String> {
        if let Some(uniq) = self.locals.get(name) && module_path == self.module_path {
            Some(uniq.to_string())
        } else {
            self.parent.var_name(module_path, name)
        }
    }

    fn ns_path(&self, module_path: &str, ns_name: &str) -> Option<String> {
        self.parent.ns_path(module_path, ns_name)
    }
}

fn nr_type(e: parser::Expr) -> Fallible<Type> {
    use parser::Expr::*;

    match e {
        Var { line, name, .. } => {
            if is_builtin_type(&name) {
                Ok(Type::Scalar { line, name })
            } else {
                err(line, format!("Undefined type {}", name))
            }
        },
        Call { line, func, args } => {
            if let Type::Scalar { name, .. } = nr_type(*func)? {
                let args: Fallible<Vec<_>> = args.into_iter().map(nr_type).collect();
                Ok(Type::Generic { line, template: name, args: args? })
            } else {
                err(line, "Cannot repeat bracket calls in type declaration".to_string())
            }
        }
        Record { line, fields } => {
            let mut new_fields = HashMap::new();
            for (key, value) in fields {
                new_fields.insert(key, nr_type(value)?);
            }
            Ok(Type::Record { line, fields: new_fields })
        }
        _ => err(e.line(), "Not a valid type expression".into())
    }
}

fn nr_expr(e: parser::Expr, module_path: &str, env: &mut LocalEnv) -> Fallible<Value> {
    match e {
        parser::Expr::Int { line, value } => Ok(Value::Int { line, value }),
        parser::Expr::Real { line, value } => Ok(Value::Real { line, value }),
        parser::Expr::Text { line, value } => Ok(Value::Text { line, value }),
        parser::Expr::Bool { line, value } => Ok(Value::Bool { line, value }),
        parser::Expr::Var { line, ns: None, name } => {
            if let Some(name) = env.var_name(module_path, &name) {
                Ok(Value::Var { line, name })
            } else {
                err(line, format!("Undefined identifier '{}'", name))
            }
        }
        parser::Expr::Var { line, ns: Some(ns_name), name } => {
            if let Some(ns_path) = env.ns_path(module_path, &ns_name) {
                if let Some(name) = env.var_name(&ns_path, &name) {
                    Ok(Value::Var { line, name })
                } else {
                    err(line, format!("Undefined identifier '{}'", name))
                }
            } else {
                err(line, format!("Unknown module '{}'", ns_name))
            }
        }
        parser::Expr::BinOp { line, name, lhs, rhs } => {
            let new_lhs = nr_expr(*lhs, module_path, env)?;
            let new_rhs = nr_expr(*rhs, module_path, env)?;
            Ok(Value::BinOp { line, name, lhs: Box::new(new_lhs), rhs: Box::new(new_rhs) })
        }
        parser::Expr::Access { line, object, field } => {
            let new_object = nr_expr(*object, module_path, env)?;
            Ok(Value::Access { line, object: Box::new(new_object), field })
        }
        parser::Expr::Call { line, func, args } => {
            let new_func = nr_expr(*func, module_path, env)?;
            let mut new_args = vec![];
            for arg in args {
                new_args.push(nr_expr(arg, module_path, env)?);
            }
            Ok(Value::Call { line, func: Box::new(new_func), args: new_args })
        }
        parser::Expr::Record { line, fields } => {
            let mut new_fields = HashMap::new();
            for (key, value) in fields {
                new_fields.insert(key, nr_expr(value, module_path, env)?);
            }
            Ok(Value::Record { line, fields: new_fields })
        }
        parser::Expr::Lambda { line, args, return_type, body } => {
            let mut inner = LocalEnv::new_scope(env);
            let args : Fallible<Vec<_>> = args.into_iter().map(|(n, t)| {
                let name = if let Some(name) = inner.new_var(&n, true) {
                    name
                } else {
                    err(line, format!("Duplicate argument name {}", n))?
                };
                Ok((name, nr_type(t)?))
            }).collect();

            let body = nr_exprs(body, module_path, inner)?;
            let return_type = nr_type(*return_type)?;
            Ok(Value::Lambda { line, args: args?, return_type, body })
        }
        parser::Expr::Assign { line, name, expr: value } => {
            let value = nr_expr(*value, module_path, env)?;
            env.new_var(&name, false);
            let name = env.var_name(module_path, &name).unwrap();
            Ok(Value::Assign { line, name, value: Box::new(value) })
        }
        parser::Expr::If { line, cond, then, otherwise } => {
            let cond = nr_expr(*cond, module_path, &mut LocalEnv::new_scope(env))?;
            let then = nr_exprs(then, module_path, LocalEnv::new_scope(env))?;
            let otherwise = nr_exprs(otherwise, module_path, LocalEnv::new_scope(env))?;
            Ok(Value::If { line, cond: Box::new(cond), then, otherwise })
        }
        parser::Expr::While { line, cond, body } => {
            let cond = nr_expr(*cond, module_path, &mut LocalEnv::new_scope(env))?;
            let body = nr_exprs(body, module_path, LocalEnv::new_scope(env))?;
            Ok(Value::While { line, cond: Box::new(cond), body })
        }
        parser::Expr::For { line, key, value, expr, body } => {
            let expr = nr_expr(*expr, module_path, &mut LocalEnv::new_scope(env))?;
            let mut inner = LocalEnv::new_scope(env);
            let key = if let Some(key) = key {
                inner.new_var(&key, true).unwrap()
            } else {
                inner.new_unique_id()
            };
            let value = if let Some(value) = inner.new_var(&value, true) {
                value
            } else {
                err(line, format!("Both variables in the for loop have the name {}", value))?
            };
            let body = nr_exprs(body, module_path, inner)?;
            Ok(Value::For { line, key, value, expr: Box::new(expr), body })
        }
    }
}

fn nr_exprs(exprs: Vec<parser::Expr>, module_path: &str, mut env: LocalEnv) -> Fallible<Vec<Value>> {
    let mut result = vec![];
    for expr in exprs {
        result.push(nr_expr(expr, module_path, &mut env)?);
    }
    Ok(result)
}

pub fn name_resolution(main: String, modules: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, HashMap<String, Value>)> {
    let mut global_env = GlobalEnv::new(&modules)?;
    let mut result = HashMap::new();
    let mut main_name = None;

    for (module_path, decls) in modules {
        let mut module_env = LocalEnv::new_module(&mut global_env, &module_path, &decls)?;
        for decl in decls {
            if let parser::Decl::Const { name, expr, .. } = decl {
                let new_name = module_env.var_name(&module_path, &name).unwrap();
                let new_value = nr_expr(expr, &module_path, &mut module_env)?;

                if module_path == main && name == "main" {
                    main_name = Some(new_name.clone());
                }

                result.insert(new_name, new_value);
            }
        }
    }

    if let Some(main_name) = main_name {
        Ok((main_name, result))
    } else {
        err(0, "No main function defined".into())
    }
}
