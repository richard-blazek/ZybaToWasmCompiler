use std::collections::HashMap;

use crate::builtin::{is_builtin_type, is_builtin_function};
use crate::error::{err, Fallible};
use crate::parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Scalar { line: i64, name: String },
    Generic { line: i64, template: Box<Type>, args: Vec<Type> },
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
    Lambda { line: i64, args: Vec<(String, Type)>, return_type: Type, body: Vec<Statement> }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Value { line: i64, value: Value },
    Assign { line: i64, name: String, value: Value },
    If { line: i64, cond: Value, then: Vec<Statement>, otherwise: Vec<Statement> },
    While { line: i64, cond: Value, body: Vec<Statement> },
    For { line: i64, key: String, value: String, expr: Value, body: Vec<Statement> },
    Return { line: i64, value: Value }
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

fn nr_block(stms: Vec<parser::Statement>, module_path: &str, mut env: LocalEnv) -> Fallible<Vec<Statement>> {
    let mut result = vec![];

    for stm in stms {
        match stm {
            parser::Statement::Value { line, value } => {
                result.push(Statement::Value { line, value: nr_value(value, module_path, &mut env)? });
            }
            parser::Statement::Assign { line, name, value } => {
                let value = nr_value(value, module_path, &mut env)?;
                env.new_var(&name, false);
                let name = env.var_name(module_path, &name).unwrap();
                result.push(Statement::Assign { line, name, value })
            }
            parser::Statement::If { line, cond, then, otherwise } => {
                let cond = nr_value(cond, module_path, &mut env)?;
                let then = nr_block(then, module_path, LocalEnv::new_scope(&mut env))?;
                let otherwise = nr_block(otherwise, module_path, LocalEnv::new_scope(&mut env))?;
                result.push(Statement::If { line, cond, then, otherwise });
            }
            parser::Statement::While { line, cond, body } => {
                let cond = nr_value(cond, module_path, &mut env)?;
                let body = nr_block(body, module_path, LocalEnv::new_scope(&mut env))?;
                result.push(Statement::While { line, cond, body });
            }
            parser::Statement::For { line, key, value, expr, body } => {
                let expr = nr_value(expr, module_path, &mut env)?;
                let mut inner = LocalEnv::new_scope(&mut env);
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
                let body = nr_block(body, module_path, inner)?;
                result.push(Statement::For { line, key, value, expr, body });
            }
            parser::Statement::Return { line, value } => {
                let value = nr_value(value, module_path, &mut env)?;
                result.push(Statement::Return { line, value });
            }
        }
    }
    Ok(result)
}

fn nr_type(val: parser::Value) -> Fallible<Type> {
    use parser::Value::*;

    match val {
        Var { line, name, .. } => {
            if is_builtin_type(&name) {
                Ok(Type::Scalar { line, name })
            } else {
                err(line, format!("Undefined type {}", name))
            }
        },
        Call { line, func, args } => {
            let template = nr_type(*func)?;
            let args: Fallible<Vec<_>> = args.into_iter().map(nr_type).collect();
            Ok(Type::Generic { line, template: Box::new(template), args: args? })
        }
        Record { line, fields } => {
            let mut new_fields = HashMap::new();
            for (key, value) in fields {
                new_fields.insert(key, nr_type(value)?);
            }
            Ok(Type::Record { line, fields: new_fields })
        }
        Int { line, value } => err(line, format!("Expected a type, but got an integer {}", value)),
        Real { line, value } => err(line, format!("Expected a type, but got a number {}", value)),
        Text { line, value } => err(line, format!("Expected a type, but got a text {}", value)),
        Bool { line, value } => err(line, format!("Expected a type, but got a boolean {}", value)),
        BinOp { line, name, .. } => err(line, format!("Invalid operation {}, expected a type", name)),
        Lambda { line, .. } => err(line, "Expected a type, but got a function".into()),
        Access { line, field, .. } => err(line, format!("Invalid operation for a type: .{}", field)),
    }
}

fn nr_value(val: parser::Value, module_path: &str, env: &mut LocalEnv) -> Fallible<Value> {
    match val {
        parser::Value::Int { line, value } => Ok(Value::Int { line, value }),
        parser::Value::Real { line, value } => Ok(Value::Real { line, value }),
        parser::Value::Text { line, value } => Ok(Value::Text { line, value }),
        parser::Value::Bool { line, value } => Ok(Value::Bool { line, value }),
        parser::Value::Var { line, ns: None, name } => {
            if let Some(name) = env.var_name(module_path, &name) {
                Ok(Value::Var { line, name })
            } else {
                err(line, format!("Undefined identifier '{}'", name))
            }
        }
        parser::Value::Var { line, ns: Some(ns_name), name } => {
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
        parser::Value::BinOp { line, name, lhs, rhs } => {
            let new_lhs = nr_value(*lhs, module_path, env)?;
            let new_rhs = nr_value(*rhs, module_path, env)?;
            Ok(Value::BinOp { line, name, lhs: Box::new(new_lhs), rhs: Box::new(new_rhs) })
        }
        parser::Value::Access { line, object, field } => {
            let new_object = nr_value(*object, module_path, env)?;
            Ok(Value::Access { line, object: Box::new(new_object), field })
        }
        parser::Value::Call { line, func, args } => {
            let new_func = nr_value(*func, module_path, env)?;
            let mut new_args = vec![];
            for arg in args {
                new_args.push(nr_value(arg, module_path, env)?);
            }
            Ok(Value::Call { line, func: Box::new(new_func), args: new_args })
        }
        parser::Value::Record { line, fields } => {
            let mut new_fields = HashMap::new();
            for (key, value) in fields {
                new_fields.insert(key, nr_value(value, module_path, env)?);
            }
            Ok(Value::Record { line, fields: new_fields })
        }
        parser::Value::Lambda { line, args, return_type, body } => {
            let mut inner = LocalEnv::new_scope(env);
            let args : Fallible<Vec<_>> = args.into_iter().map(|(n, t)| {
                let name = if let Some(name) = inner.new_var(&n, true) {
                    name
                } else {
                    err(line, format!("Duplicate argument name {}", n))?
                };
                Ok((name, nr_type(t)?))
            }).collect();

            let body = nr_block(body, module_path, inner)?;
            let return_type = nr_type(*return_type)?;
            Ok(Value::Lambda { line, args: args?, return_type, body })
        }
    }
}

pub fn name_resolution(main: String, modules: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, HashMap<String, Value>)> {
    let mut global_env = GlobalEnv::new(&modules)?;
    let mut result = HashMap::new();
    let mut main_name = None;

    for (module_path, decls) in modules {
        let mut module_env = LocalEnv::new_module(&mut global_env, &module_path, &decls)?;
        for decl in decls {
            if let parser::Decl::Const { name, value, .. } = decl {
                let new_name = module_env.var_name(&module_path, &name).unwrap();
                let new_value = nr_value(value, &module_path, &mut module_env)?;

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
