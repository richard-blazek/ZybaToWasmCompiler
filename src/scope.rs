use std::collections::HashMap;

use crate::builtin::is_builtin_global;
use crate::error::{err, Fallible};
use crate::parser;

trait Environment {
    fn inc_counter(&mut self) -> i64;
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
    fn inc_counter(&mut self) -> i64 {
        self.counter += 1;
        self.counter
    }

    fn var_name(&self, module_path: &str, name: &str) -> Option<String> {
        if let Some(name) = self.consts.get(&(module_path.to_string(), name.to_string())) {
            Some(name.clone())
        } else if is_builtin_global(name) {
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
            let uniq = format!("_v{}", self.inc_counter());
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
    fn inc_counter(&mut self) -> i64 {
        self.parent.inc_counter()
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

fn nr_block<'a>(stms: Vec<parser::Statement>, module_path: &str, mut env: LocalEnv) -> Fallible<Vec<parser::Statement>> {
    use parser::Statement::*;

    let mut result = vec![];

    for stm in stms {
        match stm {
            Value { line, value } => {
                result.push(Value { line, value: nr_value(value, module_path, &mut env)? });
            }
            Assignment { line, name, value } => {
                let value = nr_value(value, module_path, &mut env)?;
                env.new_var(&name, false);
                let name = env.var_name(module_path, &name).unwrap();
                result.push(Assignment { line, name, value })
            }
            If { line, cond, then, otherwise } => {
                let cond = nr_value(cond, module_path, &mut env)?;
                let then = nr_block(then, module_path, LocalEnv::new_scope(&mut env))?;
                let otherwise = nr_block(otherwise, module_path, LocalEnv::new_scope(&mut env))?;
                result.push(If { line, cond, then, otherwise });
            }
            While { line, cond, body } => {
                let cond = nr_value(cond, module_path, &mut env)?;
                let body = nr_block(body, module_path, LocalEnv::new_scope(&mut env))?;
                result.push(While { line, cond, body });
            }
            For { line, key, value, expr, body } => {
                let expr = nr_value(expr, module_path, &mut env)?;
                let mut inner = LocalEnv::new_scope(&mut env);
                let key = key.map(|key| inner.new_var(&key, true).unwrap());
                let value = if let Some(value) = inner.new_var(&value, true) {
                    value
                } else {
                    err(line, format!("Both variables in the for loop have the name {}", value))?
                };
                let body = nr_block(body, module_path, inner)?;
                result.push(For { line, key, value, expr, body });
            }
            Return { line, value } => {
                let value = nr_value(value, module_path, &mut env)?;
                result.push(Return { line, value });
            }
        }
    }

    Ok(result)
}

fn nr_value<'a>(val: parser::Value, module_path: &str, env: &'a mut LocalEnv) -> Fallible<parser::Value> {
    use parser::Value::*;

    match val {
        Int { .. } | Real { .. } | Text { .. } | Bool { .. } => Ok(val),
        Var { line, ns: None, name } => {
            if let Some(name) = env.var_name(module_path, &name) {
                Ok(Var { line, ns: None, name })
            } else {
                err(line, format!("Undefined identifier '{}'", name))
            }
        }
        Var { line, ns: Some(ns_name), name } => {
            if let Some(ns_path) = env.ns_path(module_path, &ns_name) {
                if let Some(name) = env.var_name(&ns_path, &name) {
                    Ok(Var { line, ns: None, name })
                } else {
                    err(line, format!("Undefined identifier '{}'", name))
                }
            } else {
                err(line, format!("Unknown module '{}'", ns_name))
            }
        }
        BinOp { line, name, lhs, rhs } => {
            let new_lhs = nr_value(*lhs, module_path, env)?;
            let new_rhs = nr_value(*rhs, module_path, env)?;
            Ok(BinOp { line, name, lhs: Box::new(new_lhs), rhs: Box::new(new_rhs) })
        }
        Access { line, object, field } => {
            let new_object = nr_value(*object, module_path, env)?;
            Ok(Access { line, object: Box::new(new_object), field })
        }
        Call { line, func, args } => {
            let new_func = nr_value(*func, module_path, env)?;
            let mut new_args = vec![];
            for arg in args {
                new_args.push(nr_value(arg, module_path, env)?);
            }
            Ok(Call { line, func: Box::new(new_func), args: new_args })
        }
        Record { line, fields } => {
            let mut new_fields = HashMap::new();
            for (key, value) in fields {
                new_fields.insert(key, nr_value(value, module_path, env)?);
            }
            Ok(Record { line, fields: new_fields })
        }
        Lambda { line, args, return_type, body } => {
            let mut inner = LocalEnv::new_scope(env);
            let mut new_args = vec![];
            for (name, type_) in args {
                if let Some(name) = inner.new_var(&name, true) {
                    new_args.push((name, type_));
                } else {
                    err(line, format!("Duplicate argument name {}", name))?;
                }
            }

            let body = nr_block(body, module_path, inner)?;
            let new_return_type = nr_value(*return_type, module_path, env)?;
            Ok(Lambda { line, args: new_args, return_type: Box::new(new_return_type), body })
        }
    }
}

pub fn name_resolution(main: String, modules: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, HashMap<String, parser::Value>)> {
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
