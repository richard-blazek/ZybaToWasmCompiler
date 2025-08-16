use std::collections::HashMap;

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
    fn new(files: &HashMap<String, Vec<parser::Decl>>) -> GlobalEnv {
        let mut consts = HashMap::new();
        let mut imports = HashMap::new();
        let mut i = 0;
        for (module_path, decls) in files {
            for decl in decls {
                if let parser::Decl::Const { name, private: false, .. } = decl {
                    let uniq = format!("_global{}_{}", i, name);
                    consts.insert((module_path.clone(), name.clone()), uniq);
                } else if let parser::Decl::Import { path, .. } = decl {
                    let ns = path.split('/').last().unwrap().replace(".zyba", "");
                    imports.insert((module_path.clone(), ns), path.clone());
                }
            }
            i += 1;
        }
        GlobalEnv { consts, imports, counter: 0 }
    }
}

impl Environment for GlobalEnv {
    fn inc_counter(&mut self) -> i64 {
        self.counter += 1;
        self.counter
    }

    fn var_name(&self, module_path: &str, name: &str) -> Option<String> {
        self.consts.get(&(module_path.to_string(), name.to_string())).cloned()
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
    fn insert(&mut self, name: String) -> Option<String> {
        if self.locals.contains_key(&name) {
            None
        } else {
            let uniq = format!("_local{}", self.inc_counter());
            self.locals.insert(name, uniq.clone());
            Some(uniq)
        }
    }

    fn new_module(parent: &'a mut GlobalEnv, module_path: &'a str, decls: &Vec<parser::Decl>) -> LocalEnv<'a> {
        let mut env = LocalEnv { parent, module_path, locals: HashMap::new() };
        for decl in decls {
            if let parser::Decl::Const { name, private: true, .. } = decl {
                env.insert(name.clone());
            }
        }
        env
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

fn resolve_value<'a>(line: i64, val: parser::Value, module_path: &str, env: &'a mut LocalEnv) -> Fallible<parser::Value> {
    match &val {
        parser::Value::Int { .. } => Ok(val),
        parser::Value::Real { .. } => Ok(val),
        parser::Value::Text { .. } => Ok(val),
        parser::Value::Bool { .. } => Ok(val),
        parser::Value::Var { line, ns: None, name } => {
            if let Some(new_name) = env.var_name(module_path, name) {
                Ok(parser::Value::Var { line: *line, ns: None, name: new_name })
            } else {
                err(*line, format!("Undefined identifier '{}'", name))
            }
        }
        parser::Value::Var { line, ns: Some(ns_name), name } => {
            if let Some(ns_path) = env.ns_path(module_path, ns_name) {
                if let Some(new_name) = env.var_name(&ns_path, &name) {
                    Ok(parser::Value::Var { line: *line, ns: None, name: new_name })
                } else {
                    err(*line, format!("Undefined identifier '{}'", name))
                }
            } else {
                err(*line, format!("Unknown module '{}'", ns_name))
            }
        }
        parser::Value::Record { line, fields } => todo!(),
        parser::Value::Call { line, func, args } => todo!(),
        parser::Value::BinOp { line, name, lhs, rhs } => todo!(),
        parser::Value::Access { line, object, field } => todo!(),
        parser::Value::Lambda { line, args, return_type, body } => todo!(),
    }
}

pub fn resolve(main: String, modules: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, HashMap<String, parser::Value>)> {
    let mut global_env = GlobalEnv::new(&modules);
    let mut result = HashMap::new();
    let mut main_name = None;

    for (module_path, decls) in modules {
        let mut module_env = LocalEnv::new_module(&mut global_env, &module_path, &decls);
        for decl in decls {
            if let parser::Decl::Const { line, name, value, .. } = decl {
                let new_name = module_env.var_name(&module_path, &name).unwrap();
                let new_value = resolve_value(line, value, &module_path, &mut module_env)?;

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
