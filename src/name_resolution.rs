use std::collections::HashMap;

use crate::error::Fallible;
use crate::parser;

trait Environment {
    fn inc_counter(&mut self) -> i64;
    fn var_name(&self, module_path: String, name: String) -> Option<String>;
    fn ns_path(&self, current_path: String, ns_name: String) -> Option<String>;
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

    fn var_name(&self, module_path: String, name: String) -> Option<String> {
        self.consts.get(&(module_path, name)).cloned()
    }

    fn ns_path(&self, current_path: String, ns_name: String) -> Option<String> {
        self.imports.get(&(current_path, ns_name)).cloned()
    }
}

struct LocalEnv {
    parent: Box<dyn Environment>,
    locals: HashMap<String, String>,
    module_path: String
}

impl LocalEnv {
    fn insert(&mut self, name: String) -> Option<String> {
        if self.locals.contains_key(&name) {
            None
        } else {
            let uniq = format!("_local{}", self.inc_counter());
            self.locals.insert(name, uniq.clone());
            Some(uniq)
        }
    }

    fn new_module(parent: GlobalEnv, module_path: String, decls: &Vec<parser::Decl>) -> LocalEnv {
        let mut env = LocalEnv { parent: Box::new(parent), locals: HashMap::new(), module_path };

        for decl in decls {
            if let parser::Decl::Const { name, private: true, .. } = decl {
                env.insert(name.clone());
            }
        }

        env
    }

    fn new_scope(parent: LocalEnv) -> LocalEnv {
        let module_path = parent.module_path.clone();
        LocalEnv { parent: Box::new(parent), locals: HashMap::new(), module_path }
    }
}

impl Environment for LocalEnv {
    fn inc_counter(&mut self) -> i64 {
        self.parent.inc_counter()
    }

    fn var_name(&self, module_path: String, name: String) -> Option<String> {
        if let Some(uniq) = self.locals.get(&name) && module_path == self.module_path {
            Some(uniq.to_string())
        } else {
            self.parent.var_name(module_path, name)
        }
    }

    fn ns_path(&self, current_path: String, ns_name: String) -> Option<String> {
        self.parent.ns_path(current_path, ns_name)
    }
}

struct Locals(i64);

impl Locals {
    fn new(&mut self) -> String {
        self.0 += 1;
        format!("_local{}", self.0)
    }
}

pub fn resolve(main: String, files: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, HashMap<String, parser::Value>)> {
    let global_env = GlobalEnv::new(&files);
    todo!();
}
