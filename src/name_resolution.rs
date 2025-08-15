use std::collections::HashMap;

use crate::parser;

struct VarCounter {
    modules: HashMap<String, i64>,
    locals: i64
}

impl VarCounter {
    fn global(&mut self, file: &str, name: &str) -> String {
        if let Some(id) = self.modules.get(file) {
            format!("_global{}_{}", id, name)
        } else {
            let id = self.modules.len() as i64;
            self.modules.insert(file.to_string(), id);
            format!("_global{}_{}", id, name)
        }
    }

    fn local(&mut self) -> String {
        self.locals += 1;
        format!("_local{}", self.locals)
    }
}

pub fn resolve(main: String, files: HashMap<String, Vec<parser::Decl>>) -> Vec<parser::Decl> {
    todo!();
    files[main.as_str()].clone()
}
