use std::collections::HashMap;

use crate::error::Fallible;
use crate::parser;

struct Locals(i64);

impl Locals {
    fn local(&mut self) -> String {
        self.0 += 1;
        format!("_local{}", self.0)
    }
}

fn mangle_globals(files: &mut HashMap<String, Vec<parser::Decl>>) -> HashMap<(String, String), String> {
    let mut map = HashMap::new();
    let mut i = 0;
    for (file, decls) in files {
        for decl in decls {
            if let parser::Decl::Const { name, .. } = decl {
                let k = (file.clone(), name.clone());
                let v = format!("_global{}_{}", i, name);
                *name = v.clone();
                map.insert(k, v);
            }
        }
        i += 1;
    }
    map
}

pub fn resolve(main: String, mut files: HashMap<String, Vec<parser::Decl>>) -> Fallible<(String, Vec<parser::Decl>)> {
    let globals_map = mangle_globals(&mut files);
    todo!();
}
