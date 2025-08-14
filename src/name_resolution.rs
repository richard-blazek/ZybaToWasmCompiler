use std::collections::HashMap;

use crate::error::{Fallible, err};
use crate::parser;

struct VarCounter(i64);

impl VarCounter {
    fn global(&self, file: &str, name: &str) -> Fallible<String> {
        let file = if let Some(pos) = file.rfind(|c| c == '/' || c == '\\') {
            &file[pos+1..]
        } else {
            file
        };
        let file = if file.ends_with(".zyba") {
            &file[0..file.len()-5]
        } else {
            file
        };
        if file.chars().all(|c| c.is_ascii_alphanumeric()) {
            Ok(format!("_global_{}_{}", file, name))
        } else {
            err(0, format!("File name {} must be alphanumeric", file))
        }
    }

    fn local(&mut self) -> String {
        self.0 += 1;
        format!("_local_{}", self.0)
    }
}

pub fn resolve(main: String, files: HashMap<String, Vec<parser::Decl>>) -> Vec<parser::Decl> {
    todo!();
    files[main.as_str()].clone()
}
