use std::collections::HashMap;

use crate::frontend::error::{Fallible, err};
use crate::frontend::filesystem;
use crate::frontend::parser;

fn read_file<FS: filesystem::FS>(fs: &FS, file_path: &str) -> Fallible<Vec<parser::Decl>> {
    let content = if let Some(content) = fs.read(file_path) {
        content
    } else {
        err(0, format!("Cannot read file: {}", file_path))?
    };
    parser::parse(&content)
}

fn load_module<FS: filesystem::FS>(fs: &FS, module_path: &str) -> Fallible<(Vec<parser::Decl>, Vec<String>)> {
    let mut decls = read_file(fs, module_path)?;
    let mut imports = vec![];
    for decl in &mut decls {
        if let parser::Decl::Import { line, path } = decl {
            let path = if let Some(abs) = fs.to_absolute(path, Some(module_path)) {
                abs
            } else {
                err(*line, format!("Import path is not valid: {}", path))?
            };
            imports.push(path.clone());
            *decl = parser::Decl::Import { line: *line, path };
        }
    }
    Ok((decls, imports))
}

fn load_modules<FS: filesystem::FS>(fs: &FS, main_path: &str) -> Fallible<HashMap<String, Vec<parser::Decl>>> {
    let mut modules = HashMap::new();
    let mut remaining = vec![main_path.to_string()];
    while let Some(path) = remaining.pop() {
        if !modules.contains_key(&path) {
            let (decls, mut imports) = load_module(fs, &path)?;
            modules.insert(path, decls);
            remaining.append(&mut imports);
        }
    }
    Ok(modules)
}

pub fn load<FS: filesystem::FS>(fs: &FS, main_path: &str) -> Fallible<(String, HashMap<String, Vec<parser::Decl>>)> {
    if let Some(main_path) = fs.to_absolute(main_path, None) {
        let files = load_modules(fs, &main_path)?;
        Ok((main_path, files))
    } else {
        err(0, format!("Invalid path: {}", main_path))
    }
}
