use std::collections::HashMap;

use crate::frontend::error::{Fallible, err};
use crate::frontend::filesystem;
use crate::frontend::parser;

type Importer = (String, i64);

fn read_file(fs: &dyn filesystem::FS, file_path: &str, importer: &Importer) -> Fallible<Vec<parser::Decl>> {
    let content = if let Some(content) = fs.read(file_path) {
        content
    } else {
        let (importer_path, line) = importer;
        err(*line, format!("Cannot read file: {}", file_path)).map_err(|e| e.in_file(importer_path))?
    };
    parser::parse(&content).map_err(|e| e.in_file(file_path))
}

fn load_module(fs: &dyn filesystem::FS, module_path: &str, importer: &Importer) -> Fallible<(Vec<parser::Decl>, Vec<(String, Importer)>)> {
    let mut decls = read_file(fs, module_path, importer)?;
    let mut imports = vec![];
    for decl in &mut decls {
        if let parser::Decl::Import { line, path } = decl {
            let path = if let Some(abs) = fs.to_absolute(path, Some(module_path)) {
                abs
            } else {
                err(*line, format!("Import path is not valid: {}", path)).map_err(|e| e.in_file(module_path))?
            };
            imports.push((path.clone(), (module_path.to_string(), *line)));
            *decl = parser::Decl::Import { line: *line, path };
        }
    }
    Ok((decls, imports))
}

fn load_modules(fs: &dyn filesystem::FS, main_path: &str) -> Fallible<HashMap<String, Vec<parser::Decl>>> {
    let mut modules = HashMap::new();
    let mut remaining = vec![(main_path.to_string(), (main_path.to_string(), 0))];
    while let Some((path, importer)) = remaining.pop() {
        if !modules.contains_key(&path) {
            let (decls, mut imports) = load_module(fs, &path, &importer)?;
            modules.insert(path, decls);
            remaining.append(&mut imports);
        }
    }
    Ok(modules)
}

pub fn load(fs: &dyn filesystem::FS, main_path: &str) -> Fallible<(String, HashMap<String, Vec<parser::Decl>>)> {
    if let Some(main_path) = fs.to_absolute(main_path, None) {
        let files = load_modules(fs, &main_path)?;
        Ok((main_path, files))
    } else {
        err(0, format!("Invalid path: {}", main_path)).map_err(|e| e.in_file(main_path))
    }
}
