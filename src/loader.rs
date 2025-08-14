use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::error::{Fallible, err};
use crate::parser;

fn to_fallible<T, E: std::fmt::Display>(x: Result<T, E>) -> Fallible<T> {
    match x {
        Ok(value) => Ok(value),
        Err(error) => err(0, error.to_string())
    }
}

pub trait FileSystem {
    fn read(&self, path: &str) -> Fallible<String>;
    fn to_absolute(&self, path: &str, anchor: Option<&str>) -> Fallible<String>;
}

struct SystemFS;

impl FileSystem for SystemFS {
    fn read(&self, path: &str) -> Fallible<String> {
        to_fallible(fs::read_to_string(path))
    }

    fn to_absolute(&self, path: &str, anchor: Option<&str>) -> Fallible<String> {
        if let Some(anchor) = anchor {
            let anchor_path = Path::new(anchor);
            let dir = anchor_path.parent().unwrap_or(anchor_path);
            Ok(dir.join(path).to_string_lossy().to_string())
        } else {
            let new_path = to_fallible(Path::new(path).canonicalize())?;
            Ok(new_path.to_string_lossy().to_string())
        }
    }
}

pub fn system_fs() -> impl FileSystem {
    SystemFS
}

struct PlaygroundFS(HashMap<String, String>);

impl FileSystem for PlaygroundFS {
    fn read(&self, path: &str) -> Fallible<String> {
        if let Some(content) = self.0.get(path) {
            Ok(content.clone())
        } else {
            err(0, format!("File does not exist: {}", path))
        }
    }

    fn to_absolute(&self, path: &str, _anchor: Option<&str>) -> Fallible<String> {
        if let Some(name) = Path::new(path).file_name() {
            Ok(name.to_string_lossy().to_string())
        } else {
            err(0, format!("No valid file name: {}", path))
        }
    }
}

pub fn playground_fs(files: HashMap<String, String>) -> impl FileSystem {
    PlaygroundFS(files)
}

fn load_module<FS: FileSystem>(fs: &FS, path: &str) -> Fallible<(Vec<parser::Decl>, Vec<String>)> {
    let mut decls = vec![];
    let mut imports = vec![];
    for decl in parser::parse(&fs.read(path)?)? {
        if let parser::Decl::Import { line, path: import } = decl {
            let absolute = fs.to_absolute(&import, Some(path))?;
            imports.push(absolute.clone());
            decls.push(parser::Decl::Import { line: line, path: absolute });
        } else {
            decls.push(decl);
        }
    }
    Ok((decls, imports))
}

pub fn load<FS: FileSystem>(fs: &FS, main_path: &str) -> Fallible<(String, HashMap<String, Vec<parser::Decl>>)> {
    let main_path = fs.to_absolute(main_path, None)?;
    let mut files = HashMap::new();
    let mut remaining = vec![main_path.clone()];
    while let Some(path) = remaining.pop() {
        if !files.contains_key(&path) {
            let (decls, mut imports) = load_module(fs, &path)?;
            files.insert(path, decls);
            remaining.append(&mut imports);
        }
    }
    Ok((main_path, files))
}
