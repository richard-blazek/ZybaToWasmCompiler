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

fn is_valid_path(path: &str) -> bool {
    if let Some(name) = path.split('/').last() && name.is_ascii() {
        if let Some((stem, "zyba")) = name.split_once('.') {
            !stem.is_empty()
            && stem.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
            && stem.chars().nth(0).unwrap().is_ascii_alphabetic()
        } else {
            false
        }
    } else {
        false
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
        let anchor = Path::new(anchor.unwrap_or("."));
        let dir = anchor.parent().unwrap_or(anchor);
        let cpath = to_fallible(dir.join(path).canonicalize())?;

        let result = cpath.to_string_lossy().replace('\\', "/");
        if is_valid_path(&result) {
            Ok(result)
        } else {
            err(0, format!("File name '{}' is not valid; expected format like: dir1/dir2/file.zyba", result))
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
        if is_valid_path(path) {
            Ok(path.to_string())
        } else {
            err(0, format!("Invalid file name: {}", path))
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
