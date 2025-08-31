use std::path::Path;
use std::collections::HashMap;

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

pub trait FS {
    fn read(&self, path: &str) -> Option<String>;
    fn to_absolute(&self, path: &str, anchor: Option<&str>) -> Option<String>;
}

struct SystemFS;

impl FS for SystemFS {
    fn read(&self, path: &str) -> Option<String> {
        std::fs::read_to_string(path).ok()
    }

    fn to_absolute(&self, path: &str, anchor: Option<&str>) -> Option<String> {
        let anchor = Path::new(anchor.unwrap_or("."));
        let dir = anchor.parent().unwrap_or(anchor);
        let canonical = dir.join(path).canonicalize().ok()?;

        let result = canonical.to_str()?.replace('\\', "/");
        if is_valid_path(&result) {
            Some(result)
        } else {
            None
        }
    }
}

pub fn system_fs() -> impl FS {
    SystemFS
}

struct PlaygroundFS(HashMap<String, String>);

impl FS for PlaygroundFS {
    fn read(&self, path: &str) -> Option<String> {
        self.0.get(path).cloned()
    }

    fn to_absolute(&self, path: &str, _anchor: Option<&str>) -> Option<String> {
        if is_valid_path(path) { Some(path.to_string()) } else { None }
    }
}

pub fn playground_fs(files: HashMap<String, String>) -> impl FS {
    PlaygroundFS(files)
}
