use std::collections::HashMap;

use wasm_bindgen::prelude::wasm_bindgen;

mod frontend;
mod midend;
mod backend;

#[wasm_bindgen]
pub fn compile(code: &str) -> String {
    let mut files = HashMap::new();
    files.insert("main.zyba".to_string(), code.to_string());

    let fs = frontend::playground_fs(files);

    let (main_fn, decls) = match frontend::compile(&fs, "main.zyba") {
        Ok((main_path, files)) => (main_path, files),
        Err(error) => return format!("Error: {:?}", error)
    };
    backend::to_wasm(midend::codegen(&main_fn, decls))
}
