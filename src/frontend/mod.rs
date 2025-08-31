mod builtin;
mod error;
mod filesystem;
mod lexer;
mod loader;
mod nameres;
mod parser;
mod typecheck;

use std::collections::HashMap;

pub use filesystem::{FS, system_fs, playground_fs};
pub use error::{Error, Fallible};
pub use builtin::{Type, void};
pub use typecheck::Value;

pub fn compile<FS: filesystem::FS>(fs: &FS, main_path: &str) -> Fallible<(String, HashMap<String, Value>)> {
    let (main_path, files) = loader::load(fs, main_path)?;
    let (main_fn, decls) = nameres::name_resolution(main_path, files)?;
    let values = typecheck::check(decls)?;
    Ok((main_fn, values))
}
