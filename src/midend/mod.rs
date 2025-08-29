mod utils;
mod ir;
mod globals;
mod locals;
mod codegen;

pub use ir::{Cmp, Code, Instr, Program, Type};
pub use codegen::generate as codegen;
