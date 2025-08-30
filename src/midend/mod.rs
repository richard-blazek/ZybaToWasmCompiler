mod utils;
mod ir;
mod globals;
mod locals;
mod codegen;

pub use ir::{Func, Instr, Program, Type};
pub use codegen::generate as codegen;
