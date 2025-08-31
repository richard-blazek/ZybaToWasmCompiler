use crate::midend::{Func, Instr, Program, Type};

macro_rules! fmt {
    ($s:tt, $($arg:tt)*) => {
        ($s).push_str(&format!($($arg)*))
    }
}

fn gen_func(s: &mut String, name: String, code: Vec<Instr>, args: Vec<Type>, ret: Type) {

}

fn gen_program(s: &mut String, funcs: Vec<Func>, entry: usize) {

}

pub fn generate(program: Program) -> String {
    let mut s = String::new();
    gen_program(&mut s, program.funcs, program.entry);
    s
}
