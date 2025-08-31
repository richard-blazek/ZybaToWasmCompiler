use crate::midend::{Func, Instr, Program, Type};

macro_rules! fmt {
    ($s:tt, $($arg:tt)*) => {
        ($s).push_str(&format!($($arg)*))
    }
}

fn type_to_str(tpe: &Type) -> &str {
    match tpe {
        Type::Int => "i64",
        Type::Real => "f64",
        _ => "i32",
    }
}

fn gen_instr(s: &mut String, instr: Instr) {

}

fn gen_func(s: &mut String, i: usize, code: Vec<Instr>, args: Vec<Type>, ret: Type) {
    fmt!(s, "(func $func_{} (param", i);
    for arg in &args {
        fmt!(s, " {}", type_to_str(arg));
    }
    fmt!(s, " i32) (result {})\n", type_to_str(&ret));

    for instr in code {
        gen_instr(s, instr);
    }
    fmt!(s, ")\n");

    fmt!(s, "elem (i32.const {}) $func_{})\n", i, i);
}

fn gen_program(s: &mut String, funcs: Vec<Func>, entry: usize) {
    fmt!(s, "(module\n");
    fmt!(s, "(table $table {} funcref)\n", funcs.len());

    for (i, func) in funcs.into_iter().enumerate() {
        gen_func(s, i, func.code, func.args, func.ret);
    }

    fmt!(s, "(start $func_{})\n", entry);
    fmt!(s, ")\n");
}

pub fn generate(program: Program) -> String {
    let mut s = String::new();
    gen_program(&mut s, program.funcs, program.entry);
    s
}
