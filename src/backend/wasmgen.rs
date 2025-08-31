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

fn gen_text_literal(s: &mut String, text: String) {
    todo!()
}

fn gen_instr(s: &mut String, instr: Instr) {
    match instr {
        Instr::PushInt { value } => fmt!(s, "i64.const {}\n", value),
        Instr::PushReal { value } => fmt!(s, "f64.const {}\n", value),
        Instr::PushText { value } => gen_text_literal(s, value),
        Instr::PushBool { value } => fmt!(s, "i32.const {}\n", value as i32),
        Instr::Drop { .. } => fmt!(s, "drop\n"),
        Instr::Block { id, inner } => {
            fmt!(s, "(block $block_{}\n", id);
            fmt!(s, "(loop $loop_{}\n", id);
            gen_instrs(s, inner);
            fmt!(s, ")\n)\n");
        }
        Instr::RepeatBlock { id } => fmt!(s, "br $loop_{}\n", id),
        Instr::QuitBlock { id } => fmt!(s, "br $block_{}\n", id),
        Instr::CondBlock { id } => fmt!(s, "i32.eqz\nbr_if $block_{}\n", id),
        Instr::NewTuple { fields } => todo!(),
        Instr::GetField { fields, i } => todo!(),
        Instr::SetField { fields, i } => todo!(),
        Instr::GetLocal { id, .. } => fmt!(s, "local.get {}\n", id),
        Instr::SetLocal { id, .. } => fmt!(s, "local.set {}\n", id),
        Instr::CallFunc { args, ret } => todo!(),
        Instr::BindFunc { id, args, ret, capture } => todo!(),
        Instr::RealToInt => fmt!(s, "i64.trunc_f64_s\n"),
        Instr::IntToReal => fmt!(s, "f64.convert_i64_s\n"),
        Instr::IntToTextAscii => todo!(),
        Instr::NotInt => fmt!(s, "i64.const -1\ni64.xor\n"),
        Instr::NotBool => fmt!(s, "i32.eqz\n"),
        Instr::PrintText => todo!(),
        Instr::NewArray { item } => todo!(),
        Instr::LenArray { item } => todo!(),
        Instr::GetArray { item } => todo!(),
        Instr::SetArray { item } => todo!(),
        Instr::GetText => fmt!(s, "i32.wrap_i64\ni32.add\ni64.load8_u\n"),
        Instr::CatText => todo!(),
        Instr::LtText => fmt!(s, "call $text_lt\n"),
        Instr::EqText => fmt!(s, "call $text_eq\n"),
        Instr::LenText => fmt!(s, "call $text_len\n"),
        Instr::MulInt => fmt!(s, "i64.mul\n"),
        Instr::MulReal => fmt!(s, "f64.add\n"),
        Instr::DivInt => fmt!(s, "i64.div_s\n"),
        Instr::DivReal => fmt!(s, "f64.div\n"),
        Instr::RemInt => fmt!(s, "i64.rem_s\n"),
        Instr::AddInt => fmt!(s, "i64.add\n"),
        Instr::AddReal => fmt!(s, "f64.add\n"),
        Instr::SubInt => fmt!(s, "i64.sub\n"),
        Instr::SubReal => fmt!(s, "f64.sub\n"),
        Instr::LtInt => fmt!(s, "i64.lt\n"),
        Instr::EqInt => fmt!(s, "i64.eq\n"),
        Instr::LtReal => fmt!(s, "f64.lt\n"),
        Instr::EqReal => fmt!(s, "f64.eq\n"),
        Instr::AndInt => fmt!(s, "i64.and\n"),
        Instr::AndBool => fmt!(s, "i32.and\n"),
        Instr::OrInt => fmt!(s, "i64.or\n"),
        Instr::OrBool => fmt!(s, "i32.or\n"),
        Instr::XorInt => fmt!(s, "i64.xor\n"),
        Instr::XorBool => fmt!(s, "i32.xor\n"),
        Instr::Abort => fmt!(s, "unreachable\n"),
    }
}

fn gen_instrs(s: &mut String, instrs: Vec<Instr>) {
    for instr in instrs {
        gen_instr(s, instr);
    }
}

fn gen_func(s: &mut String, i: usize, code: Vec<Instr>, args: Vec<Type>, ret: Type) {
    fmt!(s, "(func $func_{} (param", i);
    for arg in &args {
        fmt!(s, " {}", type_to_str(arg));
    }
    fmt!(s, " i32) (result {})\n", type_to_str(&ret));
    gen_instrs(s, code);
    fmt!(s, ")\n");

    fmt!(s, "elem (i32.const {}) $func_{})\n", i, i);
}

fn gen_program(s: &mut String, funcs: Vec<Func>, entry: usize) {
    fmt!(s, "(module
(func $text_len (param $ptr i32) (result i64)
    (local $len i64)
    (local.set $len (i64.const 0))
    (block $exit
        (loop $loop
            (i32.load8_u (local.get $ptr))
            (br_if $exit (i32.eqz (i32.load8_u (local.get $ptr))))
            (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
            (local.set $len (i64.add (local.get $len) (i64.const 1)))
            (br $loop)
        )
    )
    (local.get $len)
)
(func $text_eq (param $a i32) (param $b i32) (result i32)
    (local $charA i32)
    (local $charB i32)

    (block $not_equal
      (loop $loop
        (local.set $charA (i32.load8_u (local.get $a)))
        (local.set $charB (i32.load8_u (local.get $b)))

        (br_if $not_equal (i32.ne (local.get $charA) (local.get $charB)))
        (br_if 1 (i32.eqz (local.get $charA)))

        (local.set $a (i32.add (local.get $a) (i32.const 1)))
        (local.set $b (i32.add (local.get $b) (i32.const 1)))
        (br $loop)
      )
      (i32.const 1)
      return
    )
    (i32.const 0)
)
(func $text_lt (param $a i32) (param $b i32) (result i32)
    (local $charA i32)
    (local $charB i32)

    (block $gt_eq
      (loop $loop
        (local.set $charA (i32.load8_u (local.get $a)))
        (local.set $charB (i32.load8_u (local.get $b)))

        (br_if $gt_eq (i32.ge_u (local.get $charA) (local.get $charB)))
        (br_if 1 (i32.eqz (local.get $charA)))

        (local.set $a (i32.add (local.get $a) (i32.const 1)))
        (local.set $b (i32.add (local.get $b) (i32.const 1)))
        (br $loop)
      )
      (i32.const 1)
      return
    )
    (i32.const 0)
)
\n");

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
