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
    let mut bytes = text.into_bytes();
    bytes.resize(bytes.len() + (4 - bytes.len() % 4), 0);

    fmt!(s, "(global.set $handy1 (call $malloc (i32.const {})))", bytes.len());
    for (i, byte) in bytes.iter().enumerate() {
        fmt!(s, "(i32.store8 (i32.const {}) (i32.add (global.get $handy1) (i32.const {})))", byte, i);
    }
    fmt!(s, "(global.get $handy1)");
}

fn gen_instr(s: &mut String, instr: Instr) {
    match instr {
        Instr::PushInt { value } => fmt!(s, "(i64.const {})", value),
        Instr::PushReal { value } => fmt!(s, "(f64.const {})", value),
        Instr::PushText { value } => gen_text_literal(s, value),
        Instr::PushBool { value } => fmt!(s, "(i32.const {})", value as i32),
        Instr::Drop { .. } => fmt!(s, "(drop)"),
        Instr::Block { id, inner } => {
            fmt!(s, "(block $block_{} ", id);
            fmt!(s, "(loop $loop_{} ", id);
            gen_instrs(s, inner);
            fmt!(s, "))");
        }
        Instr::RepeatBlock { id } => fmt!(s, "(br $loop_{})", id),
        Instr::QuitBlock { id } => fmt!(s, "(br $block_{})", id),
        Instr::CondBlock { id } => fmt!(s, "(br_if $block_{} (i32.eqz))", id),
        Instr::NewTuple { fields } => {
            fmt!(s, "(global.set $handy1 (call $malloc (i32.const {})))", fields.len() * 8);
            for i in (0..fields.len()).rev() {
                fmt!(s, "(global.get $handy1)");
                fmt!(s, "(i64.store offset={})", i * 8);
            }
            fmt!(s, "(global.get $handy1)");
        }
        Instr::GetField { fields, i } => {
            fmt!(s, "(i64.load offset={})", i * 8);
        }
        Instr::SetField { fields, i } => {
            fmt!(s, "(i64.store offset={})", i * 8);
        }
        Instr::GetLocal { id, .. } => fmt!(s, "(local.get {})", id),
        Instr::SetLocal { id, .. } => fmt!(s, "(local.set {})", id),
        Instr::CallFunc { args, ret } => {
            todo!()
        }
        Instr::BindFunc { id, args, ret, capture } => {
            todo!()
        }
        Instr::RealToInt => fmt!(s, "(i64.trunc_f64_s)"),
        Instr::IntToReal => fmt!(s, "(f64.convert_i64_s)"),
        Instr::IntToTextAscii => {
            todo!()
        }
        Instr::NotInt => fmt!(s, "(i64.xor (i64.const -1))"),
        Instr::NotBool => fmt!(s, "(i32.eqz)"),
        Instr::PrintText => {
            todo!()
        }
        Instr::NewArray { item } => {
            fmt!(s, "(global.set $handy1 (i32.wrap_i64))");
            fmt!(s, "(global.set $handy2 (call $malloc (i32.add (i32.mul (global.get $handy1) (i32.const 8)) (i32.const 1))))");
            fmt!(s, "(i64.store (i64.extend_i32_s (global.get $handy1)) (global.get $handy2))");
        }
        Instr::LenArray { item } => {
            fmt!(s, "(i64.store)");
        }
        Instr::GetArray { item } => {
            fmt!(s, "(i32.add (i32.mul (i32.wrap_i64) (i32.const 8)) (i32.const 1))");
            fmt!(s, "(i32.add)");
            fmt!(s, "({}.load)", type_to_str(&item));
        }
        Instr::SetArray { item } => {
            fmt!(s, "(i32.add (i32.mul (i32.wrap_i64) (i32.const 8)) (i32.const 1))");
            fmt!(s, "(i32.add)");
            fmt!(s, "({}.store)", type_to_str(&item));
        }
        Instr::GetText => {
            fmt!(s, "(i32.wrap_i64)");
            fmt!(s, "(i32.add)");
            fmt!(s, "(i64.load8_u)");
        }
        Instr::CatText => fmt!(s, "(call $strcat)"),
        Instr::LtText => fmt!(s, "(call $strcmp_lt)"),
        Instr::EqText => fmt!(s, "(call $strcmp_eq)"),
        Instr::LenText => fmt!(s, "(call $strlen)"),
        Instr::MulInt => fmt!(s, "(i64.mul)"),
        Instr::MulReal => fmt!(s, "(f64.add)"),
        Instr::DivInt => fmt!(s, "(i64.div_s)"),
        Instr::DivReal => fmt!(s, "(f64.div)"),
        Instr::RemInt => fmt!(s, "(i64.rem_s)"),
        Instr::AddInt => fmt!(s, "(i64.add)"),
        Instr::AddReal => fmt!(s, "(f64.add)"),
        Instr::SubInt => fmt!(s, "(i64.sub)"),
        Instr::SubReal => fmt!(s, "(f64.sub)"),
        Instr::LtInt => fmt!(s, "(i64.lt)"),
        Instr::EqInt => fmt!(s, "(i64.eq)"),
        Instr::LtReal => fmt!(s, "(f64.lt)"),
        Instr::EqReal => fmt!(s, "(f64.eq)"),
        Instr::AndInt => fmt!(s, "(i64.and)"),
        Instr::AndBool => fmt!(s, "(i32.and)"),
        Instr::OrInt => fmt!(s, "(i64.or)"),
        Instr::OrBool => fmt!(s, "(i32.or)"),
        Instr::XorInt => fmt!(s, "(i64.xor)"),
        Instr::XorBool => fmt!(s, "(i32.xor)"),
        Instr::Abort => fmt!(s, "(unreachable)"),
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
    fmt!(s, " i32) (result {})", type_to_str(&ret));
    gen_instrs(s, code);
    fmt!(s, ")");

    fmt!(s, "elem (i32.const {}) $func_{})", i, i);
}

fn gen_program(s: &mut String, funcs: Vec<Func>, entry: usize) {
    fmt!(s, "(module
(func $strlen (param $ptr i32) (result i64)
  (local $len i64)
  (local.set $len (i64.const 0))
  (block $exit
    (loop $loop
      (i32.load8_u (local.get $ptr))
        (br_if $exit (i32.eqz (i32.load8_u (local.get $ptr))))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
        (local.set $len (i64.add (local.get $len) (i64.const 1)))
        (br $loop)))
  (local.get $len))
(func $strcmp_eq (param $a i32) (param $b i32) (result i32)
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
      (br $loop))
    (i32.const 1)
    return)
  (i32.const 0))
(func $strcmp_lt (param $a i32) (param $b i32) (result i32)
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
      (br $loop))
    (i32.const 1)
    return)
  (i32.const 0))

(memory $mem 1)
(global $heap_ptr (mut i32) (i32.const 0))
(func $malloc (param $sz i64) (result i32)
    (local $size i32)
    (local $old_base i32)
    (local $new_end i32)
    (local $need_pages i32)

    (if (i64.eqz (local.get $sz))
      (then
        (local.set $size (i32.const 1)))
      (else
        (local.set $size (i32.wrap_i64 (local.get $sz)))))

    ;; Align up to 8 bytes: (size + 7) & ~7
    (local.set $size (i32.and (i32.add (local.get $size) (i32.const 7)) (i32.const -8)))

    (if (i32.eqz (global.get $heap_ptr))
      (then
        ;; heap_base = memory.size << 16  (pages -> bytes)
        (global.set $heap_ptr (i32.shl (memory.size) (i32.const 16)))))

    (local.set $old_base (globals.get $heap_ptr))
    (local.set $new_end (i32.add (local.get $old_base) (local.get $size)))
    (local.set $need (i32.shr_u (i32.add (local.get $new_end) (i32.const 65535)) (i32.const 16)))
    (local.set $need (i32.sub (local.get $need) (memory.size)))

    (if (i32.gt_s (local.get $need) (i32.const 0))
      (then
        ;; memory.grow returns -1 on failure
        (if (i32.eq (memory.grow (local.get $need)) (i32.const -1))
          (then
            (unreachable)))))

    (global.set $heap_ptr (local.get $new_end))
    (local.get $old_base))

  (func $strcat (param $a i32) (param $b i32) (result i32)
    (local $lenA i32)
    (local $lenB i32)
    (local $total i32)
    (local $dst i32)
    (local $i i32)

    (local.set $lenA (call $strlen (local.get $a)))
    (local.set $lenB (call $strlen (local.get $b)))
    (local.set $total (i32.add (i32.add (local.get $lenA) (local.get $lenB)) (i32.const 1)))
    (local.set $dst (call $malloc (local.get $total)))

    (local.set $i (i32.const 0))
    (block $copy_a_done
      (loop $copy_a
        (br_if $copy_a_done (i32.ge_u (local.get $i) (local.get $lenA)))
        (i32.store8
          (i32.add (local.get $dst) (local.get $i))
          (i32.load8_u (i32.add (local.get $a) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy_a)))

    (local.set $i (i32.const 0))
    (block $copy_b_done
      (loop $copy_b
        (br_if $copy_b_done (i32.ge_u (local.get $i) (local.get $lenB)))
        (i32.store8
          (i32.add (local.get $dst) (i32.add (local.get $lenA) (local.get $i)))
          (i32.load8_u (i32.add (local.get $b) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy_b)))

    (i32.store8
      (i32.add (local.get $dst) (i32.add (local.get $lenA) (local.get $lenB)))
      (i32.const 0))
    (local.get $dst))

(global $handy1 (mut i32) (i32.const 0))
(global $handy2 (mut i32) (i32.const 0))
");

    fmt!(s, "(table $table {} funcref)", funcs.len());

    for (i, func) in funcs.into_iter().enumerate() {
        gen_func(s, i, func.code, func.args, func.ret);
    }

    fmt!(s, "(start $func_{})", entry);
    fmt!(s, ")");
}

pub fn generate(program: Program) -> String {
    let mut s = String::new();
    gen_program(&mut s, program.funcs, program.entry);
    s
}
