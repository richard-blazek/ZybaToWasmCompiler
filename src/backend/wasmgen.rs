use std::collections::HashMap;

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

struct FnTypes {
    map: HashMap<(Vec<String>, String), String>
}

impl FnTypes {
    fn type_of(&mut self, args: &Vec<Type>, ret: &Type) -> String {
        let args: Vec<_> = args.iter().map(|arg| type_to_str(arg).to_string()).collect();
        let ret = type_to_str(ret).to_string();

        if let Some(typename) = self.map.get(&(args.clone(), ret.clone())) {
            typename.clone()
        } else {
            let idx = format!("fn_type_{}", self.map.len());
            self.map.insert((args, ret), idx.clone());
            idx
        }
    }

    fn gen_signatures(&self, s: &mut String) {
        for ((args, ret), typename) in self.map.iter() {
            fmt!(s, "(type ${} (func ", typename);
            if !args.is_empty() {
                fmt!(s, "(param");
                for arg in args {
                    fmt!(s, " {}", arg);
                }
                fmt!(s, ") ");
            }
            fmt!(s, "(result {})", ret);
            fmt!(s, "))");
        }
    }
}

fn gen_text_literal(s: &mut String, text: String) {
    let mut bytes = text.into_bytes();
    bytes.resize(bytes.len() + (4 - bytes.len() % 4), 0);

    fmt!(s, "(global.set $handy1 (call $malloc (i32.const {})))", bytes.len());
    for (i, byte) in bytes.iter().enumerate() {
        fmt!(s, "(i32.store8 (i32.add (global.get $handy1) (i32.const {})) (i32.const {}))", i, byte);
    }
    fmt!(s, "(global.get $handy1)");
}

fn gen_instr(s: &mut String, fn_types: &mut FnTypes, instr: Instr) {
    match instr {
        Instr::PushInt { value } => fmt!(s, "(i64.const {})", value),
        Instr::PushReal { value } => fmt!(s, "(f64.const {})", value),
        Instr::PushText { value } => gen_text_literal(s, value),
        Instr::PushBool { value } => fmt!(s, "(i32.const {})", value as i32),
        Instr::Drop { .. } => fmt!(s, "(drop)"),
        Instr::Loop { id, inner } => {
            fmt!(s, "(block $block_{} ", id);
            fmt!(s, "(loop $loop_{} ", id);
            gen_instrs(s, fn_types, inner);
            fmt!(s, "))");
        }
        Instr::Ifte { then, elsë, ret } => {
            fmt!(s, "(if (result {})", type_to_str(&ret));
            fmt!(s, "(then ");
            gen_instrs(s, fn_types, then);
            fmt!(s, ")(else ");
            gen_instrs(s, fn_types, elsë);
            fmt!(s, "))");
        }
        Instr::RepeatLoop { id } => fmt!(s, "(br $loop_{})", id),
        Instr::QuitUnless { id } => fmt!(s, "(br_if $block_{} (i32.eqz))", id),
        Instr::NewTuple { fields } => {
            if fields.is_empty() {
                fmt!(s, "(i32.const -1)")
            } else {
                fmt!(s, "(global.set $handy1 (call $malloc (i32.const {})))", fields.len() * 8);
                for i in (0..fields.len()).rev() {
                    fmt!(s, "(global.set $handy2)");
                    fmt!(s, "(i64.store (i32.add (global.get $handy1) (i32.const {})) (global.get $handy2))", i * 8);
                }
                fmt!(s, "(global.get $handy1)");
            }
        }
        Instr::GetField { fields, i } => {
            fmt!(s, "({}.load offset={})", type_to_str(&fields[i]), i * 8);
        }
        Instr::SetField { fields, i } => {
            fmt!(s, "(global.set $tmp{})", type_to_str(&fields[i]));
            fmt!(s, "(i32.add (i32.const {}))", i * 8);
            fmt!(s, "(global.get $tmp{})", type_to_str(&fields[i]));
            fmt!(s, "({}.store)", type_to_str(&fields[i]));
        }
        Instr::GetLocal { id, .. } => fmt!(s, "(local.get {})", id),
        Instr::SetLocal { id, .. } => fmt!(s, "(local.set {})", id),
        Instr::CallFunc { args, ret } => {
            fmt!(s, "(global.set $handy1)");
            fmt!(s, "(global.set $capture_ptr (i32.add (global.get $handy1) (i32.const 1)))");
            fmt!(s, "(i32.load (global.get $handy1))");

            fmt!(s, "(call_indirect (type ${}))", fn_types.type_of(&args, &ret));
        }
        Instr::BindFunc { id, captures, .. } => {
            fmt!(s, "(global.set $handy1 (call $malloc (i32.const {})))", (captures.len() + 1) * 8);
            fmt!(s, "(i32.store (global.get $handy1) (i32.const {}))", id);
            for (i, (loc_id, tpe)) in captures.into_iter().enumerate() {
                fmt!(s, "(i32.add (global.get $handy1) (i32.const {}))", (i + 1) * 8);
                fmt!(s, "(local.get {})", loc_id);
                fmt!(s, "({}.store)", type_to_str(&tpe));
            }
            fmt!(s, "(global.get $handy1)");
        }
        Instr::RealToInt => fmt!(s, "(i64.trunc_f64_s)"),
        Instr::IntToReal => fmt!(s, "(f64.convert_i64_s)"),
        Instr::IntToTextAscii => {
            fmt!(s, "(global.set $handy1 (call $malloc (i32.const 2)))");
            fmt!(s, "(global.set $handy2 (i32.wrap_i64))");
            fmt!(s, "(i32.store8 (global.get $handy1) (global.get $handy2))");
            fmt!(s, "(i32.store8 (i32.add (global.get $handy1) (i32.const 1)) (i32.const 0))");
            fmt!(s, "(global.get $handy1)");
        }
        Instr::NotInt => fmt!(s, "(i64.xor (i64.const -1))"),
        Instr::NotBool => fmt!(s, "(i32.eqz)"),
        Instr::PrintText => fmt!(s, "(call $print_text)(i32.const 0)"),
        Instr::PrintReal => fmt!(s, "(call $print_real)(i32.const 0)"),
        Instr::PrintInt => fmt!(s, "(call $print_int)(i32.const 0)"),
        Instr::NewArray { item: _item } => {
            fmt!(s, "(global.set $handy2 (i32.wrap_i64))");
            fmt!(s, "(global.set $handy1 (call $malloc (i32.add (i32.mul (global.get $handy2) (i32.const 8)) (i32.const 1))))");
            fmt!(s, "(i64.store (global.get $handy1) (i64.extend_i32_s (global.get $handy2)))");
            fmt!(s, "(global.get $handy1)");
        }
        Instr::LenArray { item: _item } => {
            fmt!(s, "(i64.load)");
        }
        Instr::GetArray { item } => {
            fmt!(s, "(i32.add (i32.mul (i32.wrap_i64) (i32.const 8)) (i32.const 8))");
            fmt!(s, "(i32.add)");
            fmt!(s, "({}.load)", type_to_str(&item));
        }
        Instr::SetArray { item } => {
            fmt!(s, "(global.set $tmp{})", type_to_str(&item));
            fmt!(s, "(i32.add (i32.mul (i32.wrap_i64) (i32.const 8)) (i32.const 8))");
            fmt!(s, "(i32.add)");
            fmt!(s, "(global.get $tmp{})", type_to_str(&item));
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
        Instr::LenText => fmt!(s, "(i64.extend_i32_s (call $strlen))"),
        Instr::MulInt => fmt!(s, "(i64.mul)"),
        Instr::MulReal => fmt!(s, "(f64.add)"),
        Instr::DivInt => fmt!(s, "(i64.div_s)"),
        Instr::DivReal => fmt!(s, "(f64.div)"),
        Instr::RemInt => fmt!(s, "(i64.rem_s)"),
        Instr::AddInt => fmt!(s, "(i64.add)"),
        Instr::AddReal => fmt!(s, "(f64.add)"),
        Instr::SubInt => fmt!(s, "(i64.sub)"),
        Instr::SubReal => fmt!(s, "(f64.sub)"),
        Instr::LtInt => fmt!(s, "(i64.lt_s)"),
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

fn gen_instrs(s: &mut String, fn_types: &mut FnTypes, instrs: Vec<Instr>) {
    for instr in instrs {
        gen_instr(s, fn_types, instr);
    }
}

fn gen_func(s: &mut String, fn_types: &mut FnTypes, i: usize, f: Func) {
    fmt!(s, "(func $func_{} ", i);

    if !f.args.is_empty() {
        fmt!(s, "(param");
        for arg in &f.args {
            fmt!(s, " {}", type_to_str(arg));
        }
        fmt!(s, ") ");
    }
    fmt!(s, "(result {})", type_to_str(&f.ret));

    for (i, (loc_id, loc_tpe)) in f.captures.into_iter().enumerate() {
        fmt!(s, "(local {})", type_to_str(&loc_tpe));
        fmt!(s, "({}.load (global.get $capture_ptr) offset={})", type_to_str(&loc_tpe), i * 8);
        fmt!(s, "(local.set {})", loc_id);
    }

    for loc_tpe in f.locals {
        fmt!(s, "(local {})", type_to_str(&loc_tpe));
    }

    gen_instrs(s, fn_types, f.code);
    fmt!(s, ")");

    fmt!(s, "(elem (i32.const {}) $func_{})", i, i);
}

fn gen_program(s: &mut String, funcs: Vec<Func>, entry: usize) {
    fmt!(s, "(module
(import \"env\" \"print_text\" (func $print_text (param i32)))
(import \"env\" \"print_real\" (func $print_real (param f64)))
(import \"env\" \"print_ints\" (func $print_ints (param i32 i32)))

(func $print_int (param $n i64)
  (call $print_ints
    (i32.wrap_i64 (local.get $n))
    (i32.wrap_i64 (i64.shr_u (local.get $n) (i64.const 32)))))

(func $strlen (param $ptr i32) (result i32)
  (local $len i32)
  (local.set $len (i32.const 0))
  (block $exit
    (loop $loop
      (i32.load8_u (local.get $ptr))
        (br_if $exit (i32.eqz (i32.load8_u (local.get $ptr))))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
        (local.set $len (i32.add (local.get $len) (i32.const 1)))
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

(memory (export \"memory\") 1)
(global $heap_ptr (mut i32) (i32.const 0))
(func $malloc (param $size i32) (result i32)
    (local $old_base i32)
    (local $new_end i32)
    (local $need i32)

    (if (i32.eqz (local.get $size))
      (then
        (local.set $size (i32.const 1))))

    (local.set $size (i32.and (i32.add (local.get $size) (i32.const 7)) (i32.const -8)))

    (if (i32.eqz (global.get $heap_ptr))
      (then
        (global.set $heap_ptr (i32.shl (memory.size) (i32.const 16)))))

    (local.set $old_base (global.get $heap_ptr))
    (local.set $new_end (i32.add (local.get $old_base) (local.get $size)))
    (local.set $need (i32.shr_u (i32.add (local.get $new_end) (i32.const 65535)) (i32.const 16)))
    (local.set $need (i32.sub (local.get $need) (memory.size)))

    (if (i32.gt_s (local.get $need) (i32.const 0))
      (then
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
(global $tmpf64 (mut f64) (f64.const 0.0))
(global $tmpi32 (mut i32) (i32.const 0))
(global $tmpi64 (mut i64) (i64.const 0))
(global $capture_ptr (mut i32) (i32.const 0))
");

    fmt!(s, "(table $table {} funcref)", funcs.len());

    let mut fn_types = FnTypes { map: HashMap::new() };
    for (i, func) in funcs.into_iter().enumerate() {
        gen_func(s, &mut fn_types, i, func);
    }

    fn_types.gen_signatures(s);

    fmt!(s, "(func (export \"main\") (call $func_{}) (drop))", entry);
    fmt!(s, ") ");
}

pub fn generate(program: Program) -> String {
    let mut s = String::new();
    gen_program(&mut s, program.funcs, program.entry);
    s
}

/*

let memory = null;

function print_text(ptr) {
  const bytes = new Uint8Array(memory.buffer, ptr);
  let str = "";
  for (let i = 0; bytes[i] !== 0; i++) {
    str += String.fromCharCode(bytes[i]);
  }
  console.log(str);
}

function print_real(num) {
  console.log(num);
}

function print_ints(low, high) {
  const big = BigInt(low >>> 0) | (BigInt(high) << 32n);
  console.log(big.toString());
}

const wasmInstance =
      new WebAssembly.Instance(wasmModule, { env: { print_text, print_real, print_ints, memory } });

let main = wasmInstance.exports.main;
memory = wasmInstance.exports.memory;
main()


*/
