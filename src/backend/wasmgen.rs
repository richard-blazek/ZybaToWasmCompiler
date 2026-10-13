use std::collections::HashMap;

use crate::midend::*;

macro_rules! fmt {
    ($s:expr, $($arg:tt)*) => {
        ($s).push_str(&format!($($arg)*))
    }
}

const PRELUDE: &str = r#"
(import "env" "print_text" (func $print_text (param i32)))
(import "env" "print_real" (func $print_real (param f64)))
(import "env" "print_ints" (func $print_ints (param i32 i32)))

(func $print_int (param $n i64)
  (call $print_ints
    (i32.wrap_i64 (local.get $n))
    (i32.wrap_i64 (i64.shr_u (local.get $n) (i64.const 32)))))

;; Makes the memory below $end accessible
(func $ensure_memory (param $end i64)
  (local $need i32)
  (if (i64.gt_u (local.get $end) (i64.const 0x100000000))
    (then
      (unreachable)))
  (local.set $need
    (i32.sub
      (i32.wrap_i64 (i64.shr_u (i64.add (local.get $end) (i64.const 65535)) (i64.const 16)))
      (memory.size)))
  (if (i32.gt_s (local.get $need) (i32.const 0))
    (then
      (if (i32.eq (memory.grow (local.get $need)) (i32.const -1))
        (then
          (unreachable))))))
"#;

// Static data starts here so that address 0 stays the null address
const DATA_START: usize = 8;

fn wasm_type(ty: Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::F64 => "f64",
        Type::Addr | Type::Bool => "i32"
    }
}

fn wasm_types(tys: &[Type]) -> Vec<&'static str> {
    tys.iter().map(|t| wasm_type(*t)).collect()
}

fn escape(bytes: &[u8]) -> String {
    bytes.iter().map(|&c| {
        if c.is_ascii_graphic() && c != b'"' && c != b'\\' {
            (c as char).to_string()
        } else {
            format!("\\{:02x}", c)
        }
    }).collect()
}

struct SigTypes {
    map: HashMap<(Vec<&'static str>, Vec<&'static str>), usize>
}

impl SigTypes {
    fn id(&mut self, sig: &Signature) -> usize {
        let key = (wasm_types(&sig.params), wasm_types(&sig.results));
        let next = self.map.len();
        *self.map.entry(key).or_insert(next)
    }

    fn emit(&self, s: &mut String) {
        let mut sigs: Vec<_> = self.map.iter().collect();
        sigs.sort_by_key(|(_, id)| **id);
        for ((params, results), id) in sigs {
            fmt!(s, "(type $sig{} (func", id);
            for p in params {
                fmt!(s, " (param {})", p);
            }
            for r in results {
                fmt!(s, " (result {})", r);
            }
            fmt!(s, "))\n");
        }
    }
}

struct FuncGen<'a> {
    s: &'a mut String,
    sigs: &'a mut SigTypes,
    data_offsets: &'a [usize],
    heap_begin: usize,
    func: &'a Function,
    loop_count: usize,
    indent: usize
}

fn get(v: usize) -> String {
    format!("(local.get $v{})", v)
}

fn int_bin(op: IntBinOp) -> &'static str {
    match op {
        IntBinOp::Add => "i64.add",
        IntBinOp::Sub => "i64.sub",
        IntBinOp::Mul => "i64.mul",
        IntBinOp::DivS => "i64.div_s",
        IntBinOp::RemS => "i64.rem_s",
        IntBinOp::And => "i64.and",
        IntBinOp::Or => "i64.or",
        IntBinOp::Xor => "i64.xor",
        IntBinOp::Shl => "i64.shl",
        IntBinOp::ShrS => "i64.shr_s",
        IntBinOp::ShrU => "i64.shr_u"
    }
}

fn int_cmp(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "i64.eq",
        CmpOp::Ne => "i64.ne",
        CmpOp::Lt => "i64.lt_s",
        CmpOp::Le => "i64.le_s",
        CmpOp::Gt => "i64.gt_s",
        CmpOp::Ge => "i64.ge_s"
    }
}

fn real_bin(op: RealBinOp) -> &'static str {
    match op {
        RealBinOp::Add => "f64.add",
        RealBinOp::Sub => "f64.sub",
        RealBinOp::Mul => "f64.mul",
        RealBinOp::Div => "f64.div"
    }
}

fn real_cmp(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "f64.eq",
        CmpOp::Ne => "f64.ne",
        CmpOp::Lt => "f64.lt",
        CmpOp::Le => "f64.le",
        CmpOp::Gt => "f64.gt",
        CmpOp::Ge => "f64.ge"
    }
}

fn bool_bin(op: BoolBinOp) -> &'static str {
    match op {
        BoolBinOp::And => "i32.and",
        BoolBinOp::Or => "i32.or",
        BoolBinOp::Xor => "i32.xor",
        BoolBinOp::Eq => "i32.eq"
    }
}

impl<'a> FuncGen<'a> {
    fn line(&mut self, text: &str) {
        for _ in 0..self.indent {
            self.s.push_str("  ");
        }
        self.s.push_str(text);
        self.s.push('\n');
    }

    fn assign(&mut self, targets: &[usize], values: &[usize]) {
        for v in values {
            self.line(&get(*v));
        }
        for t in targets.iter().rev() {
            self.line(&format!("(local.set $v{})", t));
        }
    }

    fn instrs(&mut self, block: &Block) {
        for instr in &block.instrs {
            self.instr(instr);
        }
    }

    fn block(&mut self, block: &Block, yield_to: &[usize]) {
        self.indent += 1;
        self.instrs(block);
        self.assign(yield_to, &block.yields);
        self.indent -= 1;
    }

    fn expr(&mut self, op: &Op) -> String {
        let ty = |t: &Type| wasm_type(*t);
        match op {
            Op::ConstI64(v) => format!("(i64.const {})", v),
            Op::ConstF64(v) => format!("(f64.reinterpret_i64 (i64.const {}))", v.to_bits() as i64),
            Op::ConstBool(v) => format!("(i32.const {})", *v as i32),
            Op::NullAddr => "(i32.const 0)".to_string(),
            Op::DataAddr(id) => format!("(i32.const {})", self.data_offsets[*id]),
            Op::FuncAddr(id) => format!("(i32.const {})", id),
            Op::IntBin(op, a, b) => format!("({} {} {})", int_bin(*op), get(*a), get(*b)),
            Op::IntCmp(op, a, b) => format!("({} {} {})", int_cmp(*op), get(*a), get(*b)),
            Op::RealBin(op, a, b) => format!("({} {} {})", real_bin(*op), get(*a), get(*b)),
            Op::RealCmp(op, a, b) => format!("({} {} {})", real_cmp(*op), get(*a), get(*b)),
            Op::BoolBin(op, a, b) => format!("({} {} {})", bool_bin(*op), get(*a), get(*b)),
            Op::BoolNot(a) => format!("(i32.eqz {})", get(*a)),
            Op::I64ToF64(a) => format!("(f64.convert_i64_s {})", get(*a)),
            Op::F64ToI64(a) => format!("(i64.trunc_f64_s {})", get(*a)),
            Op::BoolToI64(a) => format!("(i64.extend_i32_u {})", get(*a)),
            Op::AddrAdd(a, b) => format!("(i32.add {} (i32.wrap_i64 {}))", get(*a), get(*b)),
            Op::AddrToI64(a) => format!("(i64.extend_i32_u {})", get(*a)),
            Op::HeapBegin => format!("(i32.const {})", self.heap_begin),
            Op::EnsureMemory(end) => format!("(call $ensure_memory {})", get(*end)),
            Op::Load { ty: t, addr, offset } => format!("({}.load offset={} {})", ty(t), offset, get(*addr)),
            Op::Store { ty: t, addr, offset, value } => {
                format!("({}.store offset={} {} {})", ty(t), offset, get(*addr), get(*value))
            }
            Op::LoadU8 { addr, offset } => format!("(i64.load8_u offset={} {})", offset, get(*addr)),
            Op::StoreU8 { addr, offset, value } => {
                format!("(i64.store8 offset={} {} {})", offset, get(*addr), get(*value))
            }
            Op::Call { func, args } => {
                let args: String = args.iter().map(|a| format!(" {}", get(*a))).collect();
                format!("(call $f{}{})", func, args)
            }
            Op::CallIndirect { callee, sig, args } => {
                let args: String = args.iter().map(|a| format!(" {}", get(*a))).collect();
                format!("(call_indirect (type $sig{}){} {})", self.sigs.id(sig), args, get(*callee))
            }
            Op::PrintInt(v) => format!("(call $print_int {})", get(*v)),
            Op::PrintReal(v) => format!("(call $print_real {})", get(*v)),
            Op::PrintText(v) => format!("(call $print_text {})", get(*v)),
            Op::Abort => "(unreachable)".to_string(),
            Op::If { .. } | Op::Loop { .. } => unreachable!()
        }
    }

    fn instr(&mut self, instr: &Instr) {
        match &instr.op {
            Op::If { cond, then, elsë } => {
                self.line(&format!("(if {}", get(*cond)));
                self.indent += 1;
                self.line("(then");
                self.block(then, &instr.results);
                self.line(")");
                self.line("(else");
                self.block(elsë, &instr.results);
                self.line("))");
                self.indent -= 1;
            }
            Op::Loop { params, inits, body } => {
                let label = self.loop_count;
                self.loop_count += 1;
                self.assign(params, inits);
                self.line(&format!("(loop $loop{}", label));
                self.indent += 1;
                self.instrs(body);

                let (again, next) = body.yields.split_first().unwrap();
                self.line(&get(*again));
                self.assign(params, next);
                self.line(&format!("(br_if $loop{}))", label));
                self.indent -= 1;
                self.assign(&instr.results, params);
            }
            op => {
                let expr = self.expr(op);
                match instr.results[..] {
                    [] => self.line(&expr),
                    [r] => self.line(&format!("(local.set $v{} {})", r, expr)),
                    _ => unreachable!("multiple results")
                }
            }
        }
    }

    fn function(&mut self, id: usize) {
        let func = self.func;
        let sig = self.sigs.id(&func.sig);
        let mut header = format!("(func $f{} (type $sig{})", id, sig);
        for p in &func.params {
            fmt!(header, " (param $v{} {})", p, wasm_type(func.type_of(*p)));
        }
        for r in &func.sig.results {
            fmt!(header, " (result {})", wasm_type(*r));
        }
        self.line(&format!("{} ;; {}", header, func.name));

        self.indent += 1;
        for (i, ty) in func.types.iter().enumerate() {
            if !func.params.contains(&i) {
                self.line(&format!("(local $v{} {})", i, wasm_type(*ty)));
            }
        }

        self.instrs(&func.body);
        for v in &func.body.yields {
            self.line(&get(*v));
        }
        self.line(")");
        self.indent -= 1;
    }
}

pub fn generate(module: Module) -> String {
    let mut s = String::from("(module\n");
    s.push_str(PRELUDE);
    s.push('\n');

    let mut data_offsets = vec![];
    let mut end = DATA_START;
    for data in &module.data {
        data_offsets.push(end);
        end = (end + data.bytes.len() + 7) & !7;
    }
    fmt!(s, "(memory (export \"memory\") {})\n", end.div_ceil(65536).max(1));
    for (data, offset) in module.data.iter().zip(&data_offsets) {
        fmt!(s, "(data (i32.const {}) \"{}\")\n", offset, escape(&data.bytes));
    }

    let n = module.funcs.len();
    fmt!(s, "(table {} funcref)\n", n);
    let elems: String = (0..n).map(|i| format!(" $f{}", i)).collect();
    fmt!(s, "(elem (i32.const 0){})\n\n", elems);

    let mut sigs = SigTypes { map: HashMap::new() };
    let mut funcs = String::new();
    for (i, func) in module.funcs.iter().enumerate() {
        let mut fg = FuncGen {
            s: &mut funcs, sigs: &mut sigs, data_offsets: &data_offsets, heap_begin: end, func, loop_count: 0, indent: 0
        };
        fg.function(i);
        funcs.push('\n');
    }
    sigs.emit(&mut s);
    s.push('\n');
    s.push_str(&funcs);

    fmt!(s, "(func (export \"main\") (call $f{}))\n", module.main_fn);
    s.push_str(")\n");
    s
}
