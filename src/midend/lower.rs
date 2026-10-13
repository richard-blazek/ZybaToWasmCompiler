use std::collections::{BTreeSet, HashMap, HashSet};

use crate::frontend::{self, Value};
use crate::midend::builder::{FunctionBuilder, Helper, ModuleBuilder};
use crate::midend::ir::*;
use crate::midend::layout::*;
use crate::midend::runtime;
use crate::midend::utils::sorted;

fn collect_vars(v: &Value) -> (HashSet<String>, HashSet<String>) {
    let mut used = HashSet::new();
    let mut defined = HashSet::new();
    let add = |v: &Value, used: &mut HashSet<String>, defined: &mut HashSet<String>| {
        let (new_used, new_defined) = collect_vars(v);
        used.extend(new_used);
        defined.extend(new_defined);
    };

    match v {
        Value::Record { fields, .. } => {
            for value in fields.values() {
                add(value, &mut used, &mut defined);
            }
        }
        Value::Var { name, .. } => {
            used.insert(name.clone());
        }
        Value::Call { func, args, .. } => {
            add(func, &mut used, &mut defined);
            for arg in args {
                add(arg, &mut used, &mut defined);
            }
        }
        Value::Builtin { args, .. } => {
            for arg in args {
                add(arg, &mut used, &mut defined);
            }
        }
        Value::Access { object, .. } => add(object, &mut used, &mut defined),
        Value::Lambda { args, body, .. } => {
            add(body, &mut used, &mut defined);
            defined.extend(args.iter().map(|(n, _)| n.clone()));
        }
        Value::Init { name, value, .. } => {
            add(value, &mut used, &mut defined);
            defined.insert(name.clone());
        }
        Value::Assign { value, .. } => add(value, &mut used, &mut defined),
        Value::If { cond, then, elsë, .. } => {
            add(cond, &mut used, &mut defined);
            add(then, &mut used, &mut defined);
            add(elsë, &mut used, &mut defined);
        }
        Value::While { cond, body, .. } => {
            add(cond, &mut used, &mut defined);
            add(body, &mut used, &mut defined);
        }
        Value::For { key, value, expr, body, .. } => {
            add(expr, &mut used, &mut defined);
            add(body, &mut used, &mut defined);
            defined.extend([key.clone(), value.clone()]);
        }
        Value::Block { values, .. } => {
            for value in values {
                add(value, &mut used, &mut defined);
            }
        }
        Value::Int { .. } | Value::Real { .. } | Value::Text { .. } | Value::Bool { .. } => {}
    }
    (used, defined)
}

fn collect_assigned(v: &Value, out: &mut BTreeSet<String>) {
    match v {
        Value::Assign { name, value, .. } => {
            out.insert(name.clone());
            collect_assigned(value, out);
        }
        Value::Record { fields, .. } => fields.values().for_each(|v| collect_assigned(v, out)),
        Value::Call { func, args, .. } => {
            collect_assigned(func, out);
            args.iter().for_each(|v| collect_assigned(v, out));
        }
        Value::Builtin { args, .. } => args.iter().for_each(|v| collect_assigned(v, out)),
        Value::Access { object, .. } => collect_assigned(object, out),
        Value::Lambda { body, .. } => collect_assigned(body, out),
        Value::Init { value, .. } => collect_assigned(value, out),
        Value::If { cond, then, elsë, .. } => {
            collect_assigned(cond, out);
            collect_assigned(then, out);
            collect_assigned(elsë, out);
        }
        Value::While { cond, body, .. } => {
            collect_assigned(cond, out);
            collect_assigned(body, out);
        }
        Value::For { expr, body, .. } => {
            collect_assigned(expr, out);
            collect_assigned(body, out);
        }
        Value::Block { values, .. } => values.iter().for_each(|v| collect_assigned(v, out)),
        Value::Int { .. } | Value::Real { .. } | Value::Text { .. } | Value::Bool { .. } | Value::Var { .. } => {}
    }
}

enum Global {
    Int(i64),
    Real(f64),
    Text(String),
    Bool(bool),
    Func(usize)
}

struct Lowerer {
    mb: ModuleBuilder,
    globals: HashMap<String, Global>
}

struct FnCtx {
    fb: FunctionBuilder,
    env: HashMap<String, Option<usize>>
}

impl FnCtx {
    // Variables of `env` assigned somewhere in `values` that need SSA merging
    fn carried(&self, values: &[&Value]) -> Vec<String> {
        let mut assigned = BTreeSet::new();
        for v in values {
            collect_assigned(v, &mut assigned);
        }
        assigned.into_iter().filter(|name| {
            matches!(self.env.get(name), Some(Some(_)))
        }).collect()
    }

    fn current(&self, names: &[String]) -> Vec<usize> {
        names.iter().map(|name| self.env[name].unwrap()).collect()
    }

    fn rebind(&mut self, names: &[String], vals: &[usize]) {
        for (name, val) in names.iter().zip(vals) {
            self.env.insert(name.clone(), Some(*val));
        }
    }

}

fn first2(args: Vec<Value>) -> (Value, Value) {
    let mut it = args.into_iter();
    (it.next().unwrap(), it.next().unwrap())
}

fn cmp_op(op: &str) -> CmpOp {
    match op {
        "==" => CmpOp::Eq,
        "!=" => CmpOp::Ne,
        "<" => CmpOp::Lt,
        "<=" => CmpOp::Le,
        ">" => CmpOp::Gt,
        ">=" => CmpOp::Ge,
        _ => unreachable!()
    }
}

fn int_op(op: &str) -> IntBinOp {
    match op {
        "+" => IntBinOp::Add,
        "-" => IntBinOp::Sub,
        "*" => IntBinOp::Mul,
        "/" => IntBinOp::DivS,
        "%" => IntBinOp::RemS,
        "&" => IntBinOp::And,
        "|" => IntBinOp::Or,
        "^" => IntBinOp::Xor,
        "<<" => IntBinOp::Shl,
        ">>" => IntBinOp::ShrS,
        ">>>" => IntBinOp::ShrU,
        _ => unreachable!()
    }
}

fn real_op(op: &str) -> RealBinOp {
    match op {
        "+" => RealBinOp::Add,
        "-" => RealBinOp::Sub,
        "*" => RealBinOp::Mul,
        "/" => RealBinOp::Div,
        _ => unreachable!()
    }
}

impl Lowerer {
    fn gen_var(&mut self, name: String, f: &mut FnCtx) -> Option<usize> {
        if let Some(val) = f.env.get(&name) {
            return *val;
        }
        let fb = &mut f.fb;
        Some(match &self.globals[&name] {
            Global::Int(value) => fb.i64(*value),
            Global::Real(value) => fb.f64(*value),
            Global::Bool(value) => fb.bool(*value),
            Global::Text(value) => {
                let id = self.mb.text(value);
                fb.emit(Type::Addr, Op::DataAddr(id))
            }
            Global::Func(id) => {
                let closure = fb.alloc_const(CAPTURES_OFFSET);
                let code = fb.emit(Type::Addr, Op::FuncAddr(*id));
                fb.store(closure, CODE_OFFSET, code);
                closure
            }
        })
    }

    fn gen_record(&mut self, fields: HashMap<String, Value>, tpe: frontend::Type, f: &mut FnCtx) -> Option<usize> {
        let mut vals = HashMap::new();
        for (name, value) in sorted(fields, |(n, _)| n) {
            vals.insert(name, self.gen_value(value, f));
        }
        let slots = record_slots(record_fields(&tpe));
        if slots.is_empty() {
            return None;
        }
        let record = f.fb.alloc_const(slots.len() as u32 * SLOT);
        for (i, (name, _)) in slots.iter().enumerate() {
            f.fb.store(record, slot_offset(i), vals[name].unwrap());
        }
        Some(record)
    }

    fn gen_access(&mut self, object: Value, field: String, f: &mut FnCtx) -> Option<usize> {
        let tpe = object.tpe();
        let object = self.gen_value(object, f);
        let slots = record_slots(record_fields(&tpe));
        let i = slots.iter().position(|(n, _)| *n == field)?;
        Some(f.fb.load(slots[i].1, object.unwrap(), slot_offset(i)))
    }

    fn gen_merge(&mut self, lhs: Value, rhs: Value, tpe: frontend::Type, f: &mut FnCtx) -> Option<usize> {
        let (lhs_t, rhs_t) = (lhs.tpe(), rhs.tpe());
        let lhs = self.gen_value(lhs, f);
        let rhs = self.gen_value(rhs, f);
        let lhs_slots = record_slots(record_fields(&lhs_t));
        let rhs_slots = record_slots(record_fields(&rhs_t));
        let slots = record_slots(record_fields(&tpe));
        if slots.is_empty() {
            return None;
        }

        let record = f.fb.alloc_const(slots.len() as u32 * SLOT);
        for (i, (name, ty)) in slots.iter().enumerate() {
            // A field of the right record wins
            let (source, source_slots) = if record_fields(&rhs_t).contains_key(name) {
                (rhs, &rhs_slots)
            } else {
                (lhs, &lhs_slots)
            };
            let j = source_slots.iter().position(|(n, _)| n == name).unwrap();
            let value = f.fb.load(*ty, source.unwrap(), slot_offset(j));
            f.fb.store(record, slot_offset(i), value);
        }
        Some(record)
    }

    fn gen_call(&mut self, func: Value, args: Vec<Value>, f: &mut FnCtx) -> Option<usize> {
        let (arg_types, ret) = match func.tpe() {
            frontend::Type::Func { args, ret } => (args, *ret),
            _ => unreachable!()
        };
        let mut arg_vals = vec![];
        for arg in args {
            arg_vals.extend(self.gen_value(arg, f));
        }
        let closure = self.gen_value(func, f).unwrap();
        let code = f.fb.load(Type::Addr, closure, CODE_OFFSET);

        let sig = func_sig(&arg_types, &ret);
        let results = f.fb.new_vals(&sig.results);
        let args = std::iter::once(closure).chain(arg_vals).collect();
        f.fb.push(results.clone(), Op::CallIndirect { callee: code, sig, args });
        results.first().copied()
    }

    fn gen_if(&mut self, cond: Value, then: Value, elsë: Value, tpe: frontend::Type, f: &mut FnCtx) -> Option<usize> {
        let cond = self.gen_value(cond, f).unwrap();
        let carried = f.carried(&[&then, &elsë]);
        let before = f.env.clone();
        let value_t = repr(&tpe);

        let mut gen_arm = |arm: Value, f: &mut FnCtx| {
            f.fb.begin_block();
            let value = self.gen_value(arm, f);
            let yielded = value_t.and(value).into_iter().chain(f.current(&carried)).collect();
            f.env = before.clone();
            f.fb.end_block(yielded)
        };
        let then = gen_arm(then, f);
        let elsë = gen_arm(elsë, f);

        let results = f.fb.if_(cond, then, elsë);
        let (value, vars) = results.split_at(value_t.iter().len());
        f.rebind(&carried, vars);
        value.first().copied()
    }

    // Emits a loop carrying `extra` hidden values followed by the variables
    // `names`. Each iteration runs `check` and, if the condition it returns
    // holds, `body`, which returns the hidden values for the next iteration.
    fn gen_loop(
        &mut self,
        names: &[String],
        extra: Vec<usize>,
        f: &mut FnCtx,
        check: impl FnOnce(&mut Self, &mut FnCtx, &[usize]) -> usize,
        body: impl FnOnce(&mut Self, &mut FnCtx, &[usize]) -> Vec<usize>
    ) {
        let n = extra.len();
        let inits: Vec<usize> = extra.into_iter().chain(f.current(names)).collect();
        let params = f.fb.new_vals(&f.fb.types_of(&inits));
        let before = f.env.clone();

        f.fb.begin_block();
        f.rebind(names, &params[n..]);
        let cond = check(self, f, &params[..n]);
        // The values at the time of the check become the results if it fails
        let at_check: Vec<usize> = params[..n].iter().copied().chain(f.current(names)).collect();

        f.fb.begin_block();
        let next_extra = body(self, f, &params[..n]);
        let next = next_extra.into_iter().chain(f.current(names)).collect();
        let then = f.fb.end_block(next);
        f.fb.begin_block();
        let elsë = f.fb.end_block(at_check);
        let step = f.fb.if_(cond, then, elsë);

        let body = f.fb.end_block(std::iter::once(cond).chain(step).collect());
        f.env = before;
        let results = f.fb.loop_(params, inits, body);
        f.rebind(names, &results[n..]);
    }

    fn gen_while(&mut self, cond: Value, body: Value, f: &mut FnCtx) -> Option<usize> {
        let carried = f.carried(&[&cond, &body]);
        self.gen_loop(&carried, vec![], f, |this, f, _| {
            this.gen_value(cond, f).unwrap()
        }, |this, f, _| {
            this.gen_value(body, f);
            vec![]
        });
        None
    }

    fn gen_for(&mut self, key: String, value: String, expr: Value, body: Value, f: &mut FnCtx) -> Option<usize> {
        let item_t = match expr.tpe() {
            frontend::Type::Array { item } => repr(&item),
            _ => unreachable!()
        };
        let array = self.gen_value(expr, f).unwrap();
        let len = f.fb.load(Type::I64, array, LEN_OFFSET);
        let zero = f.fb.i64(0);

        // The index is a hidden counter: assigning the key variable in the
        // body does not affect the iteration
        let carried = f.carried(&[&body]);
        self.gen_loop(&carried, vec![zero], f, |_, f, extra| {
            f.fb.int_cmp(CmpOp::Lt, extra[0], len)
        }, |this, f, extra| {
            let i = extra[0];
            let item = item_t.map(|ty| {
                let slot = f.fb.i64(SLOT as i64);
                let offset = f.fb.int_bin(IntBinOp::Mul, i, slot);
                let addr = f.fb.addr_add(array, offset);
                f.fb.load(ty, addr, ITEMS_OFFSET)
            });
            f.env.insert(key, Some(i));
            f.env.insert(value, item);
            this.gen_value(body, f);

            let one = f.fb.i64(1);
            vec![f.fb.int_bin(IntBinOp::Add, i, one)]
        });
        None
    }

    fn gen_array(&mut self, init: Value, len: Value, f: &mut FnCtx) -> Option<usize> {
        let init = self.gen_value(init, f);
        let len = self.gen_value(len, f).unwrap();
        let zero = f.fb.i64(0);
        let max = f.fb.i64(MAX_ARRAY_LEN);
        let non_negative = f.fb.int_cmp(CmpOp::Ge, len, zero);
        let small = f.fb.int_cmp(CmpOp::Le, len, max);
        let ok = f.fb.bool_bin(BoolBinOp::And, non_negative, small);
        f.fb.trap_unless(ok);

        let header = f.fb.i64(ITEMS_OFFSET as i64);
        let size = if init.is_some() {
            let slot = f.fb.i64(SLOT as i64);
            let items = f.fb.int_bin(IntBinOp::Mul, len, slot);
            f.fb.int_bin(IntBinOp::Add, items, header)
        } else {
            header
        };
        let array = f.fb.alloc(size);
        f.fb.store(array, LEN_OFFSET, len);

        // Items without a representation take no slots, so there is nothing to fill
        if let Some(init) = init {
            self.gen_loop(&[], vec![zero], f, |_, f, extra| {
                f.fb.int_cmp(CmpOp::Lt, extra[0], len)
            }, |_, f, extra| {
                let i = extra[0];
                let slot = f.fb.i64(SLOT as i64);
                let offset = f.fb.int_bin(IntBinOp::Mul, i, slot);
                let addr = f.fb.addr_add(array, offset);
                f.fb.store(addr, ITEMS_OFFSET, init);

                let one = f.fb.i64(1);
                vec![f.fb.int_bin(IntBinOp::Add, i, one)]
            });
        }
        Some(array)
    }

    fn gen_function(
        &mut self,
        name: String,
        args: Vec<(String, frontend::Type)>,
        ret: frontend::Type,
        captures: Vec<(String, Option<Type>)>,
        body: Value
    ) -> Function {
        let arg_types: Vec<_> = args.iter().map(|(_, t)| t.clone()).collect();
        let fb = FunctionBuilder::new(name, func_sig(&arg_types, &ret));
        let mut f = FnCtx { fb, env: HashMap::new() };

        let params = f.fb.params();
        let closure = params[0];
        let mut params = params[1..].iter();
        for (name, tpe) in args {
            let val = repr(&tpe).map(|_| *params.next().unwrap());
            f.env.insert(name, val);
        }

        let mut slot = 0;
        for (name, ty) in captures {
            let val = ty.map(|ty| {
                slot += 1;
                f.fb.load(ty, closure, CAPTURES_OFFSET + slot_offset(slot - 1))
            });
            f.env.insert(name, val);
        }

        let value = self.gen_value(body, &mut f);
        let results = repr(&ret).and(value).into_iter().collect();
        f.fb.finish(results)
    }

    fn gen_lambda(&mut self, args: Vec<(String, frontend::Type)>, ret: frontend::Type, body: Value, f: &mut FnCtx) -> Option<usize> {
        let (used, defined) = collect_vars(&body);
        let arg_names: HashSet<_> = args.iter().map(|(n, _)| n.clone()).collect();
        let mut names: Vec<_> = used.into_iter().filter(|n| {
            !defined.contains(n) && !arg_names.contains(n) && !self.globals.contains_key(n)
        }).collect();
        names.sort();

        let captured: Vec<(String, Option<usize>)> = names.into_iter().map(|n| {
            let val = f.env[&n];
            (n, val)
        }).collect();
        let captures = captured.iter().map(|(n, v)| (n.clone(), v.map(|v| f.fb.type_of(v)))).collect();

        let func = self.gen_function("lambda".into(), args, ret, captures, body);
        let id = self.mb.add_func(func);

        let vals: Vec<usize> = captured.into_iter().filter_map(|(_, v)| v).collect();
        let closure = f.fb.alloc_const(CAPTURES_OFFSET + vals.len() as u32 * SLOT);
        let code = f.fb.emit(Type::Addr, Op::FuncAddr(id));
        f.fb.store(closure, CODE_OFFSET, code);
        for (i, val) in vals.into_iter().enumerate() {
            f.fb.store(closure, CAPTURES_OFFSET + slot_offset(i), val);
        }
        Some(closure)
    }

    fn gen_text_cmp(&mut self, op: &str, lhs: usize, rhs: usize, f: &mut FnCtx) -> usize {
        let compare = self.mb.helper(Helper::TextCompare);
        let sign = f.fb.call(compare, vec![lhs, rhs], &[Type::I64])[0];
        let zero = f.fb.i64(0);
        f.fb.int_cmp(cmp_op(op), sign, zero)
    }

    fn gen_builtin(&mut self, op: String, args: Vec<Value>, tpe: frontend::Type, f: &mut FnCtx) -> Option<usize> {
        use frontend::Type::*;

        let arg_types: Vec<_> = args.iter().map(Value::tpe).collect();
        let op = op.as_str();

        match (op, &arg_types[..]) {
            ("&&", _) => {
                let (lhs, rhs) = first2(args);
                let r#false = Value::Bool { value: false, tpe: Bool };
                return self.gen_if(lhs, rhs, r#false, Bool, f);
            }
            ("||", _) => {
                let (lhs, rhs) = first2(args);
                let r#true = Value::Bool { value: true, tpe: Bool };
                return self.gen_if(lhs, r#true, rhs, Bool, f);
            }
            ("|", [Record { .. }, Record { .. }]) => {
                let (lhs, rhs) = first2(args);
                return self.gen_merge(lhs, rhs, tpe, f);
            }
            ("array", _) => {
                let (init, len) = first2(args);
                return self.gen_array(init, len, f);
            }
            _ => {}
        }

        let mut vals = vec![];
        for arg in args {
            vals.push(self.gen_value(arg, f));
        }
        let fb = &mut f.fb;
        let arg = |i: usize| vals[i].unwrap();

        Some(match (op, &arg_types[..]) {
            ("+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | ">>>", [Int, Int]) => {
                fb.int_bin(int_op(op), arg(0), arg(1))
            }
            ("+" | "-" | "*" | "/", [Real, Real]) => {
                fb.emit(Type::F64, Op::RealBin(real_op(op), arg(0), arg(1)))
            }
            ("+", [Text, Text]) => {
                let concat = self.mb.helper(Helper::TextConcat);
                fb.call(concat, vec![arg(0), arg(1)], &[Type::Addr])[0]
            }
            ("&", [Bool, Bool]) => fb.bool_bin(BoolBinOp::And, arg(0), arg(1)),
            ("|", [Bool, Bool]) => fb.bool_bin(BoolBinOp::Or, arg(0), arg(1)),
            ("^" | "!=", [Bool, Bool]) => fb.bool_bin(BoolBinOp::Xor, arg(0), arg(1)),
            ("==", [Bool, Bool]) => fb.bool_bin(BoolBinOp::Eq, arg(0), arg(1)),
            ("==" | "!=" | "<" | "<=" | ">" | ">=", [Int, Int]) => {
                fb.int_cmp(cmp_op(op), arg(0), arg(1))
            }
            ("==" | "!=" | "<" | "<=" | ">" | ">=", [Real, Real]) => {
                fb.emit(Type::Bool, Op::RealCmp(cmp_op(op), arg(0), arg(1)))
            }
            ("==" | "!=" | "<" | "<=" | ">" | ">=", [Text, Text]) => {
                let (lhs, rhs) = (arg(0), arg(1));
                self.gen_text_cmp(op, lhs, rhs, f)
            }
            ("int", [Int]) | ("real", [Real]) | ("bool", [Bool]) | ("text", [Text]) => arg(0),
            ("int", [Bool]) => fb.emit(Type::I64, Op::BoolToI64(arg(0))),
            ("int", [Real]) => fb.emit(Type::I64, Op::F64ToI64(arg(0))),
            ("real", [Int]) => fb.emit(Type::F64, Op::I64ToF64(arg(0))),
            ("bool", [Int]) => {
                let zero = fb.i64(0);
                fb.int_cmp(CmpOp::Ne, arg(0), zero)
            }
            ("not", [Int]) => {
                let ones = fb.i64(-1);
                fb.int_bin(IntBinOp::Xor, arg(0), ones)
            }
            ("not", [Bool]) => fb.emit(Type::Bool, Op::BoolNot(arg(0))),
            ("chr", [Int]) => {
                let mask = fb.i64(0xFF);
                let byte = fb.int_bin(IntBinOp::And, arg(0), mask);
                let text = fb.alloc_const(ITEMS_OFFSET + SLOT);
                let one = fb.i64(1);
                fb.store(text, LEN_OFFSET, one);
                fb.store_u8(text, ITEMS_OFFSET, byte);
                text
            }
            ("print", _) => {
                for (val, t) in vals.iter().zip(&arg_types) {
                    let val = val.unwrap();
                    let op = match t {
                        Int => Op::PrintInt(val),
                        Real => Op::PrintReal(val),
                        Bool => Op::PrintInt(fb.emit(Type::I64, Op::BoolToI64(val))),
                        Text => {
                            let offset = fb.i64(ITEMS_OFFSET as i64);
                            Op::PrintText(fb.addr_add(val, offset))
                        }
                        _ => unreachable!()
                    };
                    fb.push(vec![], op);
                }
                return None;
            }
            ("len", [Text | Array { .. }]) => fb.load(Type::I64, arg(0), LEN_OFFSET),
            ("get", [Text, Int]) => {
                let (text, i) = (arg(0), arg(1));
                let len = fb.load(Type::I64, text, LEN_OFFSET);
                fb.bounds_check(i, len);
                let addr = fb.addr_add(text, i);
                fb.load_u8(addr, ITEMS_OFFSET)
            }
            ("get", [Array { item }, Int]) => {
                let (array, i) = (arg(0), arg(1));
                let len = fb.load(Type::I64, array, LEN_OFFSET);
                fb.bounds_check(i, len);
                let ty = repr(item)?;
                let slot = fb.i64(SLOT as i64);
                let offset = fb.int_bin(IntBinOp::Mul, i, slot);
                let addr = fb.addr_add(array, offset);
                fb.load(ty, addr, ITEMS_OFFSET)
            }
            ("set", [Array { .. }, Int, _]) => {
                let (array, i) = (arg(0), arg(1));
                let len = fb.load(Type::I64, array, LEN_OFFSET);
                fb.bounds_check(i, len);
                if let Some(value) = vals[2] {
                    let slot = fb.i64(SLOT as i64);
                    let offset = fb.int_bin(IntBinOp::Mul, i, slot);
                    let addr = fb.addr_add(array, offset);
                    fb.store(addr, ITEMS_OFFSET, value);
                }
                return None;
            }
            _ => unreachable!("no lowering for builtin {} {:?}", op, arg_types)
        })
    }

    fn gen_value(&mut self, value: Value, f: &mut FnCtx) -> Option<usize> {
        match value {
            Value::Int { value, .. } => Some(f.fb.i64(value)),
            Value::Real { value, .. } => Some(f.fb.f64(value)),
            Value::Bool { value, .. } => Some(f.fb.bool(value)),
            Value::Text { value, .. } => {
                let id = self.mb.text(&value);
                Some(f.fb.emit(Type::Addr, Op::DataAddr(id)))
            }
            Value::Var { name, .. } => self.gen_var(name, f),
            Value::Init { name, value, .. } | Value::Assign { name, value, .. } => {
                let val = self.gen_value(*value, f);
                f.env.insert(name, val);
                None
            }
            Value::Record { fields, tpe } => self.gen_record(fields, tpe, f),
            Value::Access { object, field, .. } => self.gen_access(*object, field, f),
            Value::Call { func, args, .. } => self.gen_call(*func, args, f),
            Value::If { cond, then, elsë, tpe } => self.gen_if(*cond, *then, *elsë, tpe, f),
            Value::While { cond, body, .. } => self.gen_while(*cond, *body, f),
            Value::For { key, value, expr, body, .. } => self.gen_for(key, value, *expr, *body, f),
            Value::Lambda { args, ret, body, .. } => self.gen_lambda(args, ret, *body, f),
            Value::Builtin { op, args, tpe } => self.gen_builtin(op, args, tpe, f),
            Value::Block { values, .. } => {
                let mut last = None;
                for value in values {
                    last = self.gen_value(value, f);
                }
                last
            }
        }
    }
}

pub fn generate(main_name: &str, globals: HashMap<String, Value>) -> Module {
    let mut lowerer = Lowerer { mb: ModuleBuilder::new(), globals: HashMap::new() };

    let mut funcs = vec![];
    for (name, value) in sorted(globals, |(n, _)| n) {
        let global = match value {
            Value::Int { value, .. } => Global::Int(value),
            Value::Real { value, .. } => Global::Real(value),
            Value::Text { value, .. } => Global::Text(value),
            Value::Bool { value, .. } => Global::Bool(value),
            Value::Lambda { args, ret, body, .. } => {
                let id = lowerer.mb.reserve_func();
                funcs.push((id, name.clone(), args, ret, *body));
                Global::Func(id)
            }
            _ => unreachable!("global must be a literal or a function")
        };
        lowerer.globals.insert(name, global);
    }

    for (id, name, args, ret, body) in funcs {
        let func = lowerer.gen_function(name, args, ret, vec![], body);
        lowerer.mb.define_func(id, func);
    }

    let main = match lowerer.globals[main_name] {
        Global::Func(id) => id,
        _ => unreachable!("main must be a function")
    };

    let start = Signature { params: vec![], results: vec![] };
    let mut fb = FunctionBuilder::new("_start".into(), start);
    runtime::init_heap(&mut fb);
    let null = fb.emit(Type::Addr, Op::NullAddr);
    let results = lowerer.mb.func_sig(main).results;
    fb.call(main, vec![null], &results);
    let entry = lowerer.mb.add_func(fb.finish(vec![]));

    lowerer.mb.finish(entry)
}
