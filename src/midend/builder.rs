use std::collections::HashMap;

use crate::midend::ir::*;
use crate::midend::layout::encode_text;
use crate::midend::runtime;

pub struct FunctionBuilder {
    name: String,
    sig: Signature,
    params: Vec<usize>,
    types: Vec<Type>,
    open: Vec<Vec<Instr>> // Instructions of the blocks currently being built, innermost last
}

impl FunctionBuilder {
    pub fn new(name: String, sig: Signature) -> FunctionBuilder {
        let mut fb = FunctionBuilder {
            name, sig: sig.clone(), params: vec![], types: vec![], open: vec![vec![]]
        };
        fb.params = fb.new_vals(&sig.params);
        fb
    }

    pub fn params(&self) -> Vec<usize> {
        self.params.clone()
    }

    pub fn type_of(&self, v: usize) -> Type {
        self.types[v]
    }

    pub fn types_of(&self, vs: &[usize]) -> Vec<Type> {
        vs.iter().map(|v| self.type_of(*v)).collect()
    }

    pub fn new_val(&mut self, ty: Type) -> usize {
        self.types.push(ty);
        self.types.len() - 1
    }

    pub fn new_vals(&mut self, tys: &[Type]) -> Vec<usize> {
        tys.iter().map(|ty| self.new_val(*ty)).collect()
    }

    pub fn push(&mut self, results: Vec<usize>, op: Op) {
        self.open.last_mut().unwrap().push(Instr { results, op });
    }

    // Emits an instruction with a single result of type `ty`
    pub fn emit(&mut self, ty: Type, op: Op) -> usize {
        let v = self.new_val(ty);
        self.push(vec![v], op);
        v
    }

    pub fn begin_block(&mut self) {
        self.open.push(vec![]);
    }

    pub fn end_block(&mut self, yields: Vec<usize>) -> Block {
        Block { instrs: self.open.pop().unwrap(), yields }
    }

    pub fn finish(mut self, yields: Vec<usize>) -> Function {
        assert_eq!(self.open.len(), 1, "unclosed blocks in {}", self.name);
        let body = self.end_block(yields);
        Function { name: self.name, sig: self.sig, params: self.params, body, types: self.types }
    }

    // Emits an `If` whose arms yield values of the types of `then.yields`
    pub fn if_(&mut self, cond: usize, then: Block, elsë: Block) -> Vec<usize> {
        let results = self.new_vals(&self.types_of(&then.yields));
        self.push(results.clone(), Op::If { cond, then, elsë });
        results
    }

    // Emits a `Loop` over `params` whose results have the params' types
    pub fn loop_(&mut self, params: Vec<usize>, inits: Vec<usize>, body: Block) -> Vec<usize> {
        let results = self.new_vals(&self.types_of(&params));
        self.push(results.clone(), Op::Loop { params, inits, body });
        results
    }

    pub fn i64(&mut self, value: i64) -> usize {
        self.emit(Type::I64, Op::ConstI64(value))
    }

    pub fn f64(&mut self, value: f64) -> usize {
        self.emit(Type::F64, Op::ConstF64(value))
    }

    pub fn bool(&mut self, value: bool) -> usize {
        self.emit(Type::Bool, Op::ConstBool(value))
    }

    pub fn int_bin(&mut self, op: IntBinOp, a: usize, b: usize) -> usize {
        self.emit(Type::I64, Op::IntBin(op, a, b))
    }

    pub fn int_cmp(&mut self, op: CmpOp, a: usize, b: usize) -> usize {
        self.emit(Type::Bool, Op::IntCmp(op, a, b))
    }

    pub fn bool_bin(&mut self, op: BoolBinOp, a: usize, b: usize) -> usize {
        self.emit(Type::Bool, Op::BoolBin(op, a, b))
    }

    pub fn addr_add(&mut self, addr: usize, offset: usize) -> usize {
        self.emit(Type::Addr, Op::AddrAdd(addr, offset))
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        self.emit(Type::Addr, Op::Alloc(size))
    }

    pub fn alloc_const(&mut self, size: u32) -> usize {
        let size = self.i64(size as i64);
        self.alloc(size)
    }

    pub fn load(&mut self, ty: Type, addr: usize, offset: u32) -> usize {
        self.emit(ty, Op::Load { ty, addr, offset })
    }

    pub fn store(&mut self, addr: usize, offset: u32, value: usize) {
        let ty = self.type_of(value);
        self.push(vec![], Op::Store { ty, addr, offset, value });
    }

    pub fn load_u8(&mut self, addr: usize, offset: u32) -> usize {
        self.emit(Type::I64, Op::LoadU8 { addr, offset })
    }

    pub fn store_u8(&mut self, addr: usize, offset: u32, value: usize) {
        self.push(vec![], Op::StoreU8 { addr, offset, value });
    }

    pub fn call(&mut self, func: usize, args: Vec<usize>, results: &[Type]) -> Vec<usize> {
        let results = self.new_vals(results);
        self.push(results.clone(), Op::Call { func, args });
        results
    }

    // -1, 0 or 1 depending on how `a` compares to `b`
    pub fn sign_of_cmp(&mut self, a: usize, b: usize) -> usize {
        let gt = self.int_cmp(CmpOp::Gt, a, b);
        let lt = self.int_cmp(CmpOp::Lt, a, b);
        let gt = self.emit(Type::I64, Op::BoolToI64(gt));
        let lt = self.emit(Type::I64, Op::BoolToI64(lt));
        self.int_bin(IntBinOp::Sub, gt, lt)
    }

    // Traps unless `cond` holds
    pub fn trap_unless(&mut self, cond: usize) {
        self.begin_block();
        let then = self.end_block(vec![]);
        self.begin_block();
        self.push(vec![], Op::Abort);
        let elsë = self.end_block(vec![]);
        self.if_(cond, then, elsë);
    }

    // Traps unless 0 <= i < len
    pub fn bounds_check(&mut self, i: usize, len: usize) {
        let zero = self.i64(0);
        let non_negative = self.int_cmp(CmpOp::Ge, i, zero);
        let below = self.int_cmp(CmpOp::Lt, i, len);
        let ok = self.bool_bin(BoolBinOp::And, non_negative, below);
        self.trap_unless(ok);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Helper {
    TextConcat,
    TextCompare
}

pub struct ModuleBuilder {
    funcs: Vec<Option<Function>>,
    data: Vec<Data>,
    // Data index of every text literal
    texts: HashMap<String, usize>,
    // Function index of every emitted helper
    helpers: HashMap<Helper, usize>
}

impl ModuleBuilder {
    pub fn new() -> ModuleBuilder {
        ModuleBuilder { funcs: vec![], data: vec![], texts: HashMap::new(), helpers: HashMap::new() }
    }

    pub fn reserve_func(&mut self) -> usize {
        self.funcs.push(None);
        self.funcs.len() - 1
    }

    pub fn define_func(&mut self, id: usize, func: Function) {
        self.funcs[id] = Some(func);
    }

    pub fn add_func(&mut self, func: Function) -> usize {
        let id = self.reserve_func();
        self.define_func(id, func);
        id
    }

    pub fn func_sig(&self, id: usize) -> Signature {
        self.funcs[id].as_ref().expect("function not defined yet").sig.clone()
    }

    pub fn text(&mut self, text: &str) -> usize {
        if let Some(id) = self.texts.get(text) {
            return *id;
        }
        self.data.push(Data { bytes: encode_text(text) });
        let id = self.data.len() - 1;
        self.texts.insert(text.to_string(), id);
        id
    }

    pub fn helper(&mut self, helper: Helper) -> usize {
        if let Some(id) = self.helpers.get(&helper) {
            return *id;
        }
        let func = match helper {
            Helper::TextConcat => runtime::text_concat(),
            Helper::TextCompare => runtime::text_compare()
        };
        let id = self.add_func(func);
        self.helpers.insert(helper, id);
        id
    }

    pub fn finish(self, entry: usize) -> Module {
        Module {
            funcs: self.funcs.into_iter().map(|f| f.expect("function reserved but not defined")).collect(),
            data: self.data,
            main_fn: entry
        }
    }
}
