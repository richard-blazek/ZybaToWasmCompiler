#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    F64,
    Addr,
    Bool
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<Type>,
    pub results: Vec<Type>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBinOp { Add, Sub, Mul, DivS, RemS, And, Or, Xor, Shl, ShrS, ShrU }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RealBinOp { Add, Sub, Mul, Div }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoolBinOp { And, Or, Xor, Eq }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp { Eq, Ne, Lt, Le, Gt, Ge }

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    ConstI64(i64),
    ConstF64(f64),
    ConstBool(bool),
    NullAddr,

    // Address of the static data with the given index
    DataAddr(usize),

    // Code address of the function with the given index
    FuncAddr(usize),

    IntBin(IntBinOp, usize, usize),
    IntCmp(CmpOp, usize, usize),
    RealBin(RealBinOp, usize, usize),
    RealCmp(CmpOp, usize, usize),
    BoolBin(BoolBinOp, usize, usize),
    BoolNot(usize),

    I64ToF64(usize),
    F64ToI64(usize),
    BoolToI64(usize),

    AddrAdd(usize, usize), // addr + i64 byte offset
    AddrToI64(usize),
    HeapBegin,
    EnsureMemory(usize),

    Load { ty: Type, addr: usize, offset: u32 },
    Store { ty: Type, addr: usize, offset: u32, value: usize },
    LoadU8 { addr: usize, offset: u32 },
    StoreU8 { addr: usize, offset: u32, value: usize },

    // `func` is a function index
    Call { func: usize, args: Vec<usize> },

    // `callee` is a code address produced by FuncAddr
    CallIndirect { callee: usize, sig: Signature, args: Vec<usize> },

    PrintInt(usize),
    PrintReal(usize),
    PrintText(usize),

    Abort,
    If { cond: usize, then: Block, elsë: Block },

    // A do-while loop. `params` are defined inside `body` and initialised
    // from `inits`. The body yields a bool followed by values of the params'
    // types: if the bool is true, they become the params of the next
    // iteration, otherwise the results of the instruction.
    Loop { params: Vec<usize>, inits: Vec<usize>, body: Block }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instr {
    pub results: Vec<usize>,
    pub op: Op
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub yields: Vec<usize>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub sig: Signature,
    pub params: Vec<usize>,
    pub body: Block,
    pub types: Vec<Type>
}

impl Function {
    pub fn type_of(&self, v: usize) -> Type {
        self.types[v]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub bytes: Vec<u8>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub funcs: Vec<Function>,
    pub data: Vec<Data>,
    pub main_fn: usize
}
