// Runtime helpers written in the IR

use crate::midend::builder::FunctionBuilder;
use crate::midend::ir::*;
use crate::midend::layout::{ITEMS_OFFSET, LEN_OFFSET};

fn copy_bytes(fb: &mut FunctionBuilder, src: usize, dst: usize, len: usize) {
    let zero = fb.i64(0);
    let i = fb.new_val(Type::I64);

    fb.begin_block();
    let more = fb.int_cmp(CmpOp::Lt, i, len);
    fb.begin_block();
    let from = fb.addr_add(src, i);
    let byte = fb.load_u8(from, ITEMS_OFFSET);
    let to = fb.addr_add(dst, i);
    fb.store_u8(to, ITEMS_OFFSET, byte);
    let one = fb.i64(1);
    let next = fb.int_bin(IntBinOp::Add, i, one);
    let then = fb.end_block(vec![next]);
    fb.begin_block();
    let elsë = fb.end_block(vec![i]);
    let next = fb.if_(more, then, elsë)[0];
    let body = fb.end_block(vec![more, next]);

    fb.loop_(vec![i], vec![zero], body);
}

pub fn text_concat() -> Function {
    let sig = Signature { params: vec![Type::Addr, Type::Addr], results: vec![Type::Addr] };
    let mut fb = FunctionBuilder::new("text_concat".into(), sig);
    let (a, b) = (fb.params()[0], fb.params()[1]);

    let len_a = fb.load(Type::I64, a, LEN_OFFSET);
    let len_b = fb.load(Type::I64, b, LEN_OFFSET);
    let len = fb.int_bin(IntBinOp::Add, len_a, len_b);
    // Length, bytes and the NUL terminator, which alloc already zeroed
    let extra = fb.i64(ITEMS_OFFSET as i64 + 1);
    let size = fb.int_bin(IntBinOp::Add, len, extra);
    let result = fb.alloc(size);
    fb.store(result, LEN_OFFSET, len);

    copy_bytes(&mut fb, a, result, len_a);
    let result_b = fb.addr_add(result, len_a);
    copy_bytes(&mut fb, b, result_b, len_b);

    fb.finish(vec![result])
}

pub fn text_compare() -> Function {
    let sig = Signature { params: vec![Type::Addr, Type::Addr], results: vec![Type::I64] };
    let mut fb = FunctionBuilder::new("text_compare".into(), sig);
    let (a, b) = (fb.params()[0], fb.params()[1]);

    let len_a = fb.load(Type::I64, a, LEN_OFFSET);
    let len_b = fb.load(Type::I64, b, LEN_OFFSET);

    let a_shorter = fb.int_cmp(CmpOp::Lt, len_a, len_b);
    fb.begin_block();
    let then = fb.end_block(vec![len_a]);
    fb.begin_block();
    let elsë = fb.end_block(vec![len_b]);
    let common = fb.if_(a_shorter, then, elsë)[0];

    // Carries the index and the result found so far
    let zero = fb.i64(0);
    let i = fb.new_val(Type::I64);
    let result = fb.new_val(Type::I64);

    fb.begin_block();
    let done = fb.int_cmp(CmpOp::Ge, i, common);

    fb.begin_block();
    let stop = fb.bool(false);
    let by_len = fb.sign_of_cmp(len_a, len_b);
    let at_end = fb.end_block(vec![stop, i, by_len]);

    fb.begin_block();
    let at_a = fb.addr_add(a, i);
    let byte_a = fb.load_u8(at_a, ITEMS_OFFSET);
    let at_b = fb.addr_add(b, i);
    let byte_b = fb.load_u8(at_b, ITEMS_OFFSET);
    let differ = fb.int_cmp(CmpOp::Ne, byte_a, byte_b);

    fb.begin_block();
    let stop = fb.bool(false);
    let by_byte = fb.sign_of_cmp(byte_a, byte_b);
    let differs = fb.end_block(vec![stop, i, by_byte]);

    fb.begin_block();
    let go_on = fb.bool(true);
    let one = fb.i64(1);
    let next = fb.int_bin(IntBinOp::Add, i, one);
    let same = fb.end_block(vec![go_on, next, result]);

    let step = fb.if_(differ, differs, same);
    let compare = fb.end_block(step);

    let step = fb.if_(done, at_end, compare);
    let body = fb.end_block(step);

    let results = fb.loop_(vec![i, result], vec![zero, zero], body);
    fb.finish(vec![results[1]])
}
