use std::collections::HashMap;

use crate::frontend;
use crate::midend::ir::{Signature, Type};
use crate::midend::utils::sorted;

pub const SLOT: u32 = 8;

pub const LEN_OFFSET: u32 = 0;
pub const ITEMS_OFFSET: u32 = 8;

pub const CODE_OFFSET: u32 = 0;
pub const CAPTURES_OFFSET: u32 = 8;

pub const MAX_ARRAY_LEN: i64 = 0x0FFF_FFFF;

pub fn is_erased(t: &frontend::Type) -> bool {
    match t {
        frontend::Type::Record { fields } => fields.values().all(is_erased),
        _ => false
    }
}

pub fn repr(t: &frontend::Type) -> Option<Type> {
    match t {
        frontend::Type::Int => Some(Type::I64),
        frontend::Type::Real => Some(Type::F64),
        frontend::Type::Bool => Some(Type::Bool),
        frontend::Type::Text | frontend::Type::Array { .. } | frontend::Type::Func { .. } => Some(Type::Addr),
        frontend::Type::Record { .. } if is_erased(t) => None,
        frontend::Type::Record { .. } => Some(Type::Addr)
    }
}

pub fn record_slots(fields: &HashMap<String, frontend::Type>) -> Vec<(String, Type)> {
    sorted(fields, |(n, _)| *n).into_iter().filter_map(|(name, t)| {
        repr(t).map(|ty| (name.clone(), ty))
    }).collect()
}

pub fn record_fields(t: &frontend::Type) -> &HashMap<String, frontend::Type> {
    match t {
        frontend::Type::Record { fields } => fields,
        _ => unreachable!()
    }
}

pub fn slot_offset(i: usize) -> u32 {
    i as u32 * SLOT
}

pub fn func_sig(args: &[frontend::Type], ret: &frontend::Type) -> Signature {
    Signature {
        params: std::iter::once(Type::Addr).chain(args.iter().filter_map(repr)).collect(),
        results: repr(ret).into_iter().collect()
    }
}

pub fn encode_text(text: &str) -> Vec<u8> {
    let mut bytes = (text.len() as i64).to_le_bytes().to_vec();
    bytes.extend(text.as_bytes());
    bytes.push(0);
    bytes
}
