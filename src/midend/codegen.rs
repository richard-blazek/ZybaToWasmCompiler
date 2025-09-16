use std::collections::{HashMap, HashSet};

use crate::frontend::{self, void, Value};
use crate::midend::{globals::Globals, locals::Locals};
use crate::midend::ir::*;
use crate::midend::utils::*;

fn collect_vars(v: &Value) -> (HashSet<String>, HashSet<String>) {
    match v {
        Value::Record { fields, .. } => {
            let mut used = HashSet::new();
            let mut defined = HashSet::new();
            for (_, value) in fields {
                let (new_used, new_defined) = collect_vars(value);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            (used, defined)
        }
        Value::Var { name, .. } => {
            (HashSet::from_iter([name.clone()]), HashSet::new())
        }
        Value::Call { func, args, .. } => {
            let (mut used, mut defined) = collect_vars(func);
            for arg in args {
                let (new_used, new_defined) = collect_vars(arg);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            (used, defined)
        }
        Value::Builtin { args, .. } => {
            let mut used = HashSet::new();
            let mut defined = HashSet::new();
            for arg in args {
                let (new_used, new_defined) = collect_vars(arg);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            (used, defined)
        }
        Value::Access { object, .. } => {
            collect_vars(&**object)
        }
        Value::Lambda { args, body, .. } => {
            let (used, mut defined) = collect_vars(body);
            defined.extend(args.iter().map(|(n, _)| n.clone()));
            (used, defined)
        }
        Value::Init { name, value, .. } => {
            let (used, mut defined) = collect_vars(value);
            defined.insert(name.clone());
            (used, defined)
        }
        Value::Assign { value, .. } => {
            collect_vars(value)
        }
        Value::If { cond, then, elsë, .. } => {
            let (mut used, mut defined) = collect_vars(cond);
            let (then_used, then_defined) = collect_vars(then);
            let (else_used, else_defined) = collect_vars(elsë);
            used.extend(then_used);
            used.extend(else_used);
            defined.extend(then_defined);
            defined.extend(else_defined);
            (used, defined)
        }
        Value::While { cond, body, .. } => {
            let (mut used, mut defined) = collect_vars(cond);
            let (new_used, new_defined) = collect_vars(body);
            used.extend(new_used);
            defined.extend(new_defined);
            (used, defined)
        }
        Value::For { key, value, expr, body, .. } => {
            let (mut used, mut defined) = collect_vars(expr);
            let (new_used, new_defined) = collect_vars(body);
            used.extend(new_used);
            defined.extend(new_defined);
            defined.extend([key.clone(), value.clone()]);
            (used, defined)
        }
        Value::Block { values, .. } => {
            let mut used = HashSet::new();
            let mut defined = HashSet::new();
            for value in values {
                let (new_used, new_defined) = collect_vars(value);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            (used, defined)
        }
        _ => {
            (HashSet::new(), HashSet::new())
        }
    }
}

fn collect_captures(body: &Value, args: HashSet<String>, g: &mut Globals) -> Vec<String> {
    let (mut used, defined) = collect_vars(body);
    used.retain(|x| {
        !defined.contains(x) && !args.contains(x) && !g.contains(x)
    });
    Vec::from_iter(used)
}

macro_rules! cat {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.extend($x);
            )*
            temp_vec
        }
    };
}

fn gen_int(value: i64) -> Vec<Instr> {
    vec![Instr::PushInt { value }]
}

fn gen_real(value: f64) -> Vec<Instr> {
    vec![Instr::PushReal { value }]
}

fn gen_text(value: String) -> Vec<Instr> {
    vec![Instr::PushText { value }]
}

fn gen_bool(value: bool) -> Vec<Instr> {
    vec![Instr::PushBool { value }]
}

fn gen_var(name: String, tpe: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    if g.contains(&name) {
        vec![g.fetch(&name)]
    } else {
        let tpe = Type::from(&tpe);
        let id = l.get_id(&name);
        vec![Instr::GetLocal { id, tpe }]
    }
}

fn gen_assign(name: String, value: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let tpe = Type::from(&value.tpe());
    let id = l.get_id(&name);
    cat!(
        gen_value(value, g, l),
        [Instr::SetLocal { id, tpe }],
        [Instr::NewTuple { fields: vec![] }]
    )
}

fn gen_init(name: String, value: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    l.define(name.clone(), Type::from(&value.tpe()));
    gen_assign(name, value, g, l)
}

fn gen_record(fields: Vec<(String, Value)>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let values: Vec<_> = fields.into_iter().map(|(_, v)| v).collect();
    let types = values.iter().map(|v| Type::from(&v.tpe())).collect();
    cat!(
        values.into_iter().flat_map(|v| gen_value(v, g, l)),
        [Instr::NewTuple { fields: types }]
    )
}

fn gen_call(func: Value, args: Vec<Value>, ret: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let args_t = args.iter().map(|a| Type::from(&a.tpe())).collect();
    let ret = Type::from(&ret);
    cat!(
        args.into_iter().flat_map(|v| gen_value(v, g, l)),
        gen_value(func, g, l),
        [Instr::CallFunc { args: args_t, ret }]
    )
}

fn gen_access(object: Value, field: String, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let fields = sorted(match object.tpe() {
        frontend::Type::Record { fields } => fields,
        _ => unreachable!(),
    }, |(n, _)| n);

    let i = fields.iter().position(|(n, _)| n == &field).unwrap();
    let types = fields.into_iter().map(|(_, v)| Type::from(&v)).collect();
    cat!(
        gen_value(object, g, l),
        [Instr::GetField { fields: types, i }]
    )
}

fn gen_if(cond: Value, then: Value, elsë: Value, tpe: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let (drop_then, drop_else) = if tpe == void() {
        (vec![Instr::Drop { tpe: Type::from(&then.tpe()) }, Instr::NewTuple { fields: vec![] }],
         vec![Instr::Drop { tpe: Type::from(&elsë.tpe()) }, Instr::NewTuple { fields: vec![] }])
    } else {
        (vec![], vec![])
    };

    cat!(
        gen_value(cond, g, l),
        [Instr::Ifte {
            then: cat!(gen_value(then, g, l), drop_then),
            elsë: cat!(gen_value(elsë, g, l), drop_else),
            ret: Type::from(&tpe)
        }]
    )
}

fn gen_while(cond: Value, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let id =  g.new_label();
    let tpe = Type::from(&body.tpe());

    vec![
        Instr::Loop { id, inner: cat!(
            gen_value(cond, g, l),
            [Instr::NotBool, Instr::QuitIf { id }],
            gen_value(body, g, l),
            [Instr::Drop { tpe }],
            [Instr::RepeatLoop { id }]
        ) },
        Instr::NewTuple { fields: vec![] }
    ]
}

fn gen_for(key: String, value: String, expr: Value, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let id = g.new_label();

    let arr_t = Type::from(&expr.tpe());
    let val_t = arr_t.array_item();

    let key_id = l.define(key, Type::Int);
    let val_id = l.define(value, val_t.clone());
    let expr_id = l.alloc(arr_t.clone());

    cat!(
        // Init
        gen_value(expr, g, l),
        [Instr::SetLocal { id: expr_id, tpe: arr_t.clone() }],
        [Instr::PushInt { value: 0 }],
        [Instr::SetLocal { id: key_id, tpe: Type::Int }],
        [Instr::Loop { id, inner: cat!(
            // Cond
            [Instr::GetLocal { id: key_id, tpe: Type::Int }],
            [Instr::GetLocal { id: expr_id, tpe: arr_t.clone() }],
            [Instr::LenArray { item: val_t.clone() }],
            [Instr::EqInt],
            [Instr::QuitIf { id }],

            // Get array[i]
            [Instr::GetLocal { id: expr_id, tpe: arr_t.clone() }],
            [Instr::GetLocal { id: key_id, tpe: Type::Int }],
            [Instr::GetArray { item: val_t.clone() }],
            [Instr::SetLocal { id: val_id, tpe: val_t.clone() }],

            // Body
            gen_value(body, g, l),

            // Increment
            [Instr::GetLocal { id: key_id, tpe: Type::Int }],
            [Instr::PushInt { value: 1 }],
            [Instr::AddInt],
            [Instr::SetLocal { id: key_id, tpe: Type::Int }],
            [Instr::RepeatLoop { id }]
        )  }],
        [Instr::NewTuple { fields: vec![] }]
    )
}

fn gen_lambda(args: Vec<(String, frontend::Type)>, ret: frontend::Type, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let captures = collect_captures(&body, args.iter().map(|(n, _)| {
        n.clone()
    }).collect(), g);

    let mut inner = Locals::new();

    let args = args.into_iter().map(|(name, tpe)| {
        let tpe = Type::from(&tpe);
        inner.define(name, tpe.clone());
        tpe
    }).collect::<Vec<_>>();

    let mut captures_bind = vec![];
    let mut captures_open = vec![];
    for name in captures {
        let tpe = l.get_type(&name);
        let outer_id = l.get_id(&name);
        let inner_id = inner.define(name, tpe.clone());
        captures_bind.push((outer_id, tpe.clone()));
        captures_open.push((inner_id, tpe));
    }

    let tpe = Type::from(&body.tpe());
    let ret_t = Type::from(&ret);
    let body = gen_value(body, g, &mut inner);

    let mut locals = inner.get_types();
    locals.drain(0..(args.len() + captures_open.len()));

    let func = Func {
        code: cat!(
            body,
            if ret == void() {
                vec![Instr::Drop { tpe }, Instr::NewTuple { fields: vec![] }]
            } else {
                vec![]
            }
        ),
        args: args.clone(),
        ret: ret_t.clone(),
        captures: captures_open,
        locals
    };

    vec![Instr::BindFunc {
        id: g.add_lambda(func),
        args,
        ret: ret_t,
        captures: captures_bind
    }]
}

fn first(args: Vec<Value>) -> Value {
    args.into_iter().next().unwrap()
}

fn first2(args: Vec<Value>) -> (Value, Value) {
    let mut it = args.into_iter();
    let fst = it.next().unwrap();
    let snd = it.next().unwrap();
    (fst, snd)
}

fn first3(args: Vec<Value>) -> (Value, Value, Value) {
    let mut it = args.into_iter();
    let fst = it.next().unwrap();
    let snd = it.next().unwrap();
    let thd = it.next().unwrap();
    (fst, snd, thd)
}

fn gen_binary_op(op: Instr, args: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let (lhs, rhs) = first2(args);
    cat!(
        gen_value(lhs, g, l),
        gen_value(rhs, g, l),
        [op]
    )
}

fn cmp_instr(op: &str, t: &frontend::Type) -> Instr {
    match (op, t) {
        ("==", frontend::Type::Int) => Instr::EqInt,
        ("<", frontend::Type::Int) => Instr::LtInt,
        ("==", frontend::Type::Real) => Instr::EqReal,
        ("<", frontend::Type::Real) => Instr::LtReal,
        ("==", frontend::Type::Text) => Instr::EqText,
        ("<", frontend::Type::Text) => Instr::LtText,
        _ => unreachable!()
    }
}

fn gen_cmp(op: &str, t: &frontend::Type, mut args: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let (_, instr, not) = match op {
        "==" => ({}, cmp_instr("==", t), vec![]),
        "!=" => ({}, cmp_instr("==", t), vec![Instr::NotBool]),
        "<" => ({}, cmp_instr("<", t), vec![]),
        ">=" => ({}, cmp_instr("<", t), vec![Instr::NotBool]),
        ">" => (args.reverse(), cmp_instr("<", t), vec![]),
        "<=" => (args.reverse(), cmp_instr("<", t), vec![Instr::NotBool]),
        _ => unreachable!(),
    };
    cat!(gen_binary_op(instr, args, g, l), not)
}

fn gen_builtin(op: String, args: Vec<Value>, tpe: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    use frontend::Type::*;

    let arg_types: Vec<_> = args.iter().map(Value::tpe).collect();
    match (op.as_str(), &arg_types[..]) {
        ("*", [Int, Int]) => {
            gen_binary_op(Instr::MulInt, args, g, l)
        }
        ("*", [Real, Real]) => {
            gen_binary_op(Instr::MulReal, args, g, l)
        }
        ("/", [Int, Int]) => {
            gen_binary_op(Instr::DivInt, args, g, l)
        }
        ("/", [Real, Real]) => {
            gen_binary_op(Instr::DivReal, args, g, l)
        }
        ("%", [Int, Int]) => {
            gen_binary_op(Instr::RemInt, args, g, l)
        }
        ("+", [Int, Int]) => {
            gen_binary_op(Instr::AddInt, args, g, l)
        }
        ("+", [Real, Real]) => {
            gen_binary_op(Instr::AddReal, args, g, l)
        }
        ("+", [Text, Text]) => {
            gen_binary_op(Instr::CatText, args, g, l)
        }
        ("-", [Int, Int]) => {
            gen_binary_op(Instr::SubInt, args, g, l)
        }
        ("-", [Real, Real]) => {
            gen_binary_op(Instr::SubReal, args, g, l)
        }
        ("&", [Int, Int]) => {
            gen_binary_op(Instr::AndInt, args, g, l)
        }
        ("&", [Bool, Bool]) => {
            gen_binary_op(Instr::AndBool, args, g, l)
        }
        ("|", [Int, Int]) => {
            gen_binary_op(Instr::OrInt, args, g, l)
        }
        ("|", [Bool, Bool]) => {
            gen_binary_op(Instr::OrBool, args, g, l)
        }
        ("^", [Int, Int]) => {
            gen_binary_op(Instr::XorInt, args, g, l)
        }
        ("^" | "!=", [Bool, Bool]) => {
            gen_binary_op(Instr::XorBool, args, g, l)
        }
        ("&&", [Bool, Bool]) => {
            let (lhs, rhs) = first2(args);
            gen_value(Value::If {
                cond: Box::new(lhs),
                then: Box::new(rhs),
                elsë: Box::new(Value::Bool { value: false, tpe: Bool }),
                tpe: Bool
            }, g, l)
        }
        ("||", [Bool, Bool]) => {
            let (lhs, rhs) = first2(args);
            gen_value(Value::If {
                cond: Box::new(lhs),
                then: Box::new(Value::Bool { value: true, tpe: Bool }),
                elsë: Box::new(rhs),
                tpe: Bool
            }, g, l)
        }
        ("==", [Bool, Bool]) => cat!(
            gen_binary_op(Instr::XorBool, args, g, l), [Instr::NotBool]
        ),
        ("==" | "!=" | "<" | "<=" | ">" | ">=", [t, _]) => {
            gen_cmp(&op, t, args, g, l)
        }
        ("int", [Int]) | ("real", [Real]) | ("bool", [Bool]) | ("text", [Text]) => {
            gen_value(first(args), g, l)
        }
        ("int", [Bool]) => gen_value(Value::If {
            cond: Box::new(first(args)),
            then: Box::new(Value::Bool { value: true, tpe: Bool }),
            elsë: Box::new(Value::Bool { value: true, tpe: Bool }),
            tpe: Bool
        }, g, l),
        ("int", [Real]) => cat!(
            gen_value(first(args), g, l), [Instr::RealToInt]
        ),
        ("real", [Int]) => cat!(
            gen_value(first(args), g, l), [Instr::IntToReal]
        ),
        ("bool", [Int]) => cat!(
            gen_value(first(args), g, l),
            [Instr::PushInt { value: 0 }],
            [Instr::EqInt],
            [Instr::NotBool]
        ),
        ("not", [Int]) => cat!(
            gen_value(first(args), g, l), [Instr::NotInt]
        ),
        ("not", [Bool]) => cat!(
            gen_value(first(args), g, l), [Instr::NotBool]
        ),
        ("chr", [Int]) => cat!(
            gen_value(first(args), g, l), [Instr::IntToTextAscii]
        ),
        ("print", _) => {
            args.into_iter().flat_map(|arg| {
                let tpe = arg.tpe();
                cat!(
                    gen_value(arg, g, l),
                    match tpe {
                        Int => [Instr::PrintInt],
                        Real => [Instr::PrintReal],
                        Text => [Instr::PrintText],
                        Bool => [Instr::PrintBool],
                        _ => unreachable!()
                    }
                )
            }).collect()
        }
        ("array", [Int]) => {
            let item_t = Type::from(&tpe).array_item();
            cat!(
                gen_value(first(args), g, l),
                [Instr::NewArray { item: item_t }]
            )
        }
        ("len", [Text]) => cat!(
            gen_value(first(args), g, l), [Instr::LenText]
        ),
        ("len", [Array { item }]) => cat!(
            gen_value(first(args), g, l),
            [Instr::LenArray { item: Type::from(item) }]
        ),
        ("get", [Text, Int]) => {
            gen_binary_op(Instr::GetText, args, g, l)
        }
        ("get", [Array { item }, Int]) => {
            let item_t = Type::from(&item);
            gen_binary_op(Instr::GetArray { item: item_t }, args, g, l)
        }
        ("set", [Array { item }, Int, _]) => {
            let (array, idx, val) = first3(args);
            cat!(
                gen_value(array, g, l),
                gen_value(idx, g, l),
                gen_value(val, g, l),
                [Instr::SetArray { item: Type::from(&item) }],
                [Instr::NewTuple { fields: vec![] }]
            )
        }
        _ => unreachable!()
    }
}

fn gen_block(vals: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let mut code = vec![];
    for val in vals {
        let tpe = val.tpe();
        code.extend(gen_value(val, g, l));
        code.push(Instr::Drop { tpe: Type::from(&tpe) });
    }
    code.pop();
    code
}

fn gen_value(value: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    use Value::*;

    match value {
        Int { value, .. } => gen_int(value),
        Real { value, .. } => gen_real(value),
        Text { value, .. } => gen_text(value),
        Bool { value, .. } => gen_bool(value),
        Var { name, tpe } => gen_var(name, tpe, g, l),
        Assign { name, value, .. } => gen_assign(name, *value, g, l),
        Init { name, value, .. } => gen_init(name, *value, g, l),
        Record { fields, .. } => gen_record(sorted(fields, |(n, _)| n), g, l),
        Call { func, args, tpe } => gen_call(*func, args, tpe, g, l),
        Access { object, field, .. } => gen_access(*object, field, g, l),
        If { cond, then, elsë, tpe } => gen_if(*cond, *then, *elsë, tpe, g, l),
        While { cond, body, .. } => gen_while(*cond, *body, g, l),
        Lambda { args, ret, body, .. } => gen_lambda(args, ret, *body, g, l),
        For { key, value, expr, body, .. } => gen_for(key, value, *expr, *body, g, l),
        Builtin { op, args, tpe } => gen_builtin(op, args, tpe, g, l),
        Block { values, .. } => gen_block(values, g, l)
    }
}

pub fn generate(main_name: &str, globals: HashMap<String, Value>) -> Program {
    let mut g = Globals::new(&globals);

    for (name, value) in globals {
        match value {
            Value::Lambda { args, body, ret, .. } => {
                let mut l = Locals::new();
                let mut arg_types = vec![];
                let args_len = args.len();

                for (arg_name, arg_tpe) in args {
                    let tpe = Type::from(&arg_tpe);
                    l.define(arg_name, tpe.clone());
                    arg_types.push(tpe);
                }

                let code = gen_value(*body, &mut g, &mut l);
                let mut locals = l.get_types();
                locals.drain(0..args_len);

                let func = Func {
                    code,
                    args: arg_types,
                    ret: Type::from(&ret),
                    captures: vec![],
                    locals
                };
                g.set_func(g.func_id(&name), func);
            }
            _ => {}
        }
    }

    let main_id = g.func_id(main_name);
    Program { funcs: g.to_funcs(), entry: main_id }
}
