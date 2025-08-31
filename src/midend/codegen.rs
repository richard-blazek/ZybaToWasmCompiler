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

    let mut code = gen_value(value, g, l);
    code.push(Instr::SetLocal { id, tpe });
    code.push(Instr::NewTuple { fields: vec![] });
    code
}

fn gen_init(name: String, value: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    l.define(name.clone(), Type::from(&value.tpe()));
    gen_assign(name, value, g, l)
}

fn gen_record(fields: Vec<(String, Value)>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let values: Vec<_> = fields.into_iter().map(|(_, v)| v).collect();
    let types = values.iter().map(|v| Type::from(&v.tpe())).collect();

    values.into_iter().flat_map(|v| {
        gen_value(v, g, l)
    }).chain([Instr::NewTuple { fields: types }]).collect()
}

fn gen_call(func: Value, args: Vec<Value>, ret: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let args_t = args.iter().map(|a| Type::from(&a.tpe())).collect();
    let ret = Type::from(&ret);

    let mut code: Vec<_> = args.into_iter().flat_map(|v| {
        gen_value(v, g, l)
    }).collect();
    code.extend(gen_value(func, g, l));
    code.push(Instr::CallFunc { args: args_t, ret });
    code
}

fn gen_access(object: Value, field: String, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let fields = sorted(match object.tpe() {
        frontend::Type::Record { fields } => fields,
        _ => unreachable!(),
    }, |(n, _)| n);

    let i = fields.iter().position(|(n, _)| n == &field).unwrap();
    let types = fields.into_iter().map(|(_, v)| Type::from(&v)).collect();

    let mut code = gen_value(object, g, l);
    code.push(Instr::GetField { fields: types, i });
    code
}

fn gen_if(cond: Value, then: Value, elsë: Value, tpe: frontend::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let if_not = g.new_label();
    let end_if = g.new_label();

    let (then_t, elsë_t) = (then.tpe(), elsë.tpe());
    let drop = tpe == void() && (then_t != void() || elsë_t != void());

    let mut code = gen_value(cond, g, l);
    code.push(Instr::JumpUnless { id: if_not });
    code.extend(gen_value(then, g, l));

    if drop {
        code.push(Instr::Drop { tpe: Type::from(&then_t) });
    }
    code.push(Instr::JumpAlways { id: end_if });
    code.push(Instr::Label { id: if_not });
    code.extend(gen_value(elsë, g, l));

    if drop {
        code.push(Instr::Drop { tpe: Type::from(&elsë_t) });
    }
    code.push(Instr::Label { id: end_if });

    if drop {
        code.push(Instr::NewTuple { fields: vec![] });
    }
    code
}

fn gen_while(cond: Value, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let leave = g.new_label();
    let start = g.new_label();

    let body_t = body.tpe();

    let mut code = vec![Instr::Label { id: start }];
    code.extend(gen_value(cond, g, l));

    code.push(Instr::JumpUnless { id: leave });
    code.extend(gen_value(body, g, l));
    code.push(Instr::Drop { tpe: Type::from(&body_t) });
    code.push(Instr::JumpAlways { id: start });
    code.push(Instr::Label { id: leave });
    code.push(Instr::NewTuple { fields: vec![] });
    code
}

fn gen_for(key: String, value: String, expr: Value, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let leave = g.new_label();
    let start = g.new_label();

    let arr_t = Type::from(&expr.tpe());
    let val_t = arr_t.array_item();

    let key_id = l.define(key, Type::Int);
    let val_id = l.define(value, val_t.clone());
    let expr_id = l.alloc(arr_t.clone());

    // Init
    let mut code = gen_value(expr, g, l);
    code.push(Instr::SetLocal { id: expr_id, tpe: arr_t.clone() });
    code.push(Instr::PushInt { value: 0 });
    code.push(Instr::SetLocal { id: key_id, tpe: Type::Int });

    // Cond
    code.push(Instr::Label { id: start });
    code.push(Instr::GetLocal { id: key_id, tpe: Type::Int });
    code.push(Instr::GetLocal { id: expr_id, tpe: arr_t.clone() });
    code.push(Instr::LenArray { item: val_t.clone() });
    code.push(Instr::LtInt);
    code.push(Instr::JumpUnless { id: leave });

    // Get array[i]
    code.push(Instr::GetLocal { id: expr_id, tpe: arr_t.clone() });
    code.push(Instr::GetLocal { id: key_id, tpe: Type::Int });
    code.push(Instr::GetArray { item: val_t.clone() });
    code.push(Instr::SetLocal { id: val_id, tpe: val_t.clone() });

    // Block
    code.extend(gen_value(body, g, l));

    // Increment
    code.push(Instr::GetLocal { id: key_id, tpe: Type::Int });
    code.push(Instr::PushInt { value: 1 });
    code.push(Instr::AddInt);
    code.push(Instr::SetLocal { id: key_id, tpe: Type::Int });

    // End
    code.push(Instr::Label { id: leave });
    code
}

fn gen_lambda(args: Vec<(String, frontend::Type)>, ret: frontend::Type, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let captures = collect_captures(&body, args.iter().map(|(n, _)| {
        n.clone()
    }).collect(), g);

    let mut inner = Locals::new();
    let mut code = vec![];

    let mut capture_types = vec![];
    for capture in captures {
        let tpe = l.get_type(&capture);
        let id = inner.define(capture, tpe.clone());

        capture_types.push(tpe.clone());
        code.push(Instr::SetLocal { id, tpe });
    }

    let mut arg_types = vec![];
    for (arg, tpe) in args {
        let tpe = Type::from(&tpe);
        let id = inner.define(arg, tpe.clone());

        arg_types.push(tpe.clone());
        code.push(Instr::SetLocal { id, tpe })
    }

    let body_t = body.tpe();
    code.extend(gen_value(body, g, &mut inner));

    if ret == void() && body_t != void() {
        code.push(Instr::Drop { tpe: Type::from(&body_t) });
        code.push(Instr::NewTuple { fields: vec![] });
    }

    vec![Instr::BindFunc {
        id: g.add_lambda(Func::new(code, inner.get_all())),
        args: arg_types,
        ret: Type::from(&ret),
        capture: capture_types
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
    let mut code = gen_value(lhs, g, l);
    code.extend(gen_value(rhs, g, l));
    code.push(op);
    code
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

    let mut code = gen_binary_op(instr, args, g, l);
    code.extend(not);
    code
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
        ("==", [Bool, Bool]) => {
            let mut code = gen_binary_op(Instr::XorBool, args, g, l);
            code.push(Instr::NotBool);
            code
        }
        ("==" | "!=" | "<" | "<=" | ">" | ">=", [t, _]) => {
            gen_cmp(&op, t, args, g, l)
        }
        ("int", [Int]) | ("real", [Real]) | ("bool", [Bool]) | ("text", [Text]) => {
            gen_value(first(args), g, l)
        }
        ("int", [Bool]) => {
            let value = Value::If {
                cond: Box::new(first(args)),
                then: Box::new(Value::Bool { value: true, tpe: Bool }),
                elsë: Box::new(Value::Bool { value: true, tpe: Bool }),
                tpe: Bool
            };
            gen_value(value, g, l)
        }
        ("int", [Real]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::RealToInt);
            code
        }
        ("real", [Int]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::IntToReal);
            code
        }
        ("bool", [Int]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::PushInt { value: 0 });
            code.push(Instr::EqInt);
            code.push(Instr::NotBool);
            code
        }
        ("not", [Int]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::NotInt);
            code
        }
        ("not", [Bool]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::NotBool);
            code
        }
        ("chr", [Int]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::IntToTextAscii);
            code
        }
        ("print", [Text]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::PrintText);
            code
        }
        ("array", [Int]) => {
            let item_t = Type::from(&tpe).array_item();

            let mut code = gen_value(first(args), g, l);
            code.push(Instr::NewArray { item: item_t });
            code
        }
        ("len", [Text]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::LenText);
            code
        }
        ("len", [Array { item }]) => {
            let mut code = gen_value(first(args), g, l);
            code.push(Instr::LenArray { item: Type::from(item) });
            code
        }
        ("get", [Text, Int]) => {
            gen_binary_op(Instr::GetText, args, g, l)
        }
        ("get", [Array { item }, Int]) => {
            let item_t = Type::from(&item);
            gen_binary_op(Instr::GetArray { item: item_t }, args, g, l)
        }
        ("set", [Array { item }, Int, _]) => {
            let (array, idx, val) = first3(args);

            let mut code = gen_value(array, g, l);
            code.extend(gen_value(idx, g, l));
            code.extend(gen_value(val, g, l));
            code.push(Instr::SetArray { item: Type::from(&item) });
            code
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
            Value::Lambda { args, body, .. } => {
                let mut l = Locals::new();
                let mut code = vec![];

                for (arg_name, arg_tpe) in args {
                    let tpe = Type::from(&arg_tpe);
                    let id = l.define(arg_name, tpe.clone());
                    code.push(Instr::SetLocal { id, tpe });
                }
                code.extend(gen_value(*body, &mut g, &mut l));

                let func = Func::new(code, l.get_all());
                g.set_func(g.func_id(&name), func);
            }
            _ => {}
        }
    }

    let main_id = g.func_id(main_name);
    Program::new(g.to_funcs(), main_id)
}
