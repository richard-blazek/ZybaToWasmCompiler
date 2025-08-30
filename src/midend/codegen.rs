use std::collections::{HashMap, HashSet};

use crate::builtin::{self, void};
use crate::midend::{globals::Globals, locals::Locals};
use crate::typecheck::Value;
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

fn gen_var(name: String, tpe: builtin::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
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

fn gen_call(func: Value, args: Vec<Value>, ret: builtin::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
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
        builtin::Type::Record { fields } => fields,
        _ => unreachable!(),
    }, |(n, _)| n);

    let i = fields.iter().position(|(n, _)| n == &field).unwrap();
    let types = fields.into_iter().map(|(_, v)| Type::from(&v)).collect();

    let mut code = gen_value(object, g, l);
    code.push(Instr::GetField { fields: types, i });
    code
}

fn gen_if(cond: Value, then: Value, elsë: Value, tpe: builtin::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
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

    let val_t = match expr.tpe() {
        builtin::Type::List { item } => Type::from(&*item),
        _ => unreachable!()
    };
    let expr_t = Type::Tuple(vec![
        Type::Array(Box::new(val_t.clone())),
        Type::Int
    ]);

    let key_id = l.define(key, Type::Int);
    let val_id = l.define(value, val_t.clone());
    let expr_id = l.alloc(expr_t.clone());

    // Init
    let mut code = gen_value(expr, g, l);
    code.push(Instr::SetLocal { id: expr_id, tpe: expr_t.clone() });
    code.push(Instr::PushInt { value: 0 });
    code.push(Instr::SetLocal { id: key_id, tpe: Type::Int });

    // Cond
    code.push(Instr::Label { id: start });
    code.push(Instr::GetLocal { id: expr_id, tpe: expr_t.clone() });
    code.push(Instr::GetField { fields: vec![
        Type::Array(Box::new(val_t.clone())),
        Type::Int
    ], i: 1 });
    code.push(Instr::GetLocal { id: key_id, tpe: Type::Int });
    code.push(Instr::CmpInt { op: Cmp::Neq });
    code.push(Instr::JumpUnless { id: leave });

    // Block
    code.push(Instr::SetLocal { id: val_id, tpe: val_t.clone() });
    code.extend(gen_value(body, g, l));
    code.push(Instr::GetLocal { id: key_id, tpe: Type::Int });
    code.push(Instr::PushInt { value: 1 });
    code.push(Instr::AddInt);
    code.push(Instr::SetLocal { id: key_id, tpe: Type::Int });

    // End
    code.push(Instr::Label { id: leave });
    code
}

fn gen_lambda(args: Vec<(String, builtin::Type)>, ret: builtin::Type, body: Value, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
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

fn gen_builtin(op: String, args: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    todo!()
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
        Builtin { op, args, .. } => gen_builtin(op, args, g, l),
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
            _ => unreachable!()
        }
    }

    let main_id = g.func_id(main_name);
    Program::new(g.to_funcs(), main_id)
}
