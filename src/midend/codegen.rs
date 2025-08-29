use std::collections::{HashMap, HashSet};

use crate::builtin;
use crate::midend::{globals::Globals, locals::Locals};
use crate::typecheck::Value;
use crate::midend::ir::*;
use crate::midend::utils::*;

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
    l.add(name.clone(), Type::from(&value.tpe()));
    gen_assign(name, value, g, l)
}

fn gen_record(fields: Vec<(String, Value)>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let values: Vec<_> = fields.into_iter().map(|(_, v)| v).collect();
    let types = values.iter().map(|v| Type::from(&v.tpe())).collect();

    let mut code = gen_values(values, g, l);
    code.push(Instr::NewTuple { fields: types });
    code
}

fn gen_call(func: Value, args: Vec<Value>, ret: builtin::Type, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let args_t = args.iter().map(|a| Type::from(&a.tpe())).collect();
    let ret = Type::from(&ret);

    let mut code = gen_values(args, g, l);
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

fn gen_if(cond: Value, then: Vec<Value>, elsë: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let if_not = g.new_label();
    let end_if = g.new_label();

    let mut code = gen_value(cond, g, l);
    code.push(Instr::JumpUnless { id: if_not });
    code.extend(gen_values(then, g, l));
    code.push(Instr::JumpAlways { id: end_if });
    code.push(Instr::Label { id: if_not });
    code.extend(gen_values(elsë, g, l));
    code.push(Instr::Label { id: end_if });

    code
}

fn gen_while(cond: Value, body: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let leave = g.new_label();
    let start = g.new_label();

    let mut code = vec![Instr::Label { id: start }];
    code.extend(gen_value(cond, g, l));

    code.push(Instr::JumpUnless { id: leave });
    code.extend(gen_values(body, g, l));
    code.push(Instr::JumpAlways { id: start });;
    code.push(Instr::Label { id: leave });

    code
}

fn collect_vars<'a, T: IntoIterator<Item=&'a Value>>(body: T) -> (HashSet<String>, HashSet<String>) {
    let mut used = HashSet::new();
    let mut defined = HashSet::new();
    for value in body {
        match value {
            Value::Record { fields, .. } => {
                let (new_used, new_defined) = collect_vars(fields.values());
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Var { name, .. } => {
                used.insert(name.clone());
            }
            Value::Call { func, args, .. } => {
                let (new_used, new_defined) = collect_vars(
                    args.iter().chain([&**func])
                );
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Builtin { args, .. } => {
                let (new_used, new_defined) = collect_vars(args);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Access { object, .. } => {
                let (new_used, new_defined) = collect_vars([&**object]);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Lambda { args, body, .. } => {
                defined.extend(args.iter().map(|(n, _)| n.clone()));

                let (new_used, new_defined) = collect_vars(body);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Init { name, value, .. } => {
                defined.insert(name.clone());

                let (new_used, new_defined) = collect_vars([&**value]);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::Assign { value, .. } => {
                let (new_used, new_defined) = collect_vars([&**value]);
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::If { cond, then, elsë, .. } => {
                let (new_used, new_defined) = collect_vars(
                    then.iter().chain(elsë).chain([&**cond])
                );
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::While { cond, body, .. } => {
                let (new_used, new_defined) = collect_vars(
                    body.iter().chain([&**cond])
                );
                used.extend(new_used);
                defined.extend(new_defined);
            }
            Value::For { key, value, expr, body, tpe } => {
                defined.insert(key.clone());
                defined.insert(value.clone());

                let (new_used, new_defined) = collect_vars(
                    body.iter().chain([&**expr])
                );
                used.extend(new_used);
                defined.extend(new_defined);
            }
            _ => {}
        }
    }
    (used, defined)
}

fn collect_captures(body: &Vec<Value>, args: HashSet<String>, g: &mut Globals) -> Vec<String> {
    let (mut used, defined) = collect_vars(body);
    used.retain(|x| {
        !defined.contains(x) && !args.contains(x) && !g.contains(x)
    });
    Vec::from_iter(used)
}

fn gen_lambda(args: Vec<(String, builtin::Type)>, ret: builtin::Type, body: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let captures = collect_captures(&body, args.iter().map(|(n, _)| {
        n.clone()
    }).collect(), g);

    let mut inner = Locals::new();
    let mut code = vec![];

    let mut capture_types = vec![];
    for capture in captures {
        let tpe = l.get_type(&capture);
        let id = inner.add(capture, tpe.clone());

        capture_types.push(tpe.clone());
        code.push(Instr::SetLocal { id, tpe });
    }

    let mut arg_types = vec![];
    for (arg, tpe) in args {
        let tpe = Type::from(&tpe);
        let id = inner.add(arg, tpe.clone());

        arg_types.push(tpe.clone());
        code.push(Instr::SetLocal { id, tpe })
    }

    code.extend(gen_values(body, g, &mut inner));

    vec![Instr::BindFunc {
        id: g.add_lambda(Func::new(code, inner.get_all())),
        args: arg_types,
        ret: Type::from(&ret),
        capture: capture_types
    }]
}

fn gen_for(key: String, value: String, expr: Value, body: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    todo!()
}

fn gen_builtin(op: String, args: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    todo!()
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
        If { cond, then, elsë, .. } => gen_if(*cond, then, elsë, g, l),
        While { cond, body, .. } => gen_while(*cond, body, g, l),
        Lambda { args, ret, body, .. } => gen_lambda(args, ret, body, g, l),
        For { key, value, expr, body, .. } => gen_for(key, value, *expr, body, g, l),
        Builtin { op, args, .. } => gen_builtin(op, args, g, l)
    }
}

fn gen_values(vals: Vec<Value>, g: &mut Globals, l: &mut Locals) -> Vec<Instr> {
    let mut code = vec![];
    for val in vals {
        let tpe = val.tpe();
        code.extend(gen_value(val, g, l));
        code.push(Instr::Drop { tpe: Type::from(&tpe) });
    }
    code.pop();
    code
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
                    let id = l.add(arg_name, tpe.clone());
                    code.push(Instr::SetLocal { id, tpe });
                }
                code.extend(gen_values(body, &mut g, &mut l));

                let func = Func::new(code, l.get_all());
                g.set_func(g.func_id(&name), func);
            }
            _ => unreachable!()
        }
    }

    let main_id = g.func_id(main_name);
    Program::new(g.to_funcs(), main_id)
}
