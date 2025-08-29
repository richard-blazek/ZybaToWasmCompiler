use std::collections::{HashMap, HashSet};

use crate::builtin;
use crate::typecheck::Value;
use crate::midend::ir::*;


fn sorted<A, T: IntoIterator<Item=A>, K: Ord, F: Fn(&A) -> &K>(items: T, key: F) -> Vec<A> {
    let mut v = Vec::from_iter(items);
    v.sort_by(|a, b| K::cmp(key(a), key(b)));
    v
}

fn conv_type(t: builtin::Type) -> Type {
    use builtin::Type::*;

    match t {
        Int => Type::Int,
        Real => Type::Real,
        Text => Type::Text,
        Bool => Type::Bool,
        List { item } => Type::Tuple(vec![
            Type::Array(Box::new(conv_type(*item))),
            Type::Int
        ]),
        Dict { key, value } => Type::Tuple(vec![
            Type::Array(Box::new(Type::Tuple(vec![
                conv_type(*key),
                conv_type(*value),
                Type::Bool
            ]))),
            Type::Int
        ]),
        Func { args, ret } => Type::Func(
            args.into_iter().map(conv_type).collect(),
            Box::new(conv_type(*ret))
        ),
        Record { fields } => {
            let vec: Vec<_> = sorted(fields, |(n, _)| n);

            Type::Tuple(vec.into_iter().map(|(_, t)| conv_type(t)).collect())
        }
    }
}

struct Env {
    local_name_to_id: HashMap<String, i64>,
    local_id_to_type: HashMap<i64, Type>,
    local_counter: i64,

    funcs: Vec<Code>,
    global_fetch: HashMap<String, Instr>,
    global_name_to_id: HashMap<String, i64>,
    global_counter: i64,

    label_counter: i64
}

impl Env {
    fn new(values: &HashMap<String, Value>) -> Env {
        use Instr::*;

        let mut instr_map = HashMap::new();
        let mut id_map = HashMap::new();
        let mut counter = 0;

        for (name, value) in values {
            let name = name.clone();
            match value {
                Value::Int { value, .. } => {
                    instr_map.insert(name, PushInt { value: *value });
                }
                Value::Real { value, .. } => {
                    instr_map.insert(name, PushReal { value: *value });
                }
                Value::Text { value, .. } => {
                    instr_map.insert(name, PushText { value: value.clone() });
                }
                Value::Bool { value, .. } => {
                    instr_map.insert(name, PushBool { value: *value });
                }
                Value::Lambda { args, ret, .. } => {
                    instr_map.insert(name.clone(), BindFunc {
                        id: counter,
                        args: args.iter().map(|(_, t)| conv_type(t.clone())).collect(),
                        ret: conv_type(ret.clone()),
                        capture: vec![]
                    });
                    id_map.insert(name, counter);
                    counter += 1;
                }
                _ => unreachable!(),
            }
        }

        let mut funcs = vec![];
        funcs.resize(counter as usize, vec![]);

        Env {
            local_name_to_id: HashMap::new(),
            local_id_to_type: HashMap::new(),
            local_counter: 0,
            global_fetch: instr_map,
            funcs,
            global_name_to_id: id_map,
            global_counter: counter,
            label_counter: 0
        }
    }

    fn new_local(&mut self, name: String, tpe: Type) -> i64 {
        self.local_id_to_type.insert(self.local_counter, tpe);
        self.local_name_to_id.insert(name, self.local_counter);
        self.local_counter += 1;
        self.local_counter - 1
    }

    fn overwrite_local(&mut self, name: String, id: i64) {
        self.local_name_to_id.insert(name, id);
    }

    fn local_type_by_id(&self, id: i64) -> Type {
        self.local_id_to_type[&id].clone()
    }

    fn local_id_by_name(&self, name: &str) -> i64 {
        self.local_name_to_id[name]
    }

    fn local_type_by_name(&self, name: &str) -> Type {
        self.local_type_by_id(self.local_id_by_name(name))
    }

    fn is_local(&self, name: &str) -> bool {
        self.local_name_to_id.contains_key(name)
    }

    fn is_global(&self, name: &str) -> bool {
        self.global_fetch.contains_key(name)
    }

    fn fetch_global(&self, name: &str) -> Instr {
        self.global_fetch[name].clone()
    }

    fn global_id_by_name(&self, name: &str) -> i64 {
        self.global_name_to_id[name]
    }

    fn set_global(&mut self, name: &str, func: Code) {
        let i = self.global_id_by_name(name) as usize;
        self.funcs[i] = func;
    }

    fn add_lambda(&mut self, func: Code) -> i64 {
        self.funcs[self.global_counter as usize - 1] = func;
        self.global_counter += 1;
        self.global_counter - 1
    }

    fn new_label(&mut self) -> i64 {
        self.label_counter += 1;
        self.label_counter - 1
    }
}

fn gen_int(value: i64) -> (Vec<Instr>, Vec<i64>) {
    (vec![Instr::PushInt { value }], vec![])
}

fn gen_real(value: f64) -> (Vec<Instr>, Vec<i64>) {
    (vec![Instr::PushReal { value }], vec![])
}

fn gen_text(value: String) -> (Vec<Instr>, Vec<i64>) {
    (vec![Instr::PushText { value }], vec![])
}

fn gen_bool(value: bool) -> (Vec<Instr>, Vec<i64>) {
    (vec![Instr::PushBool { value }], vec![])
}

fn gen_var(name: String, tpe: builtin::Type, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    if env.is_global(&name) {
        (vec![env.fetch_global(&name)], vec![])
    } else {
        let tpe = conv_type(tpe);
        let id = env.local_id_by_name(&name);
        (vec![Instr::GetLocal { id, tpe }], vec![])
    }
}

fn gen_assign(name: String, value: Value, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let tpe = conv_type(value.tpe());
    let id = env.local_id_by_name(&name);

    let (mut code, locals) = gen_value(value, env);
    code.push(Instr::SetLocal { id, tpe });
    (code, locals)
}

fn gen_init(name: String, value: Value, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let tpe = conv_type(value.tpe());
    let id = env.new_local(name.clone(), tpe.clone());

    let (mut code, mut locals) = gen_value(value, env);
    code.push(Instr::InitLocal { id, tpe });
    locals.push(id);
    (code, locals)
}

fn gen_record(fields: Vec<(String, Value)>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let values: Vec<_> = fields.into_iter().map(|(_, v)| v).collect();
    let types = values.iter().map(|v| conv_type(v.tpe())).collect();

    let (mut code, locals) = gen_values(values, env);
    code.push(Instr::NewTuple { fields: types });
    (code, locals)
}

fn gen_call(func: Value, args: Vec<Value>, ret: builtin::Type, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let args_t = args.iter().map(|a| conv_type(a.tpe())).collect();
    let ret = conv_type(ret);

    let (mut code, mut locals) = gen_values(args, env);
    let (func_code, func_locals) = gen_value(func, env);
    code.extend(func_code);
    locals.extend(func_locals);

    code.push(Instr::CallFunc { args: args_t, ret });
    (code, locals)
}

fn gen_access(object: Value, field: String, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let fields = sorted(match object.tpe() {
        builtin::Type::Record { fields } => fields,
        _ => unreachable!(),
    }, |(n, _)| n);

    let i = fields.iter().position(|(n, _)| n == &field).unwrap();
    let types = fields.into_iter().map(|(_, v)| conv_type(v)).collect();

    let (mut code, locals) = gen_value(object, env);
    code.push(Instr::GetField { fields: types, i });
    (code, locals)
}

fn gen_if(cond: Value, then: Vec<Value>, elsë: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let if_not = env.new_label();
    let end_if = env.new_label();

    let (mut code, locals) = gen_value(cond, env);
    code.push(Instr::JumpUnless { id: if_not });
    code.extend(gen_block(then, env));
    code.push(Instr::JumpAlways { id: end_if });
    code.push(Instr::Label { id: if_not });
    code.extend(gen_block(elsë, env));
    code.push(Instr::Label { id: end_if });

    (code, locals)
}

fn gen_while(cond: Value, body: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let leave = env.new_label();
    let start = env.new_label();

    let mut code = vec![Instr::Label { id: start }];
    let (cond_code, locals) = gen_value(cond, env);
    code.extend(cond_code);

    code.push(Instr::JumpUnless { id: leave });
    code.extend(gen_block(body, env));
    code.push(Instr::JumpAlways { id: start });;
    code.push(Instr::Label { id: leave });

    (code, locals)
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

fn collect_captures(body: &Vec<Value>, args: HashSet<String>, env: &mut Env) -> Vec<String> {
    let (mut used, defined) = collect_vars(body);
    used.retain(|x| {
        !defined.contains(x) && !args.contains(x) && !env.is_global(x)
    });
    Vec::from_iter(used)
}

fn gen_lambda(args: Vec<(String, builtin::Type)>, ret: builtin::Type, body: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let args_set = args.iter().map(|(n, _)| n.clone()).collect();
    let captures = collect_captures(&body, args_set, env);

    let outer_ids: Vec<_> = captures.iter().map(|capture| {
        (capture.clone(), env.global_id_by_name(capture))
    }).collect();

    let capture_types: Vec<_> = captures.iter().map(|capture| {
        env.local_type_by_name(capture)
    }).collect();

    let mut code: Vec<Instr> = vec![];
    for capture in captures {
        let tpe = env.local_type_by_name(&capture);
        let id = env.new_local(capture, tpe);
        code.push(Instr::InitLocal { id, tpe: env.local_type_by_id(id) });
    }

    let arg_types = args.iter().map(|(_, t)| conv_type(t.clone())).collect();

    for (arg_name, arg_tpe) in args {
        let tpe = conv_type(arg_tpe);
        let id = env.new_local(arg_name, tpe.clone());
        code.push(Instr::InitLocal { id, tpe })
    }

    code.extend(gen_block(body, env));

    for (name, outer_id) in outer_ids {
        env.overwrite_local(name, outer_id);
    }

    let lambda_id = env.add_lambda(code);
    (vec![Instr::BindFunc {
        id: lambda_id,
        args: arg_types,
        ret: conv_type(ret),
        capture: capture_types
    }], vec![])
}

fn gen_for(key: String, value: String, expr: Value, body: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    todo!()
}

fn gen_builtin(op: String, args: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    todo!()
}

fn gen_value(value: Value, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    use Value::*;

    match value {
        Int { value, .. } => gen_int(value),
        Real { value, .. } => gen_real(value),
        Text { value, .. } => gen_text(value),
        Bool { value, .. } => gen_bool(value),
        Var { name, tpe } => gen_var(name, tpe, env),
        Assign { name, value, .. } => gen_assign(name, *value, env),
        Init { name, value, .. } => gen_init(name, *value, env),
        Record { fields, .. } => gen_record(sorted(fields, |(n, _)| n), env),
        Call { func, args, tpe } => gen_call(*func, args, tpe, env),
        Access { object, field, .. } => gen_access(*object, field, env),
        If { cond, then, elsë, .. } => gen_if(*cond, then, elsë, env),
        While { cond, body, .. } => gen_while(*cond, body, env),
        Lambda { args, ret, body, .. } => gen_lambda(args, ret, body, env),
        For { key, value, expr, body, .. } => gen_for(key, value, *expr, body, env),
        Builtin { op, args, .. } => gen_builtin(op, args, env)
    }
}

fn gen_values(vals: Vec<Value>, env: &mut Env) -> (Vec<Instr>, Vec<i64>) {
    let mut code = vec![];
    let mut locals = vec![];
    for val in vals {
        let (new_code, new_locals) = gen_value(val, env);
        code.extend(new_code);
        locals.extend(new_locals);
    }
    (code, locals)
}

fn gen_block(body: Vec<Value>, env: &mut Env) -> Vec<Instr> {
    let (mut code, locals) = gen_values(body, env);
    for local in locals {
        code.push(Instr::DropLocal {
            id: local,
            tpe: env.local_type_by_id(local)
        });
    }
    code
}

pub fn generate(main_name: &str, globals: HashMap<String, Value>) -> Program {
    let mut env = Env::new(&globals);

    for (name, value) in globals {
        match value {
            Value::Lambda { args, body, .. } => {
                let mut code = vec![];
                for (arg_name, arg_tpe) in args {
                    let tpe = conv_type(arg_tpe);
                    let id = env.new_local(arg_name, tpe.clone());
                    code.push(Instr::InitLocal { id, tpe });
                }
                code.extend(gen_block(body, &mut env));
                env.set_global(&name, code);
            }
            _ => unreachable!()
        }
    }

    let main_id = env.global_id_by_name(main_name);
    Program::new(env.funcs, main_id as usize)
}
