use std::collections::HashMap;

use crate::builtin::is_builtin_name;
use crate::error::{err, Fallible};
use crate::parser::{Decl, Expr};

fn builtin_check(line: i64, name: &str) -> Fallible<()> {
    if is_builtin_name(name) {
        err(line, format!("{} is a builtin name", name))
    } else {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Value> },
    Var { line: i64, name: String },
    Call { line: i64, func: Box<Value>, args: Vec<Value> },
    BinOp { line: i64, name: String, lhs: Box<Value>, rhs: Box<Value> },
    Access { line: i64, object: Box<Value>, field: String },
    Lambda { line: i64, args: Vec<(String, Value)>, return_type: Box<Value>, body: Vec<Value> },
    Init { line: i64, name: String, value: Box<Value> },
    Assign { line: i64, name: String, value: Box<Value> },
    If { line: i64, cond: Box<Value>, then: Vec<Value>, elsë: Vec<Value> },
    While { line: i64, cond: Box<Value>, body: Vec<Value> },
    For { line: i64, key: String, value: String, expr: Box<Value>, body: Vec<Value> },
}

impl Value {
    pub fn line(&self) -> i64 {
        use Value::*;
        match self {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | BinOp { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Assign { line, .. } | Init { line, ..}
            | If { line, .. } | While { line, .. } | For { line, .. } => *line
        }
    }
}

trait Environment {
    fn new_id(&mut self) -> String;
    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, bool)>;
    fn get_ns(&self, module_path: &str, ns_name: &str) -> Option<String>;
}

struct GlobalEnv {
    consts: HashMap<(String, String), String>,
    imports: HashMap<(String, String), String>,
    counter: i64
}

impl GlobalEnv {
    fn new(files: &HashMap<String, Vec<Decl>>) -> Fallible<GlobalEnv> {
        let mut consts = HashMap::new();
        let mut imports = HashMap::new();
        let mut i = 0;
        for (module_path, decls) in files {
            for decl in decls {
                if let Decl::Const { line, name, private: false, .. } = decl {
                    builtin_check(*line, name)?;
                    let uniq = format!("_m{}_{}", i, name);
                    let key = (module_path.clone(), name.clone());
                    if consts.insert(key, uniq).is_some() {
                        err(*line, format!("Cannot declare {} twice", name))?;
                    }
                } else if let Decl::Import { line, path, .. } = decl {
                    let filename = path.split('/').last().unwrap();
                    let ns = filename.replace(".zyba", "");
                    let key = (module_path.clone(), ns.clone());
                    if imports.insert(key, path.clone()).is_some() {
                        err(*line, format!("Duplicate namespace {}", ns))?;
                    }
                }
            }
            i += 1;
        }
        Ok(GlobalEnv { consts, imports, counter: 0 })
    }
}

impl Environment for GlobalEnv {
    fn new_id(&mut self) -> String {
        self.counter += 1;
        format!("_v{}", self.counter)
    }

    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, bool)> {
        let key = (module_path.into(), name.into());
        if let Some(uniq) = self.consts.get(&key) {
            Some((uniq.clone(), false))
        } else {
            None
        }
    }

    fn get_ns(&self, module_path: &str, ns_name: &str) -> Option<String> {
        let key = (module_path.into(), ns_name.into());
        self.imports.get(&key).cloned()
    }
}

struct LocalEnv<'a> {
    parent: &'a mut dyn Environment,
    locals: HashMap<String, String>,
    module_path: &'a str,
    mutable: bool
}

impl<'a> LocalEnv<'a> {
    fn add_var(&mut self, name: &str) -> Option<String> {
        let uniq = self.new_id();
        let prev = self.locals.insert(name.to_string(), uniq.clone());
        if prev.is_none() { Some(uniq) } else { None }
    }

    fn new_module(parent: &'a mut GlobalEnv, module_path: &'a str, decls: &Vec<Decl>) -> Fallible<LocalEnv<'a>> {
        let mut env = LocalEnv {
            parent, module_path, locals: HashMap::new(), mutable: false
        };
        for decl in decls {
            if let Decl::Const { line, name, private: true, .. } = decl {
                builtin_check(*line, name)?;
                if env.get_var(module_path, name).is_some() {
                    err(*line, format!("Cannot declare {} twice", name))?;
                } else {
                    env.add_var(name);
                }
            }
        }
        Ok(env)
    }

    fn new_scope(parent: &'a mut LocalEnv) -> LocalEnv<'a> {
        let module_path = parent.module_path;
        LocalEnv { parent, locals: HashMap::new(), module_path, mutable: true }
    }
}

impl<'a> Environment for LocalEnv<'a> {
    fn new_id(&mut self) -> String {
        self.parent.new_id()
    }

    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, bool)> {
        if module_path != self.module_path {
            self.parent.get_var(module_path, name)
        }
        else if let Some(uniq) = self.locals.get(name) {
            Some((uniq.clone(), self.mutable))
        } else {
            self.parent.get_var(module_path, name)
        }
    }

    fn get_ns(&self, module_path: &str, ns_name: &str) -> Option<String> {
        self.parent.get_ns(module_path, ns_name)
    }
}

fn nameres_expr(e: Expr, ns_path: &str, env: &mut LocalEnv) -> Fallible<Value> {
    match e {
        Expr::Int { line, value } => Ok(Value::Int { line, value }),
        Expr::Real { line, value } => Ok(Value::Real { line, value }),
        Expr::Text { line, value } => Ok(Value::Text { line, value }),
        Expr::Bool { line, value } => Ok(Value::Bool { line, value }),
        Expr::Var { line, ns: None, name } if is_builtin_name(&name) => {
            Ok(Value::Var { line, name })
        }
        Expr::Var { line, ns: None, name } => {
            if let Some((name, _)) = env.get_var(ns_path, &name) {
                Ok(Value::Var { line, name })
            } else {
                err(line, format!("Undefined identifier {}", name))
            }
        }
        Expr::Var { line, ns: Some(ns_name), name } => {
            if let Some(ns_path) = env.get_ns(ns_path, &ns_name) {
                if let Some((name, _)) = env.get_var(&ns_path, &name) {
                    Ok(Value::Var { line, name })
                } else {
                    err(line, format!("Undefined identifier {}", name))
                }
            } else {
                err(line, format!("Unknown module {}", ns_name))
            }
        }
        Expr::BinOp { line, name, lhs, rhs } => Ok(Value::BinOp {
            line,
            name,
            lhs: Box::new(nameres_expr(*lhs, ns_path, env)?),
            rhs: Box::new(nameres_expr(*rhs, ns_path, env)?)
        }),
        Expr::Access { line, object, field } => Ok(Value::Access {
            line,
            object: Box::new(nameres_expr(*object, ns_path, env)?),
            field
        }),
        Expr::Call { line, func, args } => Ok(Value::Call {
            line,
            func: Box::new(nameres_expr(*func, ns_path, env)?),
            args: args.into_iter().map(|arg| {
                nameres_expr(arg, ns_path, env)
            }).collect::<Fallible<Vec<_>>>()?
        }),
        Expr::Record { line, fields } => Ok(Value::Record {
            line,
            fields: fields.into_iter().map(|(key, value)| {
                Ok((key, nameres_expr(value, ns_path, env)?))
            }).collect::<Fallible<HashMap<_, _>>>()?
        }),
        Expr::Lambda { line, args, return_type, body } => {
            let mut inner = LocalEnv::new_scope(env);

            let args = args.into_iter().map(|(name, tpe)| {
                builtin_check(line, &name)?;
                let name = if let Some(name) = inner.add_var(&name) {
                    name
                } else {
                    err(line, format!("Duplicate argument name {}", name))?
                };
                Ok((name, nameres_expr(tpe, ns_path, &mut inner)?))
            }).collect::<Fallible<Vec<_>>>()?;

            let return_type = nameres_expr(*return_type, ns_path, &mut inner)?;
            let body = nameres_exprs(body, ns_path, inner)?;

            Ok(Value::Lambda {
                line,
                args,
                return_type: Box::new(return_type),
                body
            })
        }
        Expr::Assign { line, name, expr: value } => {
            builtin_check(line, &name)?;
            let value = nameres_expr(*value, ns_path, env)?;
            let var = env.get_var(ns_path, &name);
            if let Some((_, false)) = var {
                err(line, format!("Cannot reassign a constant {}", name))
            } else if let Some((name, true)) = var {
                Ok(Value::Assign { line, name, value: Box::new(value) })
            } else {
                env.add_var(&name);
                Ok(Value::Init { line, name, value: Box::new(value) })
            }
        }
        Expr::If { line, cond, then, elsë } => Ok(Value::If {
            line,
            cond: Box::new(
                nameres_expr(*cond, ns_path, &mut LocalEnv::new_scope(env))?
            ),
            then: nameres_exprs(then, ns_path, LocalEnv::new_scope(env))?,
            elsë: nameres_exprs(elsë, ns_path, LocalEnv::new_scope(env))?
        }),
        Expr::While { line, cond, body } => Ok(Value::While {
            line,
            cond: Box::new(
                nameres_expr(*cond, ns_path, &mut LocalEnv::new_scope(env))?
            ),
            body: nameres_exprs(body, ns_path, LocalEnv::new_scope(env))?
        }),
        Expr::For { line, key, value, expr, body } => {
            let expr = nameres_expr(
                *expr,
                ns_path,
                &mut LocalEnv::new_scope(env)
            )?;
            let mut inner = LocalEnv::new_scope(env);
            let key = if let Some(key) = key {
                builtin_check(line, &key)?;
                inner.add_var(&key).unwrap()
            } else {
                inner.new_id()
            };
            builtin_check(line, &value)?;
            let value = if let Some(value) = inner.add_var(&value) {
                value
            } else {
                err(line, "For-loop variables have identical names".into())?
            };
            let body = nameres_exprs(body, ns_path, inner)?;
            Ok(Value::For { line, key, value, expr: Box::new(expr), body })
        }
    }
}

fn nameres_exprs(exprs: Vec<Expr>, module_path: &str, mut env: LocalEnv) -> Fallible<Vec<Value>> {
    let mut result = vec![];
    for expr in exprs {
        result.push(nameres_expr(expr, module_path, &mut env)?);
    }
    Ok(result)
}

pub fn name_resolution(main: String, modules: HashMap<String, Vec<Decl>>) -> Fallible<(String, HashMap<String, Value>)> {
    let mut global_env = GlobalEnv::new(&modules)?;
    let mut result = HashMap::new();
    let mut main_name = None;

    for (path, decls) in modules {
        let mut env = LocalEnv::new_module(&mut global_env, &path, &decls)?;
        for decl in decls {
            if let Decl::Const { name, expr, .. } = decl {
                let (new_name, _) = env.get_var(&path, &name).unwrap();
                let new_value = nameres_expr(expr, &path, &mut env)?;

                if path == main && name == "main" {
                    main_name = Some(new_name.clone());
                }

                result.insert(new_name, new_value);
            }
        }
    }

    if let Some(main_name) = main_name {
        Ok((main_name, result))
    } else {
        err(0, "No main function defined".into())
    }
}

#[cfg(test)]
mod nameres_tests {
    use super::*;
    use std::collections::HashMap;

    fn int(value: i64) -> Expr {
        Expr::Int { line: 1, value }
    }

    fn real() -> Expr {
        Expr::Real { line: 1, value: 4.5 }
    }

    fn text() -> Expr {
        Expr::Text { line: 1, value: "zyba".to_string() }
    }

    fn bool() -> Expr {
        Expr::Bool { line: 1, value: true }
    }

    fn var(name: &str) -> Expr {
        Expr::Var { line: 1, ns: None, name: name.to_string() }
    }

    fn var_ns(ns: &str, name: &str) -> Expr {
        Expr::Var { line: 1, ns: Some(ns.to_string()), name: name.to_string() }
    }

    fn record() -> Expr {
        Expr::Record {
            line: 1,
            fields: HashMap::from_iter([
                ("b".to_string(), bool()),
                ("t".to_string(), text())
            ])
        }
    }

    fn access(name: &str, field: &str) -> Expr {
        Expr::Access {
            line: 1,
            object: Box::new(var(name)),
            field: field.to_string(),
        }
    }

    fn call(func: &str, args: Expr) -> Expr {
        Expr::Call {
            line: 1,
            func: Box::new(var(func)),
            args: vec![args],
        }
    }

    fn binop() -> Expr {
        Expr::BinOp {
            line: 1,
            name: "+".to_string(),
            lhs: Box::new(int(1)),
            rhs: Box::new(int(2)),
        }
    }

    fn const_decl(name: &str, expr: Expr, private: bool) -> Decl {
        Decl::Const { line: 1, name: name.to_string(), expr, private }
    }

    fn import_decl(path: &str) -> Decl {
        Decl::Import { line: 1, path: path.to_string() }
    }

    fn get_key<K: Clone, V: PartialEq + Clone>(map: &HashMap<K, V>, val: &V) -> Option<K> {
        match map.iter().filter(|(_, v)| *v == val).map(|(k, _)| k).next() {
            Some(k) => Some(k.clone()),
            None => None
        }
    }

    #[test]
    fn test_simple_const() {
        use Value::*;

        let mut modules = HashMap::new();
        modules.insert(
            "main.zyba".to_string(),
            vec![
                const_decl("main", int(123), false),
                const_decl("num", real(), false),
                const_decl("str", text(), true),
                const_decl("truth", bool(), true),
                const_decl("str2", var("str"), true),
                const_decl("record", record(), true),
                const_decl("sum", binop(), true),
                const_decl("field", access("record", "b"), true),
                const_decl("call", call("print", var("str")), true),
            ]
        );

        let (main_name, table) = name_resolution("main.zyba".into(), modules).unwrap();
        assert!(table.contains_key(&main_name));

        match table.get(&main_name) {
            Some(v) => {
                assert_eq!(v.line(), 1);
                assert!(matches!(v, Int { value: 123, .. }));
            }
            _ => panic!("expected value 123"),
        }

        assert!(get_key(&table, &Real { value: 4.5, line: 1 }).is_some());
        assert!(get_key(&table, &Bool { value: true, line: 1 }).is_some());
        let str_key = get_key(&table, &Text { value: "zyba".to_string(), line: 1 }).expect("str not found");
        assert!(get_key(&table, &Var { name: str_key.clone(), line: 1 }).is_some());

        let e_key = get_key(&table, &Record {
            line: 1,
            fields: HashMap::from_iter([
                ("b".to_string(), Bool { line: 1, value: true }),
                ("t".to_string(), Text { line: 1, value: "zyba".to_string() }),
            ])
        }).expect("record not found");

        assert!(get_key(&table, &BinOp {
            line: 1,
            name: "+".to_string(),
            lhs: Box::new(Int { line: 1, value: 1 }),
            rhs: Box::new(Int { line: 1, value: 2 }),
        }).is_some());

        assert!(get_key(&table, &Access {
            line: 1,
            object: Box::new(Var { line: 1, name: e_key }),
            field: "b".to_string()
        }).is_some());

        assert!(get_key(&table, &Call {
            line: 1,
            func: Box::new(Var { line: 1, name: "print".to_string() }),
            args: vec![Var { line: 1, name: str_key }]
        }).is_some());
    }

    #[test]
    fn test_if() {
        use Value::*;

        let mut modules = HashMap::new();
        modules.insert(
            "main.zyba".to_string(),
            vec![
                const_decl("main", Expr::If {
                    line: 1,
                    cond: Box::new(bool()),
                    then: vec![int(321)],
                    elsë: vec![int(123)]
                }, false),
            ]
        );

        let (main_name, table) = name_resolution("main.zyba".into(), modules).unwrap();

        match table.get(&main_name).expect("missing main") {
            If { cond, then, elsë, .. } => {
                assert_eq!(**cond, Bool { line: 1, value: true });
                assert_eq!(then[0], Int { line: 1, value: 321 });
                assert_eq!(elsë[0], Int { line: 1, value: 123 });
            }
            _ => panic!("should resolve to if")
        }
    }

    #[test]
    fn test_while() {
        use Value::*;

        let mut modules = HashMap::new();
        modules.insert(
            "main.zyba".to_string(),
            vec![
                const_decl("main", Expr::While {
                    line: 1,
                    cond: Box::new(bool()),
                    body: vec![int(777)],
                }, false),
            ]
        );

        let (main_name, table) = name_resolution("main.zyba".into(), modules).unwrap();

        match table.get(&main_name).expect("missing main") {
            While { cond, body, .. } => {
                assert_eq!(**cond, Bool { line: 1, value: true });
                assert_eq!(body[0], Int { line: 1, value: 777 });
            }
            _ => panic!("should resolve to while")
        }
    }

    #[test]
    fn test_for() {
        use Value::*;

        let mut modules = HashMap::new();
        modules.insert(
            "main.zyba".to_string(),
            vec![
                const_decl("main", Expr::For {
                    line: 1,
                    key: None,
                    value: "item".to_string(),
                    expr: Box::new(int(20)),
                    body: vec![var("item")],
                }, false),
                const_decl("foo", Expr::For {
                    line: 1,
                    key: Some("key".to_string()),
                    value: "value".to_string(),
                    expr: Box::new(int(20)),
                    body: vec![var("key"), var("value")],
                }, false),
            ]
        );

        let (main_name, table) = name_resolution("main.zyba".into(), modules).unwrap();

        match table.get(&main_name).expect("missing main") {
            For { key, value, body, expr, .. } => {
                assert_ne!(key, value);
                assert!(matches!(*expr.clone(), Int { value: 20, .. }));
                assert_eq!(body[0], Var { line: 1, name: value.clone() });
            }
            _ => panic!("should resolve to for")
        }

        match table.iter().filter(|(k, _)| **k != main_name).next() {
            Some((_, For { key, value, body, expr, .. })) => {
                assert_ne!(key, value);
                assert!(matches!(*expr.clone(), Int { value: 20, .. }));
                assert_eq!(body[0], Var { line: 1, name: key.clone() });
                assert_eq!(body[1], Var { line: 1, name: value.clone() });
            }
            _ => panic!("foo not found")
        }

        let modules = HashMap::from_iter([
            ("main.zyba".to_string(), vec![
                const_decl("main", Expr::For {
                    line: 1,
                    key: Some("item".to_string()),
                    value: "item".to_string(),
                    expr: Box::new(int(20)),
                    body: vec![var("item")],
                }, false),
            ])
        ]);
        name_resolution("main.zyba".into(), modules).expect_err("duplicate var name");
    }

    #[test]
    fn test_import() {
        use Value::*;

        let mut modules = HashMap::new();
        modules.insert("main.zyba".into(), vec![
            import_decl("lib.zyba"),
            const_decl("main", var_ns("lib", "foo"), false)
        ]);
        modules.insert("lib.zyba".into(), vec![
            const_decl("foo", var("bar"), false),
            const_decl("bar", int(42), true),
            const_decl("baz", var("print"), true)
        ]);

        let (main_name, table) = name_resolution("main.zyba".into(), modules).unwrap();
        
        assert!(get_key(&table, &Var { line: 1, name: "print".to_string() }).is_some());
        let bar = get_key(&table, &Int { line: 1, value: 42 }).expect("constant not resolved correctly");
        let foo = get_key(&table, &Var { line: 1, name: bar }).expect("foo/bar not resolved correctly");
        let main = get_key(&table, &Var { line: 1, name: foo });
        assert_eq!(main, Some(main_name));
    }

    #[test]
    fn test_shadowing_assignment_and_reuse() {
        let body = vec![
            Expr::Assign { line: 1, name: "x".into(), expr: Box::new(int(1)) },
            Expr::Assign { line: 1, name: "x".into(), expr: Box::new(int(2)) },
        ];
        let main = vec![const_decl("main", Expr::Lambda {
            line: 1,
            args: vec![],
            return_type: Box::new(int(0)),
            body
        }, false)];

        let mut modules = HashMap::new();
        modules.insert("main.zyba".into(), main);

        let (_, table) = name_resolution("main.zyba".into(), modules).unwrap();
        // just ensure it succeeds: first Assign => Init, second => Assign
        match table.values().next().unwrap() {
            Value::Lambda { body, .. } => {
                assert!(matches!(body[0], Value::Init { .. }));
                assert!(matches!(body[1], Value::Assign { .. }));
            }
            v => panic!("unexpected {:?}", v),
        }
    }

    #[test]
    fn test_duplicate_is_error() {
        let cases = vec![
            vec![const_decl("main", bool(), true), const_decl("x", bool(), true), const_decl("x", bool(), true)],
            vec![const_decl("main", bool(), true), const_decl("x", bool(), true), const_decl("x", bool(), false)],
            vec![const_decl("main", bool(), true), const_decl("x", bool(), false), const_decl("x", bool(), true)],
            vec![const_decl("main", bool(), true), const_decl("x", bool(), false), const_decl("x", bool(), false)],
        ];

        for case in cases {
            let modules = HashMap::from_iter([("main.zyba".into(), case)]);
            name_resolution("main.zyba".into(), modules).expect_err("cannot redeclare x");
        }
    }

    #[test]
    fn test_unknown_is_error() {
        let cases = vec![var("x"), var_ns("lib", "y"), var_ns("std", "y")];

        for case in cases {
            let modules = HashMap::from_iter([
                ("main.zyba".into(), vec![
                    import_decl("lib.zyba"),
                    const_decl("main", case, false)
                ]),
                ("lib.zyba".into(), vec![])
            ]);
            name_resolution("main.zyba".into(), modules).expect_err("undefined name");
        }
    }

    #[test]
    fn test_duplicate_import_is_error() {
        let mut modules = HashMap::new();
        modules.insert(
            "main.zyba".into(),
            vec![
                import_decl("lib.zyba"),
                import_decl("other/lib.zyba"),
                const_decl("main", int(1), false),
            ]
        );
        modules.insert("lib.zyba".into(), vec![]);
        modules.insert("other/lib.zyba".into(), vec![]);
        name_resolution("main.zyba".into(), modules).expect_err("duplicate import");
    }

    #[test]
    fn test_builtin_name_const_is_error() {
        let mut modules = HashMap::new();
        modules.insert("main.zyba".into(), vec![const_decl("print", int(1), false)]);
        name_resolution("main.zyba".into(), modules).expect_err("cannot assign to builtin");
    }

    #[test]
    fn test_no_main_function_is_error() {
        let modules = HashMap::from_iter([("main.zyba".into(), vec![])]);
        name_resolution("main.zyba".into(), modules).expect_err("No main function");
    }

    #[test]
    fn test_duplicate_argument_is_error() {
        let modules = HashMap::from_iter([("main.zyba".to_string(), vec![
            const_decl("main", Expr::Lambda {
                line: 1,
                args: vec![
                    ("x".to_string(), var("Int")),
                    ("x".to_string(), var("Text"))
                ],
                return_type: Box::new(var("Int")),
                body: vec![]
            }, false)
        ])]);
        name_resolution("main.zyba".into(), modules).expect_err("No main function");
    }
}
