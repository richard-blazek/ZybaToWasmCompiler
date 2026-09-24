use std::collections::{HashMap, HashSet};

use crate::frontend::builtin::is_builtin_name;
use crate::frontend::error::{err, Error, Fallible};
use crate::frontend::parser::{Decl, Tree};

fn builtin_check(line: i64, name: &str) -> Fallible<()> {
    if is_builtin_name(name) {
        err(line, format!("{} is a builtin name", name))
    } else {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Expr> },
    Var { line: i64, name: String, source: String },
    Call { line: i64, func: Box<Expr>, args: Vec<Expr> },
    BinOp { line: i64, name: String, lhs: Box<Expr>, rhs: Box<Expr> },
    Access { line: i64, object: Box<Expr>, field: String },
    Lambda { line: i64, args: Vec<(String, Expr)>, ret: Box<Expr>, body: Vec<Expr> },
    Init { line: i64, name: String, value: Box<Expr> },
    Assign { line: i64, name: String, value: Box<Expr> },
    If { line: i64, cond: Box<Expr>, then: Vec<Expr>, elsë: Vec<Expr> },
    While { line: i64, cond: Box<Expr>, body: Vec<Expr> },
    For { line: i64, key: String, value: String, expr: Box<Expr>, body: Vec<Expr> },
}

impl Expr {
    pub fn line(&self) -> i64 {
        use Expr::*;
        match self {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | BinOp { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Assign { line, .. } | Init { line, ..}
            | If { line, .. } | While { line, .. } | For { line, .. } => *line
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Binding {
    Const,
    Local,
    Captured
}

trait Environment {
    fn new_id(&mut self) -> String;
    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, Binding)>;
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
        for (i, (module_path, decls)) in files.iter().enumerate() {
            let in_module = |e: Error| e.in_file(module_path);
            let mut names = HashSet::new();
            for decl in decls {
                match decl {
                    Decl::Const { line, name, private, .. } => {
                        builtin_check(*line, name).map_err(in_module)?;
                        if !names.insert(name) {
                            err(*line, format!("Cannot declare {} twice", name)).map_err(in_module)?;
                        }
                        if !private {
                            let key = (module_path.clone(), name.clone());
                            consts.insert(key, format!("_m{}_{}", i, name));
                        }
                    }
                    Decl::Import { line, path } => {
                        if path == module_path {
                            err(*line, "Module cannot import itself".into()).map_err(in_module)?;
                        }
                        let filename = path.split('/').last().unwrap();
                        let ns = filename.replace(".zyba", "");
                        let key = (module_path.clone(), ns.clone());
                        if imports.insert(key, path.clone()).is_some() {
                            err(*line, format!("Duplicate namespace {}", ns)).map_err(in_module)?;
                        }
                    }
                }
            }
        }
        Ok(GlobalEnv { consts, imports, counter: 0 })
    }
}

impl Environment for GlobalEnv {
    fn new_id(&mut self) -> String {
        self.counter += 1;
        format!("_v{}", self.counter)
    }

    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, Binding)> {
        let key = (module_path.into(), name.into());
        self.consts.get(&key).map(|uniq| (uniq.clone(), Binding::Const))
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
    binding: Binding,
    is_function: bool
}

impl<'a> LocalEnv<'a> {
    fn add_var(&mut self, name: &str) -> Option<String> {
        let uniq = self.new_id();
        let prev = self.locals.insert(name.to_string(), uniq.clone());
        if prev.is_none() { Some(uniq) } else { None }
    }

    fn new_module(parent: &'a mut GlobalEnv, module_path: &'a str, decls: &Vec<Decl>) -> LocalEnv<'a> {
        let mut env = LocalEnv {
            parent, module_path, locals: HashMap::new(), binding: Binding::Const, is_function: false
        };
        for decl in decls {
            if let Decl::Const { name, private: true, .. } = decl {
                env.add_var(name);
            }
        }
        env
    }

    fn new_scope(parent: &'a mut LocalEnv) -> LocalEnv<'a> {
        let module_path = parent.module_path;
        LocalEnv { parent, locals: HashMap::new(), module_path, binding: Binding::Local, is_function: false }
    }

    fn new_function(parent: &'a mut LocalEnv) -> LocalEnv<'a> {
        LocalEnv { is_function: true, ..LocalEnv::new_scope(parent) }
    }
}

impl<'a> Environment for LocalEnv<'a> {
    fn new_id(&mut self) -> String {
        self.parent.new_id()
    }

    fn get_var(&self, module_path: &str, name: &str) -> Option<(String, Binding)> {
        if module_path == self.module_path && let Some(uniq) = self.locals.get(name) {
            return Some((uniq.clone(), self.binding));
        }
        match self.parent.get_var(module_path, name) {
            Some((uniq, Binding::Local)) if self.is_function => Some((uniq, Binding::Captured)),
            var => var
        }
    }

    fn get_ns(&self, module_path: &str, ns_name: &str) -> Option<String> {
        self.parent.get_ns(module_path, ns_name)
    }
}

fn nameres_expr(e: Tree, ns_path: &str, env: &mut LocalEnv) -> Fallible<Expr> {
    match e {
        Tree::Int { line, value } => Ok(Expr::Int { line, value }),
        Tree::Real { line, value } => Ok(Expr::Real { line, value }),
        Tree::Text { line, value } => Ok(Expr::Text { line, value }),
        Tree::Bool { line, value } => Ok(Expr::Bool { line, value }),
        Tree::Var { line, ns: None, name } if is_builtin_name(&name) => {
            Ok(Expr::Var { line, source: name.clone(), name })
        }
        Tree::Var { line, ns: None, name: source } => {
            if let Some((name, _)) = env.get_var(ns_path, &source) {
                Ok(Expr::Var { line, name, source })
            } else {
                err(line, format!("Undefined identifier {}", source))
            }
        }
        Tree::Var { line, ns: Some(ns_name), name } => {
            if let Some(ns_path) = env.get_ns(ns_path, &ns_name) {
                if let Some((uniq, _)) = env.get_var(&ns_path, &name) {
                    Ok(Expr::Var { line, name: uniq, source: format!("{}::{}", ns_name, name) })
                } else {
                    err(line, format!("Undefined identifier {}", name))
                }
            } else {
                err(line, format!("Unknown module {}", ns_name))
            }
        }
        Tree::BinOp { line, name, lhs, rhs } => Ok(Expr::BinOp {
            line,
            name,
            lhs: Box::new(nameres_expr(*lhs, ns_path, env)?),
            rhs: Box::new(nameres_expr(*rhs, ns_path, env)?)
        }),
        Tree::Access { line, object, field } => Ok(Expr::Access {
            line,
            object: Box::new(nameres_expr(*object, ns_path, env)?),
            field
        }),
        Tree::Call { line, func, args } => Ok(Expr::Call {
            line,
            func: Box::new(nameres_expr(*func, ns_path, env)?),
            args: args.into_iter().map(|arg| {
                nameres_expr(arg, ns_path, env)
            }).collect::<Fallible<Vec<_>>>()?
        }),
        Tree::Record { line, fields } => Ok(Expr::Record {
            line,
            fields: fields.into_iter().map(|(key, value)| {
                Ok((key, nameres_expr(value, ns_path, &mut LocalEnv::new_scope(env))?))
            }).collect::<Fallible<HashMap<_, _>>>()?
        }),
        Tree::Lambda { line, args, ret, body } => {
            let (types, ret) = {
                let mut outer = LocalEnv::new_scope(env);
                let types = args.iter().map(|(_, tpe)| {
                    nameres_expr(tpe.clone(), ns_path, &mut outer)
                }).collect::<Fallible<Vec<_>>>()?;
                (types, nameres_expr(*ret, ns_path, &mut outer)?)
            };

            let mut inner = LocalEnv::new_function(env);
            let args = args.into_iter().zip(types).map(|((name, _), tpe)| {
                builtin_check(line, &name)?;
                if let Some(name) = inner.add_var(&name) {
                    Ok((name, tpe))
                } else {
                    err(line, format!("Duplicate argument name {}", name))
                }
            }).collect::<Fallible<Vec<_>>>()?;
            let body = nameres_exprs(body, ns_path, inner)?;

            Ok(Expr::Lambda {
                line,
                args,
                ret: Box::new(ret),
                body
            })
        }
        Tree::Assign { line, name, expr: value } => {
            builtin_check(line, &name)?;
            let value = Box::new(nameres_expr(*value, ns_path, env)?);
            match env.get_var(ns_path, &name) {
                Some((name, Binding::Local)) => Ok(Expr::Assign { line, name, value }),
                Some((_, Binding::Const)) => err(line, format!("Cannot reassign a constant {}", name)),
                Some((_, Binding::Captured)) => err(line, format!("Cannot assign captured variable {}", name)),
                None => {
                    let name = env.add_var(&name).unwrap();
                    Ok(Expr::Init { line, name, value })
                }
            }
        }
        Tree::If { line, cond, then, elsë } => Ok(Expr::If {
            line,
            cond: Box::new(
                nameres_expr(*cond, ns_path, &mut LocalEnv::new_scope(env))?
            ),
            then: nameres_exprs(then, ns_path, LocalEnv::new_scope(env))?,
            elsë: nameres_exprs(elsë, ns_path, LocalEnv::new_scope(env))?
        }),
        Tree::While { line, cond, body } => Ok(Expr::While {
            line,
            cond: Box::new(
                nameres_expr(*cond, ns_path, &mut LocalEnv::new_scope(env))?
            ),
            body: nameres_exprs(body, ns_path, LocalEnv::new_scope(env))?
        }),
        Tree::For { line, key, value, expr, body } => {
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
            Ok(Expr::For { line, key, value, expr: Box::new(expr), body })
        }
    }
}

fn nameres_exprs(exprs: Vec<Tree>, module_path: &str, mut env: LocalEnv) -> Fallible<Vec<Expr>> {
    let mut result = vec![];
    for expr in exprs {
        result.push(nameres_expr(expr, module_path, &mut env)?);
    }
    Ok(result)
}

fn resolve_consts(modules: HashMap<String, Vec<Decl>>) -> Fallible<HashMap<(String, String), (String, Expr)>> {
    let mut global_env = GlobalEnv::new(&modules)?;
    let mut result = HashMap::new();

    for (path, decls) in modules {
        let mut env = LocalEnv::new_module(&mut global_env, &path, &decls);
        for decl in decls {
            if let Decl::Const { name, expr, .. } = decl {
                let (uniq, _) = env.get_var(&path, &name).unwrap();
                let value = nameres_expr(expr, &path, &mut LocalEnv::new_scope(&mut env))
                    .map_err(|e| e.in_file(&path))?;
                result.insert((path.clone(), name), (uniq, value));
            }
        }
    }
    Ok(result)
}

pub fn name_resolution(main: String, modules: HashMap<String, Vec<Decl>>)
        -> Fallible<(String, HashMap<String, Expr>, HashMap<String, String>)> {
    let consts = resolve_consts(modules)?;
    if let Some((main_name, _)) = consts.get(&(main.clone(), "main".to_string())) {
        let main_name = main_name.clone();
        let mut values = HashMap::new();
        let mut paths = HashMap::new();
        for ((path, _), (uniq, value)) in consts {
            values.insert(uniq.clone(), value);
            paths.insert(uniq, path);
        }
        Ok((main_name, values, paths))
    } else {
        err(0, "No main function defined".into()).map_err(|e| e.in_file(&main))
    }
}

#[cfg(test)]
mod nameres_tests {
    use super::*;
    use std::collections::BTreeMap;
    use crate::frontend::parser::parse;

    struct Printer<'a> {
        consts: &'a HashMap<String, String>,
        scope: Vec<(String, String)>,
        count: usize,
        binders: &'a mut Vec<String>,
    }

    impl Printer<'_> {
        fn fresh(&mut self, id: &str) -> (String, String) {
            self.count += 1;
            self.binders.push(id.to_string());
            (id.to_string(), format!("${}", self.count))
        }

        fn bind(&mut self, binder: (String, String)) -> String {
            let label = binder.1.clone();
            self.scope.push(binder);
            label
        }

        fn name(&self, id: &str) -> String {
            if is_builtin_name(id) {
                id.to_string()
            } else if let Some((_, label)) = self.scope.iter().rev().find(|(i, _)| i == id) {
                label.clone()
            } else if let Some(label) = self.consts.get(id) {
                label.clone()
            } else {
                format!("?{}", id)
            }
        }

        fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
            let len = self.scope.len();
            let result = f(self);
            self.scope.truncate(len);
            result
        }

        fn block(&mut self, body: &[Expr]) -> String {
            self.scoped(|p| {
                format!("{{{}}}", body.iter().map(|e| p.show(e)).collect::<Vec<_>>().join(" "))
            })
        }

        fn show(&mut self, e: &Expr) -> String {
            match e {
                Expr::Int { value, .. } => value.to_string(),
                Expr::Real { value, .. } => format!("{:?}", value),
                Expr::Text { value, .. } => format!("{:?}", value),
                Expr::Bool { value, .. } => value.to_string(),
                Expr::Record { fields, .. } => {
                    let mut fields: Vec<_> = fields.iter().collect();
                    fields.sort_by_key(|(k, _)| *k);
                    let fields: String = fields.into_iter().map(|(k, v)| {
                        format!(" ({} {})", k, self.scoped(|p| p.show(v)))
                    }).collect();
                    format!("(record{})", fields)
                }
                Expr::Var { name, .. } => self.name(name),
                Expr::Call { func, args, .. } => {
                    let func = self.show(func);
                    let args: String = args.iter().map(|a| format!(" {}", self.show(a))).collect();
                    format!("(call {}{})", func, args)
                }
                Expr::BinOp { name, lhs, rhs, .. } => {
                    let lhs = self.show(lhs);
                    let rhs = self.show(rhs);
                    format!("({} {} {})", name, lhs, rhs)
                }
                Expr::Access { object, field, .. } => format!("(. {} {})", self.show(object), field),
                Expr::Lambda { args, ret, body, .. } => {
                    let names: Vec<_> = args.iter().map(|(id, _)| self.fresh(id)).collect();
                    let (types, ret) = self.scoped(|p| {
                        let types: Vec<_> = args.iter().map(|(_, t)| p.show(t)).collect();
                        (types, p.show(ret))
                    });
                    self.scoped(|p| {
                        let args: Vec<_> = names.into_iter().zip(types).map(|(name, tpe)| {
                            format!("({} {})", p.bind(name), tpe)
                        }).collect();
                        format!("(fun [{}] {} {})", args.join(" "), ret, p.block(body))
                    })
                }
                Expr::Init { name, value, .. } => {
                    let binder = self.fresh(name);
                    let value = self.show(value);
                    format!("(let {} {})", self.bind(binder), value)
                }
                Expr::Assign { name, value, .. } => {
                    let value = self.show(value);
                    format!("(set {} {})", self.name(name), value)
                }
                Expr::If { cond, then, elsë, .. } => {
                    let cond = self.scoped(|p| p.show(cond));
                    let then = self.block(then);
                    format!("(if {} {} {})", cond, then, self.block(elsë))
                }
                Expr::While { cond, body, .. } => {
                    let cond = self.scoped(|p| p.show(cond));
                    format!("(while {} {})", cond, self.block(body))
                }
                Expr::For { key, value, expr, body, .. } => {
                    let key = self.fresh(key);
                    let value = self.fresh(value);
                    let expr = self.scoped(|p| p.show(expr));
                    self.scoped(|p| {
                        let key = p.bind(key);
                        let value = p.bind(value);
                        format!("(for {} {} {} {})", key, value, expr, p.block(body))
                    })
                }
            }
        }
    }

    fn modules(files: &[(&str, &str)]) -> HashMap<String, Vec<Decl>> {
        files.iter().map(|(path, src)| {
            let decls = parse(src).unwrap_or_else(|e| panic!("{:?} should parse, got {:?}", src, e));
            (path.to_string(), decls)
        }).collect()
    }

    fn label(path: &str, name: &str) -> String {
        format!("{}:{}", path.trim_end_matches(".zyba"), name)
    }

    fn resolve(files: &[(&str, &str)]) -> Fallible<BTreeMap<String, String>> {
        let consts = resolve_consts(modules(files))?;
        let labels: HashMap<String, String> = consts.iter().map(|((path, name), (id, _))| {
            (id.clone(), label(path, name))
        }).collect();
        assert_eq!(labels.len(), consts.len(), "constants must have distinct names: {:?}", labels);

        let mut binders = vec![];
        let mut result = BTreeMap::new();
        for ((path, name), (_, value)) in &consts {
            let mut printer = Printer { consts: &labels, scope: vec![], count: 0, binders: &mut binders };
            result.insert(label(path, name), printer.show(value));
        }

        let distinct: HashSet<_> = binders.iter().collect();
        assert_eq!(distinct.len(), binders.len(), "binders must have distinct names: {:?}", binders);
        for id in &binders {
            assert!(!labels.contains_key(id) && !is_builtin_name(id), "binder {} clashes with a global", id);
        }
        Ok(result)
    }

    fn ok(files: &[(&str, &str)]) -> BTreeMap<String, String> {
        resolve(files).unwrap_or_else(|e| panic!("{:?} should resolve, got {:?}", files, e))
    }

    fn res(src: &str) -> BTreeMap<String, String> {
        ok(&[("main.zyba", src)])
    }

    fn map(entries: &[(&str, &str)]) -> BTreeMap<String, String> {
        entries.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
    }

    fn one(expr: &str) -> String {
        res(&format!("main = {};", expr))["main:main"].clone()
    }

    fn bad(files: &[(&str, &str)], line: i64, message: &str) {
        match resolve(files) {
            Err(e) => {
                assert_eq!(e.line, line, "{:?}: wrong line of {:?}", files, e);
                assert!(e.message.contains(message), "{:?}: expected {:?}, got {:?}", files, message, e);
            }
            Ok(result) => panic!("{:?} should be rejected, got {:?}", files, result),
        }
    }

    fn bad_src(src: &str, line: i64, message: &str) {
        bad(&[("main.zyba", src)], line, message);
    }

    fn bad_one(expr: &str, message: &str) {
        bad_src(&format!("main = {};", expr), 1, message);
    }

    fn main_of(files: &[(&str, &str)]) -> Fallible<(String, HashMap<String, Expr>)> {
        name_resolution("main.zyba".into(), modules(files)).map(|(main, values, _)| (main, values))
    }

    #[test]
    fn structure_is_preserved() {
        assert_eq!(one("7"), "7");
        assert_eq!(one("2.5"), "2.5");
        assert_eq!(one("\"s\""), "\"s\"");
        assert_eq!(one("false"), "false");
        assert_eq!(one("()"), "(record)");
        assert_eq!(one("(b: 1, a: (c: 2.5))"), "(record (a (record (c 2.5))) (b 1))");
        assert_eq!(one("1 + 2 * 3"), "(* (+ 1 2) 3)");
        assert_eq!(one("(a: 1).a"), "(. (record (a 1)) a)");
        assert_eq!(one("print[1, \"x\"][]"), "(call (call print 1 \"x\"))");
        assert_eq!(one("if true { 1; } elif false { 2; }"), "(if true {1} {(if false {2} {})})");
        assert_eq!(one("while true {}"), "(while true {})");
    }

    #[test]
    fn lines_are_preserved() {
        let (main, table) = main_of(&[("main.zyba", "main = print[\n1\n+ 2];")]).unwrap();
        assert_eq!(table[&main], Expr::Call {
            line: 1,
            func: Box::new(Expr::Var { line: 1, name: "print".into(), source: "print".into() }),
            args: vec![Expr::BinOp {
                line: 3,
                name: "+".into(),
                lhs: Box::new(Expr::Int { line: 2, value: 1 }),
                rhs: Box::new(Expr::Int { line: 3, value: 2 }),
            }],
        });

        let (main, table) = main_of(&[("main.zyba", "main = fun [] Int {\nx = 1;\nx = 2;\nfor v: 1 {};\n};")]).unwrap();
        match &table[&main] {
            Expr::Lambda { line: 1, body, .. } => {
                assert!(matches!(body[..], [Expr::Init { line: 2, .. }, Expr::Assign { line: 3, .. }, Expr::For { line: 4, .. }]));
            }
            other => panic!("expected a lambda, got {:?}", other),
        }
    }

    #[test]
    fn builtins_resolve_to_themselves() {
        assert_eq!(
            one("fun [a: Array[Int], f: Func[Int, Bool]] Text { print[len[a]]; chr[65]; not[true]; }"),
            "(fun [($1 (call Array Int)) ($2 (call Func Int Bool))] Text {(call print (call len $1)) (call chr 65) (call not true)})"
        );
    }

    #[test]
    fn builtin_names_cannot_be_bound() {
        bad_src("x = 1;\nprint = 1;", 2, "print is a builtin name");
        bad_src("x = 1;\nprivate Int = 1;", 2, "Int is a builtin name");
        bad_one("fun [len: Int] Int {}", "len is a builtin name");
        bad_one("fun [] Int { text = 1; }", "text is a builtin name");
        bad_one("for array: 1 {}", "array is a builtin name");
        bad_one("for get, v: 1 {}", "get is a builtin name");
    }

    #[test]
    fn constants_are_visible_in_the_whole_module() {
        assert_eq!(
            res("main = f; f = g; private g = main;"),
            map(&[("main:main", "main:f"), ("main:f", "main:g"), ("main:g", "main:main")])
        );
        assert_eq!(
            res("f = fun [n: Int] Int { f[n]; };")["main:f"],
            "(fun [($1 Int)] Int {(call main:f $1)})"
        );
        assert_eq!(
            res("f = fun [] Int { g[]; }; private g = fun [] Int { f[]; };"),
            map(&[("main:f", "(fun [] Int {(call main:g)})"), ("main:g", "(fun [] Int {(call main:f)})")])
        );
    }

    #[test]
    fn constants_cannot_be_declared_twice() {
        bad_src("x = 1;\nx = 2;", 2, "Cannot declare x twice");
        bad_src("private x = 1;\nx = 2;", 2, "Cannot declare x twice");
        bad_src("x = 1;\nprivate x = 2;", 2, "Cannot declare x twice");
        bad_src("private x = 1;\nprivate x = 2;", 2, "Cannot declare x twice");
    }

    #[test]
    fn modules_have_separate_constants() {
        assert_eq!(
            ok(&[("main.zyba", "import \"lib.zyba\"; main = x + lib::x; x = 1; private y = 1;"),
                 ("lib.zyba", "x = 2; private y = 2;")]),
            map(&[("main:main", "(+ main:x lib:x)"), ("main:x", "1"), ("main:y", "1"), ("lib:x", "2"), ("lib:y", "2")])
        );
    }

    #[test]
    fn namespaces() {
        assert_eq!(
            ok(&[("main.zyba", "import \"lib.zyba\"; main = lib::pub;"),
                 ("lib.zyba", "pub = hidden; private hidden = 1;")]),
            map(&[("main:main", "lib:pub"), ("lib:pub", "lib:hidden"), ("lib:hidden", "1")])
        );
        assert_eq!(
            ok(&[("main.zyba", "import \"dir/lib.zyba\"; main = lib::x;"), ("dir/lib.zyba", "x = 1;")])["main:main"],
            "dir/lib:x"
        );
        assert_eq!(
            ok(&[("main.zyba", "import \"lib.zyba\"; lib = 1; main = lib + lib::lib;"), ("lib.zyba", "lib = 2;")])["main:main"],
            "(+ main:lib lib:lib)"
        );
        assert_eq!(
            ok(&[("main.zyba", "import \"lib.zyba\"; main = lib::f;"), ("lib.zyba", "import \"main.zyba\"; f = main::main;")]),
            map(&[("main:main", "lib:f"), ("lib:f", "main:main")])
        );
    }

    #[test]
    fn namespace_errors() {
        let lib = ("lib.zyba", "pub = 1; private hidden = 2;");
        bad(&[("main.zyba", "import \"lib.zyba\";\nmain = lib::hidden;"), lib], 2, "Undefined identifier hidden");
        bad(&[("main.zyba", "import \"lib.zyba\";\nmain = lib::nothing;"), lib], 2, "Undefined identifier nothing");
        bad(&[("main.zyba", "import \"lib.zyba\";\nmain = lib::print;"), lib], 2, "Undefined identifier print");
        bad(&[("main.zyba", "import \"lib.zyba\";\nmain = other::pub;"), lib], 2, "Unknown module other");
        bad(&[("main.zyba", "import \"lib.zyba\";\nmain = pub;"), lib], 2, "Undefined identifier pub");
        bad(&[("main.zyba", "main = lib::pub;"), lib], 1, "Unknown module lib");
        bad(&[("main.zyba", "import \"a.zyba\";\nmain = b::x;"), ("a.zyba", "import \"b.zyba\";"), ("b.zyba", "x = 1;")],
            2, "Unknown module b");
        bad(&[("main.zyba", "import \"lib.zyba\";\nimport \"dir/lib.zyba\";"), ("lib.zyba", ""), ("dir/lib.zyba", "")],
            2, "Duplicate namespace lib");
        bad(&[("main.zyba", "import \"lib.zyba\";\nimport \"lib.zyba\";"), ("lib.zyba", "")], 2, "Duplicate namespace lib");
        bad_src("main = 1;\nimport \"main.zyba\";", 2, "Module cannot import itself");
    }

    #[test]
    fn main_function() {
        let (main, table) = main_of(&[
            ("main.zyba", "import \"lib.zyba\";\nmain = 7;\nprivate x = 1;"),
            ("lib.zyba", "main = 8; private y = 9;")
        ]).unwrap();
        assert_eq!(table[&main], Expr::Int { line: 2, value: 7 });
        assert_eq!(table.len(), 4);

        let (main, table) = main_of(&[("main.zyba", "private main = 7;")]).unwrap();
        assert_eq!(table[&main], Expr::Int { line: 1, value: 7 });

        let missing = [
            main_of(&[("main.zyba", "")]),
            main_of(&[("main.zyba", "import \"lib.zyba\"; x = lib::main;"), ("lib.zyba", "main = 8;")]),
        ];
        for result in missing {
            let e = result.unwrap_err();
            assert_eq!((e.line, e.message.as_str()), (0, "No main function defined"));
        }
    }

    #[test]
    fn first_assignment_declares_a_variable() {
        assert_eq!(one("fun [] Int { x = 1; x = x + 1; x; }"), "(fun [] Int {(let $1 1) (set $1 (+ $1 1)) $1})");
        assert_eq!(one("fun [] Int { x = (y = 1); y = x; }"), "(fun [] Int {(let $1 (let $2 1)) (set $2 $1)})");
        bad_one("fun [] Int { x = x; }", "Undefined identifier x");
        bad_one("fun [] Int { f = fun [] Int { f[]; }; }", "Undefined identifier f");
        assert_eq!(one("fun [] Int { print[x = 1]; x; }"), "(fun [] Int {(call print (let $1 1)) $1})");
        assert_eq!(one("fun [] Int { (x = 1) + x; }"), "(fun [] Int {(+ (let $1 1) $1)})");
        bad_one("fun [] Int { (a: x = 1, b: 2); x; }", "Undefined identifier x");
        bad_one("(a: x = 1, b: x)", "Undefined identifier x");
    }

    #[test]
    fn blocks_are_scopes() {
        assert_eq!(
            one("fun [] Int { x = 1; if true { x = 2; y = 3; y = x; } else { y = 4; }; }"),
            "(fun [] Int {(let $1 1) (if true {(set $1 2) (let $2 3) (set $2 $1)} {(let $3 4)})})"
        );
        assert_eq!(
            one("fun [] Int { x = 1; while true { x = 2; y = x; }; for v: 1 { x = v; }; }"),
            "(fun [] Int {(let $1 1) (while true {(set $1 2) (let $2 $1)}) (for $3 $4 1 {(set $1 $4)})})"
        );
        bad_one("fun [] Int { if true { y = 1; }; y; }", "Undefined identifier y");
        bad_one("fun [] Int { if true { y = 1; } else { y; }; }", "Undefined identifier y");
        bad_one("fun [] Int { if true {} else { y = 1; }; y; }", "Undefined identifier y");
        bad_one("fun [] Int { while true { y = 1; }; y; }", "Undefined identifier y");
        bad_one("fun [] Int { for v: 1 { y = 1; }; y; }", "Undefined identifier y");
        bad_one("fun [] Int { f = fun [] Int { y = 1; }; y; }", "Undefined identifier y");
        bad_src("f = fun [] Int { y = 1; };\ng = fun [] Int { y; };", 2, "Undefined identifier y");
    }

    #[test]
    fn conditions_are_scopes() {
        assert_eq!(
            one("fun [] Int { if (c = true) { c = false; }; }"),
            "(fun [] Int {(if (let $1 true) {(let $2 false)} {})})"
        );
        assert_eq!(
            one("fun [] Int { while (c = true) { c = false; }; }"),
            "(fun [] Int {(while (let $1 true) {(let $2 false)})})"
        );
        bad_one("fun [] Int { if (c = true) { c; }; }", "Undefined identifier c");
        bad_one("fun [] Int { if (c = true) {} else { c; }; }", "Undefined identifier c");
        bad_one("fun [] Int { if (c = true) {}; c; }", "Undefined identifier c");
        bad_one("fun [] Int { while (c = true) { c; }; }", "Undefined identifier c");
        bad_one("fun [] Int { while (c = true) {}; c; }", "Undefined identifier c");
        bad_one("fun [] Int { for v: (a = 1) { a; }; }", "Undefined identifier a");
        bad_one("fun [] Int { for v: (a = 1) {}; a; }", "Undefined identifier a");
    }

    #[test]
    fn assignments_in_constant_initializers_stay_local() {
        assert_eq!(res("main = x = 1;")["main:main"], "(let $1 1)");
        assert_eq!(res("main = (x = 1) + x;")["main:main"], "(+ (let $1 1) $1)");
        bad_src("a = b = 1;\nc = b;", 2, "Undefined identifier b");
    }

    #[test]
    fn constants_cannot_be_reassigned() {
        bad_src("x = 1;\nmain = fun [] Int {\nx = 2;\n};", 3, "Cannot reassign a constant x");
        bad_src("private x = 1;\nmain = fun [] Int {\nx = 2;\n};", 3, "Cannot reassign a constant x");
        bad_src("x = 1;\nmain = x = 2;", 2, "Cannot reassign a constant x");
        bad_one("fun [] Int { main = 1; }", "Cannot reassign a constant main");
        bad_one("fun [] Int { f = fun [] Int { if true { main = 1; }; }; }", "Cannot reassign a constant main");
        assert_eq!(
            ok(&[("main.zyba", "import \"lib.zyba\"; main = fun [] Int { x = 2; };"), ("lib.zyba", "x = 1;")])["main:main"],
            "(fun [] Int {(let $1 2)})"
        );
    }

    #[test]
    fn lambda_arguments() {
        assert_eq!(one("fun [x: Int, y: Text] Int { x = 2; y; }"), "(fun [($1 Int) ($2 Text)] Int {(set $1 2) $2})");
        assert_eq!(res("x = 1; main = fun [x: Int] Int { x = 2; };")["main:main"], "(fun [($1 Int)] Int {(set $1 2)})");
        assert_eq!(
            one("fun [] Int { x = 1; f = fun [x: Int] Int { x = 2; }; x; }"),
            "(fun [] Int {(let $1 1) (let $2 (fun [($3 Int)] Int {(set $3 2)})) $1})"
        );
        bad_one("fun [] Int { f = fun [x: Int] Int {}; x; }", "Undefined identifier x");
    }

    #[test]
    fn lambda_types_are_resolved_outside() {
        assert_eq!(
            one("fun [] Int { t = 1; f = fun [x: t] t {}; }"),
            "(fun [] Int {(let $1 1) (let $2 (fun [($3 $1)] $1 {}))})"
        );
        bad_one("fun [x: Int, y: x] Int {}", "Undefined identifier x");
        bad_one("fun [x: Int] x {}", "Undefined identifier x");
        bad_one("fun [x: Int] (t = Int) { t; }", "Undefined identifier t");
    }

    #[test]
    fn lambda_arguments_must_be_distinct() {
        let lambda = Tree::Lambda {
            line: 1,
            args: vec![
                ("x".into(), Tree::Var { line: 1, ns: None, name: "Int".into() }),
                ("x".into(), Tree::Var { line: 1, ns: None, name: "Text".into() }),
            ],
            ret: Box::new(Tree::Var { line: 1, ns: None, name: "Int".into() }),
            body: vec![],
        };
        let decl = Decl::Const { line: 1, name: "main".into(), expr: lambda, private: false };
        let e = resolve_consts(HashMap::from([("main.zyba".to_string(), vec![decl])])).unwrap_err();
        assert_eq!((e.line, e.message.as_str()), (1, "Duplicate argument name x"));
    }

    #[test]
    fn captured_variables_are_read_only() {
        assert_eq!(
            one("fun [a: Int] Int { x = 1; f = fun [] Int { a + x; }; }"),
            "(fun [($1 Int)] Int {(let $2 1) (let $3 (fun [] Int {(+ $1 $2)}))})"
        );
        assert_eq!(
            one("fun [] Int { f = fun [] Int { y = 1; y = 2; }; }"),
            "(fun [] Int {(let $1 (fun [] Int {(let $2 1) (set $2 2)}))})"
        );
        bad_src("main = fun [] Int {\nx = 1;\nf = fun [] Int {\nx = 2;\n};\n};", 4, "Cannot assign captured variable x");
        bad_one("fun [a: Int] Int { f = fun [] Int { a = 2; }; }", "Cannot assign captured variable a");
        bad_one("fun [] Int { x = 1; f = fun [] Int { if true { x = 2; }; }; }", "Cannot assign captured variable x");
        bad_one("fun [] Int { x = 1; f = fun [] Int { g = fun [] Int { x = 2; }; }; }", "Cannot assign captured variable x");
        bad_one("fun [] Int { f = fun [a: Int] Int { g = fun [] Int { a = 2; }; }; }", "Cannot assign captured variable a");
        bad_one("fun [] Int { for k, v: 1 { f = fun [] Int { k = 2; }; }; }", "Cannot assign captured variable k");
        bad_one("fun [] Int { if true { x = 1; f = fun [] Int { x = 2; }; }; }", "Cannot assign captured variable x");
    }

    #[test]
    fn for_loops() {
        assert_eq!(one("fun [] Int { for k, v: 1 { k = v; }; }"), "(fun [] Int {(for $1 $2 1 {(set $1 $2)})})");
        assert_eq!(one("fun [] Int { for v: 1 { v; }; }"), "(fun [] Int {(for $1 $2 1 {$2})})");
        assert_eq!(
            one("fun [] Int { v = 1; for v: v { v = 2; }; v; }"),
            "(fun [] Int {(let $1 1) (for $2 $3 $1 {(set $3 2)}) $1})"
        );
        assert_eq!(res("v = 1; main = for v: v { v; };")["main:main"], "(for $1 $2 main:v {$2})");
        assert_eq!(one("fun [] Int { for v: 1 { for v: v {}; }; }"), "(fun [] Int {(for $1 $2 1 {(for $3 $4 $2 {})})})");
        bad_one("fun [] Int { for x, x: 1 {}; }", "For-loop variables have identical names");
        bad_one("fun [] Int { for v: v {}; }", "Undefined identifier v");
        bad_one("fun [] Int { for k, v: 1 {}; k; }", "Undefined identifier k");
        bad_one("fun [] Int { for k, v: 1 {}; v; }", "Undefined identifier v");
    }

    #[test]
    fn all_names_are_distinct() {
        let src = "f = fun [x: Int] Int { y = x; for v: 1 { y = v; }; h = fun [x: Int] Int { v = x + y; }; };\n\
                   private g = fun [x: Int] Int { y = x; for v: 1 {}; for v: 1 {}; };";
        let result = ok(&[("main.zyba", src), ("lib.zyba", src)]);
        assert_eq!(result.len(), 4);
        assert_eq!(result["main:g"], "(fun [($1 Int)] Int {(let $2 $1) (for $3 $4 1 {}) (for $5 $6 1 {})})");
        assert_eq!(result["main:g"], result["lib:g"]);
    }
}
