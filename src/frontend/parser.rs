use std::collections::{HashMap, HashSet};

use crate::frontend::builtin::is_builtin_operator;
use crate::frontend::error::{err, Fallible};
use crate::frontend::lexer::{Token, tokenize};

// The tokens are produced by the lexer
// <file>      ::= (<decl> ";")*
// <decl>      ::= <import> | <const>
// <import>    ::= "import" <text>
// <const>     ::= "private"? <name> "=" <expr>
//
// <expr>      ::= (<name> "=")? <binop>
// <binop>     ::= <operand> (<binary_op> <operand>)*
// <binary_op> ::= "*" | "/" | "%" | "<<" | ">>" | ">>>" | "+" | "-" | "&" | "|" | "^"
//               | "==" | "!=" | "<" | "<=" | ">" | ">=" | "||" | "&&"
// <operand>   ::= <atom> ("." <name> | <call>)*
// <call>      ::= "[" (<expr> ("," <expr>)*)? "]"
// <atom>      ::= <int> | <real> | <text> | <bool> | <record> | "(" <expr> ")"
//               | <var> | <lambda> | <if> | <while> | <for>
// <record>    ::= "(" ")" | "(" <name> ":" <expr> ("," <name> ":" <expr>)* ")"
// <var>       ::= <name> (":" ":" <name>)?
// <lambda>    ::= "fun" "[" (<name> ":" <expr> ("," <name> ":" <expr>)*)? "]" <expr> <block>
// <block>     ::= "{" (<expr> ";")* "}"
// <if>        ::= "if" <expr> <block> ("elif" <expr> <block>)* ("else" <block>)?
// <while>     ::= "while" <expr> <block>
// <for>       ::= "for" <name> ("," <name>)? ":" <expr> <block>

#[derive(Debug, Clone, PartialEq)]
pub enum Tree {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Tree> },
    Var { line: i64, ns: Option<String>, name: String },
    Call { line: i64, func: Box<Tree>, args: Vec<Tree> },
    BinOp { line: i64, name: String, lhs: Box<Tree>, rhs: Box<Tree> },
    Access { line: i64, object: Box<Tree>, field: String },
    Lambda { line: i64, args: Vec<(String, Tree)>, ret: Box<Tree>, body: Vec<Tree> },
    Assign { line: i64, name: String, expr: Box<Tree> },
    If { line: i64, cond: Box<Tree>, then: Vec<Tree>, elsë: Vec<Tree> },
    While { line: i64, cond: Box<Tree>, body: Vec<Tree> },
    For { line: i64, key: Option<String>, value: String, expr: Box<Tree>, body: Vec<Tree> }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Import { line: i64, path: String },
    Const { line: i64, name: String, expr: Tree, private: bool },
}

fn expect(tokens: &[Token], i: usize, sep: char) -> Fallible<usize> {
    if let Token::Separator { name: c, .. } = tokens[i] && c == sep {
        Ok(i + 1)
    } else {
        err(tokens[i].line(), format!("Expected {:?} but got {:?}", sep, tokens[i]))
    }
}

fn parse_ident(tokens: &[Token], i: usize) -> Fallible<(usize, String)> {
    match &tokens[i] {
        Token::Name { name, .. } => Ok((i + 1, name.clone())),
        token => err(token.line(), format!("Expected a name but got {:?}", token))
    }
}

fn parse_atom(tokens: &[Token], i: usize) -> Fallible<(usize, Tree)> {
    match &tokens[i] {
        Token::Int { line, value } => Ok((i + 1, Tree::Int { line: *line, value: *value })),
        Token::Real { line, value } => Ok((i + 1, Tree::Real { line: *line, value: *value })),
        Token::Text { line, value } => Ok((i + 1, Tree::Text { line: *line, value: value.clone() })),
        Token::Bool { line, value } => Ok((i + 1, Tree::Bool { line: *line, value: *value })),
        Token::Separator { line, name: '(' } => parse_parentheses(tokens, i + 1, *line),
        Token::Fun { line } => parse_fun(tokens, i + 1, *line),
        Token::If { line } => parse_if(tokens, i + 1, *line),
        Token::While { line } => parse_while(tokens, i + 1, *line),
        Token::For { line } => {
            let (i, var) = parse_ident(tokens, i + 1)?;
            parse_for(tokens, i, *line, var)
        }
        Token::Name { line, name } => parse_var(tokens, i + 1, *line, name.clone()),
        token => err(token.line(), format!("Expected a value, but got {:?}", token))
    }
}

fn parse_parentheses(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    if let Token::Separator { name: ')', .. } = &tokens[i] {
        Ok((i + 1, Tree::Record { line, fields: HashMap::new() }))
    } else if let (Some(Token::Name { .. }), Some(Token::Separator { name: ':', .. })) = (tokens.get(i), tokens.get(i + 1))
            && !matches!(tokens.get(i + 2), Some(Token::Separator { name: ':', .. })) {
        parse_record(tokens, i, line)
    } else {
        let (i, value) = parse_expr(tokens, i)?;
        let i = expect(tokens, i, ')')?;
        Ok((i, value))
    }
}

fn parse_key_value_pairs(tokens: &[Token], i: usize, end: char) -> Fallible<(usize, Vec<(String, Tree)>)> {
    let mut i = i;
    let mut keys = HashSet::new();
    let mut pairs = Vec::new();

    if let Token::Separator { name: c, .. } = &tokens[i] && *c == end {
        return Ok((i + 1, pairs));
    }

    loop {
        if let (Some(Token::Name { name, line }),
                Some(Token::Separator { name: ':', .. })) = (tokens.get(i), tokens.get(i + 1)) {
            if !keys.insert(name.clone()) {
                err(*line, format!("Duplicate name {}", name))?;
            }
            let (new_i, value) = parse_expr(tokens, i + 2)?;
            i = new_i;
            pairs.push((name.clone(), value));

            match &tokens[i] {
                Token::Separator { name: ',', .. } => i += 1,
                Token::Separator { name: c, .. } if *c == end => return Ok((i + 1, pairs)),
                token => err(token.line(), format!("Expected a comma but got {:?}", token))?
            }
        } else {
            err(tokens[i].line(), format!("Expected a name but got {:?}", tokens[i]))?;
        }
    }
}

fn parse_record(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    let (i, fields) = parse_key_value_pairs(tokens, i, ')')?;
    Ok((i, Tree::Record { line: line, fields: fields.into_iter().collect() }))
}

fn parse_var(tokens: &[Token], i: usize, line: i64, first_name: String) -> Fallible<(usize, Tree)> {
    if let (Some(Token::Separator { name: ':', .. }),
            Some(Token::Separator { name: ':', .. }),
            Some(Token::Name { name, .. })) = (tokens.get(i), tokens.get(i + 1), tokens.get(i + 2)) {
        Ok((i + 3, Tree::Var { line, ns: Some(first_name), name: name.clone() }))
    } else {
        Ok((i, Tree::Var { line, ns: None, name: first_name}))
    }
}

fn parse_arguments(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<(String, Tree)>)> {
    let i = expect(tokens, i, '[')?;
    parse_key_value_pairs(tokens, i, ']')
}

fn parse_fun(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    let (i, args) = parse_arguments(tokens, i)?;
    let (i, ret) = parse_expr(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Tree::Lambda { line, args, ret: Box::new(ret), body }))
}

fn parse_access(tokens: &[Token], i: usize, line: i64, object: Tree) -> Fallible<(usize, Tree)> {
    if let Token::Name { name, .. } = &tokens[i] {
        let access = Tree::Access {
            line: line,
            object: Box::new(object),
            field: name.clone(),
        };
        Ok((i+1, access))
    } else {
        err(tokens[i].line(), format!("Expected a name after the dot but got {:?}", tokens[i]))
    }
}

fn parse_call(tokens: &[Token], i: usize, line: i64, func: Tree) -> Fallible<(usize, Tree)> {
    if let Token::Separator { name: ']', .. } = &tokens[i] {
        return Ok((i + 1, Tree::Call { line: line, func: Box::new(func), args: vec![] }));
    }

    let mut i = i;
    let mut args = vec![];
    loop {
        let (new_i, value) = parse_expr(tokens, i)?;
        i = new_i;
        args.push(value);

        match &tokens[i] {
            Token::Separator { name: ',', .. } => i += 1,
            Token::Separator { name: ']', .. } => {
                return Ok((i + 1, Tree::Call { line: line, func: Box::new(func), args: args }));
            }
            token => err(token.line(), format!("Expected a comma or a parenthesis but got {:?}", token))?
        }
    }
}

fn parse_operand(tokens: &[Token], i: usize) -> Fallible<(usize, Tree)> {
    let (mut i, mut value) = parse_atom(tokens, i)?;
    loop {
        match &tokens[i] {
            Token::Separator { line, name: '.' } => {
                (i, value) = parse_access(tokens, i + 1, *line, value)?;
            }
            Token::Separator { line, name: '[' } => {
                (i, value) = parse_call(tokens, i + 1, *line, value)?;
            }
            _ => return Ok((i, value))
        }
    }
}

fn parse_operation(tokens: &[Token], i: usize) -> Fallible<(usize, Tree)> {
    let (mut i, mut value) = parse_operand(tokens, i)?;
    loop {
        match &tokens[i] {
            Token::Operator { line, name } => {
                if !is_builtin_operator(name) {
                    err(*line, format!("There is no operator {}", name))?;
                }
                let (new_i, rhs) = parse_operand(tokens, i + 1)?;
                i = new_i;
                value = Tree::BinOp {
                    line: *line,
                    name: name.clone(),
                    lhs: Box::new(value),
                    rhs: Box::new(rhs),
                };
            }
            _ => return Ok((i, value))
        }
    }
}

fn parse_if(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    let (i, cond) = parse_expr(tokens, i)?;
    let (i, then) = parse_block(tokens, i)?;
    let cond = Box::new(cond);

    match &tokens[i] {
        Token::Else { .. } => {
            let (i, elsë) = parse_block(tokens, i + 1)?;
            Ok((i, Tree::If { line, cond, then, elsë }))
        }
        Token::Elif { line: elif_line } => {
            let (i, elif) = parse_if(tokens, i + 1, *elif_line)?;
            Ok((i, Tree::If { line, cond, then, elsë: vec![elif] }))
        }
        _ => Ok((i, Tree::If { line, cond, then, elsë: vec![] }))
    }
}

fn parse_expr(tokens: &[Token], i: usize) -> Fallible<(usize, Tree)> {
    match (&tokens[i], tokens.get(i + 1)) {
        (Token::Name { line, name }, Some(Token::Operator { name: op, .. })) if op == "=" => {
            let (i, value) = parse_operation(tokens, i + 2)?;
            Ok((i, Tree::Assign { line: *line, name: name.clone(), expr: Box::new(value) }))
        }
        _ => parse_operation(tokens, i)
    }
}

fn parse_block(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<Tree>)> {
    let mut i = expect(tokens, i, '{')?;
    let mut body = vec![];

    loop {
        if let Token::Separator { name: '}', .. } = &tokens[i] {
            return Ok((i + 1, body))
        }
        let (new_i, expr) = parse_expr(tokens, i)?;
        i = expect(tokens, new_i, ';')?;
        body.push(expr);
    }
}

fn parse_while(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    let (i, cond) = parse_expr(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Tree::While { line, cond: Box::new(cond), body }))
}

fn parse_for(tokens: &[Token], i: usize, line: i64, var1: String) -> Fallible<(usize, Tree)> {
    if let Token::Separator { name: ':', .. } = &tokens[i] {
        let (i, expr) = parse_expr(tokens, i + 1)?;
        let (i, body) = parse_block(tokens, i)?;
        Ok((i, Tree::For { line, key: None, value: var1, expr: Box::new(expr), body }))
    } else {
        let i = expect(tokens, i, ',')?;
        let (i, var2) = parse_ident(tokens, i)?;
        let i = expect(tokens, i, ':')?;
        let (i, expr) = parse_expr(tokens, i)?;
        let (i, body) = parse_block(tokens, i)?;
        Ok((i, Tree::For { line, key: Some(var1), value: var2, expr: Box::new(expr), body }))
    }
}

fn parse_import(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Decl)> {
    if let Token::Text { line, value } = &tokens[i] {
        Ok((i + 1, Decl::Import { line: *line, path: value.clone() }))
    } else {
        err(line, "Expected a string specifying the import path".into())
    }
}

fn parse_const(tokens: &[Token], i: usize, line: i64, name: String, private: bool) -> Fallible<(usize, Decl)> {
    if let Token::Operator { name: op, .. } = &tokens[i] && op == "=" {
        let (i, value) = parse_expr(tokens, i + 1)?;
        Ok((i, Decl::Const { line, name, expr: value, private }))
    } else {
        err(line, "Expected '=' in declaration".into())
    }
}

fn parse_declaration(tokens: &[Token], i: usize) -> Fallible<(usize, Decl)> {
    match &tokens[i] {
        Token::Import { line } => {
            parse_import(tokens, i + 1, *line)
        }
        Token::Private { line } => {
            let (i, name) = parse_ident(tokens, i + 1)?;
            parse_const(tokens, i, *line, name, true)
        }
        Token::Name { line, name } => {
            parse_const(tokens, i + 1, *line, name.clone(), false)
        }
        token => {
            err(token.line(), format!("Invalid start of a declaration: {:?}", token))
        }
    }
}

fn is_eof(token: &Token) -> bool {
    matches!(token, Token::Eof { .. })
}

fn parse_tokens(tokens: &[Token]) -> Fallible<Vec<Decl>> {
    let mut i = 0;
    let mut decls = vec![];
    while !is_eof(&tokens[i]) {
        let (new_i, decl) = parse_declaration(tokens, i)?;
        i = expect(tokens, new_i, ';')?;
        decls.push(decl);
    }
    Ok(decls)
}

pub fn parse(input: &str) -> Fallible<Vec<Decl>> {
    parse_tokens(&tokenize(input)?)
}


#[cfg(test)]
mod parser_tests {
    use super::*;
    use proptest::prelude::*;

    const KEYWORDS: [&str; 10] = ["true", "false", "fun", "if", "elif", "else", "while", "for", "import", "private"];
    const OPERATORS: [&str; 19] = [
        "*", "/", "%", "<<", ">>", ">>>", "+", "-", "&", "|", "^",
        "==", "!=", "<", "<=", ">", ">=", "||", "&&",
    ];

    fn show(tree: &Tree) -> String {
        match tree {
            Tree::Int { value, .. } => value.to_string(),
            Tree::Real { value, .. } => format!("{:?}", value),
            Tree::Text { value, .. } => format!("{:?}", value),
            Tree::Bool { value, .. } => value.to_string(),
            Tree::Record { fields, .. } => {
                let mut fields: Vec<_> = fields.iter().map(|(k, v)| format!(" ({} {})", k, show(v))).collect();
                fields.sort();
                format!("(record{})", fields.concat())
            }
            Tree::Var { ns: None, name, .. } => name.clone(),
            Tree::Var { ns: Some(ns), name, .. } => format!("{}::{}", ns, name),
            Tree::Call { func, args, .. } => {
                let args: String = args.iter().map(|a| format!(" {}", show(a))).collect();
                format!("(call {}{})", show(func), args)
            }
            Tree::BinOp { name, lhs, rhs, .. } => format!("({} {} {})", name, show(lhs), show(rhs)),
            Tree::Access { object, field, .. } => format!("(. {} {})", show(object), field),
            Tree::Lambda { args, ret, body, .. } => {
                let args: Vec<_> = args.iter().map(|(k, v)| format!("({} {})", k, show(v))).collect();
                format!("(fun [{}] {} {})", args.join(" "), show(ret), show_block(body))
            }
            Tree::Assign { name, expr, .. } => format!("(= {} {})", name, show(expr)),
            Tree::If { cond, then, elsë, .. } => format!("(if {} {} {})", show(cond), show_block(then), show_block(elsë)),
            Tree::While { cond, body, .. } => format!("(while {} {})", show(cond), show_block(body)),
            Tree::For { key, value, expr, body, .. } => {
                let key = key.as_deref().unwrap_or("_");
                format!("(for {} {} {} {})", key, value, show(expr), show_block(body))
            }
        }
    }

    fn show_block(body: &[Tree]) -> String {
        format!("{{{}}}", body.iter().map(show).collect::<Vec<_>>().join(" "))
    }

    fn show_decl(decl: &Decl) -> String {
        match decl {
            Decl::Import { path, .. } => format!("(import {:?})", path),
            Decl::Const { name, expr, private: false, .. } => format!("(const {} {})", name, show(expr)),
            Decl::Const { name, expr, private: true, .. } => format!("(private {} {})", name, show(expr)),
        }
    }

    fn file(src: &str) -> String {
        match parse(src) {
            Ok(decls) => decls.iter().map(show_decl).collect::<Vec<_>>().join(" "),
            Err(e) => panic!("{:?} should parse, got {:?}", src, e),
        }
    }

    fn ex(expr: &str) -> String {
        let src = format!("x = {};", expr);
        match parse(&src).as_deref() {
            Ok([Decl::Const { expr, .. }]) => show(expr),
            result => panic!("{:?} should parse to one constant, got {:?}", src, result),
        }
    }

    fn bad(src: &str) {
        let result = parse(src);
        assert!(result.is_err(), "{:?} should be rejected, got {:?}", src, result);
    }

    fn bad_ex(expr: &str) {
        bad(&format!("x = {};", expr));
    }

    fn print_expr(tree: &Tree) -> String {
        match tree {
            Tree::Assign { name, expr, .. } => format!("{} = {}", name, print_binop(expr)),
            _ => print_binop(tree),
        }
    }

    fn print_binop(tree: &Tree) -> String {
        match tree {
            Tree::BinOp { name, lhs, rhs, .. } => format!("{} {} {}", print_binop(lhs), name, print_operand(rhs)),
            _ => print_operand(tree),
        }
    }

    fn print_operand(tree: &Tree) -> String {
        match tree {
            Tree::Access { object, field, .. } => format!("{}.{}", print_object(object), field),
            Tree::Call { func, args, .. } => {
                let args: Vec<_> = args.iter().map(print_expr).collect();
                format!("{}[{}]", print_object(func), args.join(", "))
            }
            _ => print_atom(tree),
        }
    }

    fn print_object(tree: &Tree) -> String {
        match tree {
            Tree::Int { .. } => format!("({})", print_atom(tree)),
            _ => print_operand(tree),
        }
    }

    fn print_atom(tree: &Tree) -> String {
        match tree {
            Tree::Int { value, .. } => value.to_string(),
            Tree::Real { value, .. } => format!("{:?}", value),
            Tree::Text { value, .. } => format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\"")),
            Tree::Bool { value, .. } => value.to_string(),
            Tree::Var { ns: None, name, .. } => name.clone(),
            Tree::Var { ns: Some(ns), name, .. } => format!("{}::{}", ns, name),
            Tree::Record { fields, .. } => {
                let fields: Vec<_> = fields.iter().map(|(k, v)| format!("{}: {}", k, print_expr(v))).collect();
                format!("({})", fields.join(", "))
            }
            Tree::Lambda { args, ret, body, .. } => {
                let args: Vec<_> = args.iter().map(|(k, v)| format!("{}: {}", k, print_expr(v))).collect();
                format!("fun [{}] {} {}", args.join(", "), print_expr(ret), print_block(body))
            }
            Tree::If { cond, then, elsë, .. } if elsë.is_empty() => {
                format!("if {} {}", print_expr(cond), print_block(then))
            }
            Tree::If { cond, then, elsë, .. } => {
                format!("if {} {} else {}", print_expr(cond), print_block(then), print_block(elsë))
            }
            Tree::While { cond, body, .. } => format!("while {} {}", print_expr(cond), print_block(body)),
            Tree::For { key: None, value, expr, body, .. } => {
                format!("for {}: {} {}", value, print_expr(expr), print_block(body))
            }
            Tree::For { key: Some(key), value, expr, body, .. } => {
                format!("for {}, {}: {} {}", key, value, print_expr(expr), print_block(body))
            }
            Tree::BinOp { .. } | Tree::Assign { .. } | Tree::Access { .. } | Tree::Call { .. } => {
                format!("({})", print_expr(tree))
            }
        }
    }

    fn print_block(body: &[Tree]) -> String {
        format!("{{ {}}}", body.iter().map(|t| format!("{}; ", print_expr(t))).collect::<String>())
    }

    fn ident() -> impl Strategy<Value = String> {
        "[a-zA-Z][a-zA-Z0-9_]{0,3}".prop_filter("reserved word", |s| !KEYWORDS.contains(&s.as_str()))
    }

    fn block(tree: impl Strategy<Value = Tree>) -> impl Strategy<Value = Vec<Tree>> {
        prop::collection::vec(tree, 0..3)
    }

    fn tree() -> impl Strategy<Value = Tree> {
        let leaf = prop_oneof![
            (0..=i64::MAX).prop_map(|value| Tree::Int { line: 1, value }),
            (0u32..1_000_000, 0i32..4).prop_map(|(m, k)| Tree::Real { line: 1, value: m as f64 / 10f64.powi(k) }),
            any::<String>().prop_map(|value| Tree::Text { line: 1, value }),
            any::<bool>().prop_map(|value| Tree::Bool { line: 1, value }),
            (prop::option::of(ident()), ident()).prop_map(|(ns, name)| Tree::Var { line: 1, ns, name }),
        ];
        leaf.prop_recursive(4, 48, 3, |inner| prop_oneof![
            prop::collection::hash_map(ident(), inner.clone(), 0..3)
                .prop_map(|fields| Tree::Record { line: 1, fields }),
            (inner.clone(), block(inner.clone()))
                .prop_map(|(func, args)| Tree::Call { line: 1, func: Box::new(func), args }),
            (inner.clone(), prop::sample::select(OPERATORS.to_vec()), inner.clone())
                .prop_map(|(lhs, name, rhs)| Tree::BinOp {
                    line: 1, name: name.to_string(), lhs: Box::new(lhs), rhs: Box::new(rhs)
                }),
            (inner.clone(), ident())
                .prop_map(|(object, field)| Tree::Access { line: 1, object: Box::new(object), field }),
            (prop::collection::hash_map(ident(), inner.clone(), 0..3), inner.clone(), block(inner.clone()))
                .prop_map(|(args, ret, body)| Tree::Lambda {
                    line: 1, args: args.into_iter().collect(), ret: Box::new(ret), body
                }),
            (ident(), inner.clone())
                .prop_map(|(name, expr)| Tree::Assign { line: 1, name, expr: Box::new(expr) }),
            (inner.clone(), block(inner.clone()), block(inner.clone()))
                .prop_map(|(cond, then, elsë)| Tree::If { line: 1, cond: Box::new(cond), then, elsë }),
            (inner.clone(), block(inner.clone()))
                .prop_map(|(cond, body)| Tree::While { line: 1, cond: Box::new(cond), body }),
            (prop::option::of(ident()), ident(), inner.clone(), block(inner.clone()))
                .prop_map(|(key, value, expr, body)| Tree::For { line: 1, key, value, expr: Box::new(expr), body }),
        ])
    }

    fn token_soup() -> impl Strategy<Value = String> {
        let tokens = vec![
            "x", "ns", "1", "1.5", "\"s\"", "true", "(", ")", "[", "]", "{", "}", ".", ":", ",", ";",
            "=", "+", "==", "fun", "if", "elif", "else", "while", "for", "import", "private",
        ];
        (any::<bool>(), prop::collection::vec(prop::sample::select(tokens), 0..30)).prop_map(|(prefix, tokens)| {
            let prefix = if prefix { "x = " } else { "" };
            format!("{}{}", prefix, tokens.join(" "))
        })
    }

    proptest! {
        #[test]
        fn printed_tree_parses_back(tree in tree()) {
            let src = format!("x = {};", print_expr(&tree));
            let parsed = parse(&src);
            let parsed = parsed.as_deref();
            prop_assert!(matches!(parsed, Ok([Decl::Const { .. }])), "source: {:?}, got {:?}", src, parsed);
            if let Ok([Decl::Const { name, expr, private, .. }]) = parsed {
                prop_assert_eq!(name, "x");
                prop_assert!(!private);
                prop_assert_eq!(show(expr), show(&tree), "source: {:?}", src);
            }
        }

        #[test]
        fn parser_never_panics(src in token_soup()) {
            let _ = parse(&src);
        }
    }

    #[test]
    fn files() {
        assert_eq!(file(""), "");
        assert_eq!(file("# nothing\n# here"), "");
        assert_eq!(
            file("import \"a/b\"; x = 1; private y = 2; import \"c\";"),
            r#"(import "a/b") (const x 1) (private y 2) (import "c")"#
        );
        for src in ["x = 1", "x = 1;;", ";", "; x = 1;", "x = 1; y", "x = 1 y = 2;", "x = 1, y = 2;"] {
            bad(src);
        }
    }

    #[test]
    fn declarations() {
        assert_eq!(file("import \"\";"), r#"(import "")"#);
        assert_eq!(file("private x = a = 1;"), "(private x (= a 1))");
        for src in ["import p;", "import;", "import \"a\" \"b\";", "import (\"a\");", "import \"a\" + \"b\";",
                    "private private x = 1;", "private import \"a\";", "private;", "private x;",
                    "1 = x;", "\"x\" = 1;", "x;", "x 1;", "x == 1;", "x = ;", "ns::x = 1;", "x.y = 1;", "(x) = 1;"] {
            bad(src);
        }
    }

    #[test]
    fn assignments() {
        assert_eq!(ex("a = 1 + 2"), "(= a (+ 1 2))");
        assert_eq!(ex("(a = 1)"), "(= a 1)");
        assert_eq!(ex("f[a = 1, b = c]"), "(call f (= a 1) (= b c))");
        assert_eq!(ex("(k: a = 1)"), "(record (k (= a 1)))");
        assert_eq!(ex("a = (b = 1)"), "(= a (= b 1))");
        assert_eq!(ex("1 + (a = 2)"), "(+ 1 (= a 2))");
        for e in ["a = b = 1", "ns::a = 1", "1 + a = 2", "a.b = 1", "a[] = 1", "(a) = 1", "a = ", "= 1"] {
            bad_ex(e);
        }
    }

    #[test]
    fn binary_operators_are_left_associative_without_precedence() {
        assert_eq!(ex("1 + 2 * 3"), "(* (+ 1 2) 3)");
        assert_eq!(ex("1 * 2 + 3"), "(+ (* 1 2) 3)");
        assert_eq!(ex("1 - 2 - 3"), "(- (- 1 2) 3)");
        assert_eq!(ex("a || b && c"), "(&& (|| a b) c)");
        assert_eq!(ex("a == b < c != d"), "(!= (< (== a b) c) d)");
        assert_eq!(ex("1 + (2 * 3)"), "(+ 1 (* 2 3))");
        assert_eq!(ex("a.b + f[1] * c"), "(* (+ (. a b) (call f 1)) c)");
        assert_eq!(ex("a<<b>>c"), "(>> (<< a b) c)");
        for e in ["-1", "!a", "a + -1", "a +", "+", "a + + b", "~a", "a b"] {
            bad_ex(e);
        }
    }

    #[test]
    fn exactly_the_grammar_operators_are_accepted() {
        let op_chars: Vec<char> = "+-*/%&|~^<>=!".chars().collect();
        let mut ops: Vec<String> = op_chars.iter().map(|c| c.to_string()).collect();
        for _ in 0..2 {
            let longer: Vec<String> = ops.iter()
                .filter(|o| o.len() == ops.last().unwrap().len())
                .flat_map(|o| op_chars.iter().map(move |c| format!("{}{}", o, c)))
                .collect();
            ops.extend(longer);
        }
        assert_eq!(ops.len(), 13 + 13 * 13 + 13 * 13 * 13);

        for o in ops {
            let src = format!("x = 1 {} b;", o);
            assert_eq!(parse(&src).is_ok(), OPERATORS.contains(&o.as_str()), "source: {:?}", src);
        }
    }

    #[test]
    fn operand_suffixes() {
        assert_eq!(ex("a.b[1, 2].c[]"), "(call (. (call (. a b) 1 2) c))");
        assert_eq!(ex("f[][]"), "(call (call f))");
        assert_eq!(ex("f[g[1], (a: 2)]"), "(call f (call g 1) (record (a 2)))");
        assert_eq!(ex("(x).y"), "(. x y)");
        assert_eq!(ex("\"s\".len"), "(. \"s\" len)");
        assert_eq!(ex("(1).x"), "(. 1 x)");
        assert_eq!(ex("1.5.x"), "(. 1.5 x)");
        assert_eq!(ex("1[2]"), "(call 1 2)");
        assert_eq!(ex("ns::f[x].y"), "(. (call ns::f x) y)");
        assert_eq!(ex("().a"), "(. (record) a)");
        for e in ["a.1", "a.", "a..b", "f[1,]", "f[,]", "f[1 2]", "f[", "f]", "f[1", "a.(b)", "a.\"b\""] {
            bad_ex(e);
        }
    }

    #[test]
    fn literals() {
        assert_eq!(ex("0"), "0");
        assert_eq!(ex("9223372036854775807"), "9223372036854775807");
        assert_eq!(ex("2.5"), "2.5");
        assert_eq!(ex("\"a\\nb\""), "\"a\\nb\"");
        assert_eq!(ex("true"), "true");
        assert_eq!(ex("false"), "false");
    }

    #[test]
    fn records_and_parentheses() {
        assert_eq!(ex("()"), "(record)");
        assert_eq!(ex("(a)"), "a");
        assert_eq!(ex("((a))"), "a");
        assert_eq!(ex("(a::b)"), "a::b");
        assert_eq!(ex("(a : : b)"), "a::b");
        assert_eq!(ex("(a::b + 1)"), "(+ a::b 1)");
        assert_eq!(ex("(a: 1)"), "(record (a 1))");
        assert_eq!(ex("(a: b::c)"), "(record (a b::c))");
        assert_eq!(ex("((a: 1))"), "(record (a 1))");
        assert_eq!(ex("(b: 2, a: 1)"), "(record (a 1) (b 2))");
        assert_eq!(ex("(a: (b: ()))"), "(record (a (record (b (record)))))");
        for e in ["(a: 1,)", "(a: 1 b: 2)", "(1: 2)", "(a:)", "(a: 1, a: 2)", "(a", "(a: 1", "(,)", "(1, 2)",
                  "(a: 1, 2)", "(a: 1, b)", "(\"a\": 1)", "(a:: 1)"] {
            bad_ex(e);
        }
    }

    #[test]
    fn variables() {
        assert_eq!(ex("x"), "x");
        assert_eq!(ex("ns::x"), "ns::x");
        for e in ["a::b::c", "ns::", "ns::1", "ns:::x", "ns:x", "::x", "ns::(x)"] {
            bad_ex(e);
        }
    }

    #[test]
    fn lambdas() {
        assert_eq!(ex("fun [] t {}"), "(fun [] t {})");
        assert_eq!(
            ex("fun [x: int, f: fun [] int {}] ns::T { x; f[]; }"),
            "(fun [(x int) (f (fun [] int {}))] ns::T {x (call f)})"
        );
        assert_eq!(ex("fun [x: a + b] () {}"), "(fun [(x (+ a b))] (record) {})");
        assert_eq!(ex("fun [b: t, a: t] r {}"), "(fun [(b t) (a t)] r {})");
        assert_eq!(ex("fun [] if c {t;} {}"), "(fun [] (if c {t} {}) {})");
        for e in ["fun [x] int {}", "fun [] {}", "fun [x: int,] t {}", "fun [x: a, x: b] t {}", "fun x: int t {}",
                  "fun [] t", "fun [] t {1}", "fun t {}", "fun (x: int) t {}", "fun [x: int y: int] t {}"] {
            bad_ex(e);
        }
    }

    #[test]
    fn blocks() {
        assert_eq!(ex("while a {}"), "(while a {})");
        assert_eq!(ex("while a { 1; b = 2; if c {}; }"), "(while a {1 (= b 2) (if c {} {})})");
        assert_eq!(ex("while a { while b { c; }; }"), "(while a {(while b {c})})");
        for e in ["while a { 1 }", "while a { ; }", "while a { 1;; }", "while a", "while a { 1; ", "while a 1;"] {
            bad_ex(e);
        }
    }

    #[test]
    fn conditionals() {
        assert_eq!(ex("if a {1;}"), "(if a {1} {})");
        assert_eq!(ex("if a {1;} else {2;}"), "(if a {1} {2})");
        assert_eq!(ex("if a {1;} elif b {2;}"), "(if a {1} {(if b {2} {})})");
        assert_eq!(ex("if a {1;} elif b {2;} else {3;}"), "(if a {1} {(if b {2} {3})})");
        assert_eq!(ex("if a {} elif b {} elif c {} else {}"), "(if a {} {(if b {} {(if c {} {})})})");
        assert_eq!(ex("if a {1;} elif b {2;} else {3;}"), ex("if a {1;} else { if b {2;} else {3;}; }"));
        assert_eq!(ex("if if a {} {}"), "(if (if a {} {}) {} {})");
        for e in ["if a {} else if b {}", "elif a {}", "if a {} else {} else {}", "if a {} else {} elif b {}",
                  "if a", "if {}", "if a {} else", "if a {} elif {}", "else {}"] {
            bad_ex(e);
        }
    }

    #[test]
    fn loops() {
        assert_eq!(ex("while true { 1; }"), "(while true {1})");
        assert_eq!(ex("for v: arr { v; }"), "(for _ v arr {v})");
        assert_eq!(ex("for k, v: map { k; }"), "(for k v map {k})");
        assert_eq!(ex("for k , v : a::b {}"), "(for k v a::b {})");
        assert_eq!(ex("for v: f[1] + 2 {}"), "(for _ v (+ (call f 1) 2) {})");
        for e in ["for a, b, c: x {}", "for : x {}", "for x x {}", "for x: {}", "for x: y", "for x, : y {}",
                  "for (x): y {}", "for x; y {}", "for ns::x: y {}", "while {}"] {
            bad_ex(e);
        }
    }

    #[test]
    fn control_flow_is_an_operand() {
        assert_eq!(ex("if a {1;} + 1"), "(+ (if a {1} {}) 1)");
        assert_eq!(ex("1 + if a {1;} else {2;}"), "(+ 1 (if a {1} {2}))");
        assert_eq!(ex("while a {}.f"), "(. (while a {}) f)");
        assert_eq!(ex("for x: y {}[1]"), "(call (for _ x y {}) 1)");
        assert_eq!(ex("fun [] t {}[]"), "(call (fun [] t {}))");
        assert_eq!(ex("a = if b {} else {}"), "(= a (if b {} {}))");
    }

    #[test]
    fn keywords_are_not_names() {
        for kw in KEYWORDS {
            bad(&format!("{} = 1;", kw));
            bad(&format!("private {} = 1;", kw));
            if kw != "true" && kw != "false" {
                bad_ex(kw);
            }
            bad_ex(&format!("fun [{}: t] r {{}}", kw));
            bad_ex(&format!("({}: 1)", kw));
            bad_ex(&format!("r.{}", kw));
            bad_ex(&format!("{}::x", kw));
            bad_ex(&format!("ns::{}", kw));
            bad_ex(&format!("for {}: xs {{}}", kw));
            bad_ex(&format!("for {}, v: xs {{}}", kw));
            bad_ex(&format!("for k, {}: xs {{}}", kw));
            bad_ex(&format!("fun [] t {{ {} = 1; }}", kw));
        }
    }

    fn var(line: i64, name: &str) -> Tree {
        Tree::Var { line, ns: None, name: name.to_string() }
    }

    #[test]
    fn node_lines() {
        let src = [
            "x",                 // 1
            "=",                 // 2
            "g = f",             // 3
            "[a",                // 4
            ".b",                // 5
            "+",                 // 6
            "(k: 1)];",          // 7
            "z = fun",           // 8
            "[p: t]",            // 9
            "r {",               // 10
            "if c {",            // 11
            "1;",                // 12
            "} elif d {",        // 13
            "2.5;",              // 14
            "} else {",          // 15
            "while e {",         // 16
            "for k, v: m {};",   // 17
            "};",                // 18
            "};",                // 19
            "};",                // 20
            "private w = \"s\";" // 21
        ].join("\n");

        let x = Decl::Const { line: 1, name: "x".to_string(), private: false, expr: Tree::Assign {
            line: 3,
            name: "g".to_string(),
            expr: Box::new(Tree::Call {
                line: 4,
                func: Box::new(var(3, "f")),
                args: vec![Tree::BinOp {
                    line: 6,
                    name: "+".to_string(),
                    lhs: Box::new(Tree::Access { line: 5, object: Box::new(var(4, "a")), field: "b".to_string() }),
                    rhs: Box::new(Tree::Record {
                        line: 7,
                        fields: HashMap::from([("k".to_string(), Tree::Int { line: 7, value: 1 })]),
                    }),
                }],
            }),
        }};
        let z = Decl::Const { line: 8, name: "z".to_string(), private: false, expr: Tree::Lambda {
            line: 8,
            args: vec![("p".to_string(), var(9, "t"))],
            ret: Box::new(var(10, "r")),
            body: vec![Tree::If {
                line: 11,
                cond: Box::new(var(11, "c")),
                then: vec![Tree::Int { line: 12, value: 1 }],
                elsë: vec![Tree::If {
                    line: 13,
                    cond: Box::new(var(13, "d")),
                    then: vec![Tree::Real { line: 14, value: 2.5 }],
                    elsë: vec![Tree::While {
                        line: 16,
                        cond: Box::new(var(16, "e")),
                        body: vec![Tree::For {
                            line: 17,
                            key: Some("k".to_string()),
                            value: "v".to_string(),
                            expr: Box::new(var(17, "m")),
                            body: vec![],
                        }],
                    }],
                }],
            }],
        }};
        let w = Decl::Const {
            line: 21, name: "w".to_string(), private: true, expr: Tree::Text { line: 21, value: "s".to_string() }
        };
        assert_eq!(parse(&src), Ok(vec![x, z, w]));
    }

    #[test]
    fn error_lines() {
        assert_eq!(parse("x = 1\ny = 2;").unwrap_err().line, 2);
        assert_eq!(parse("x = f[1\n2];").unwrap_err().line, 2);
        assert_eq!(parse("\n\nx = ;").unwrap_err().line, 3);
        assert_eq!(parse("x = 1;\n\n\n+").unwrap_err().line, 4);
        assert_eq!(parse("x = a.\n\n1;").unwrap_err().line, 3);
    }
}
