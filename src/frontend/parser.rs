use std::collections::{HashMap, HashSet};

use crate::frontend::builtin::is_builtin_operator;
use crate::frontend::error::{err, Fallible};
use crate::frontend::lexer::{Token, tokenize};

// <file> ::= (<decl> ";")*
// <decl> ::= <import> | <const>
// <expr> ::= (<name> "=")? <binop>
// <binop> ::= <operand> (<operator> <operand>)*
// <operand> ::= <atom> ("." <name> | "[" (<expr> ("," <expr>)*)? "]")*
// <atom> ::= <int> | <real> | <text> | <bool> | <record> | <var> | <lambda> | <if> | <while> | <for>
// <record> ::= "(" ")" | "(" <name> ":" <expr> ("," <name> ":" <expr>)* ")"
// <var> ::= <name> ("::" <name>)?
// <lambda> ::= "fun" "[" (<name> ":" <expr> ("," <name> ":" <expr>)*)? "]" <expr> <block>
// <block> ::= "{" (<expr> ";")* "}"
// <if> ::= "if" <expr> <block> ("elif" <expr> <block>)* ("else" <block>)?
// <while> ::= "while" <expr> <block>
// <for> ::= "for" <name> ("," <name>)? ":" <expr> <block>

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

fn parse_atom(tokens: &[Token], i: usize) -> Fallible<(usize, Tree)> {
    match &tokens[i] {
        Token::Int { line, value } => Ok((i + 1, Tree::Int { line: *line, value: *value })),
        Token::Real { line, value } => Ok((i + 1, Tree::Real { line: *line, value: *value })),
        Token::Text { line, value } => Ok((i + 1, Tree::Text { line: *line, value: value.clone() })),
        Token::Bool { line, value } => Ok((i + 1, Tree::Bool { line: *line, value: *value })),
        Token::Separator { line, name: '(' } => parse_parentheses(tokens, i + 1, *line),
        Token::Name { line, name } if name == "fun" => parse_fun(tokens, i + 1, *line),
        Token::Name { line, name } if name == "if" => parse_if(tokens, i + 1, *line),
        Token::Name { line, name } if name == "while" => parse_while(tokens, i + 1, *line),
        Token::Name { line, name } if name == "for" => {
            if let Token::Name { name: var, .. } = &tokens[i + 1] {
                parse_for(tokens, i + 2, *line, var.clone())
            } else {
                err(*line, "Expected a name after 'for'".to_string())
            }
        }
        Token::Name { line, name } => parse_var(tokens, i + 1, *line, name.clone()),
        token => err(token.line(), format!("Expected a value, but got {:?}", token))
    }
}

fn parse_parentheses(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Tree)> {
    if let Token::Separator { name: ')', .. } = &tokens[i] {
        Ok((i + 1, Tree::Record { line, fields: HashMap::new() }))
    } else if let Some(Token::Separator { name: ':', .. }) = tokens.get(i + 1) {
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
        err(line, format!("Expected a name after the dot"))?
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

    if let Token::Name { name, .. } = &tokens[i] && name == "else" {
        let (i, elsë) = parse_block(tokens, i + 1)?;
        Ok((i, Tree::If { line, cond, then, elsë }))
    } else if let Token::Name { name, .. } = &tokens[i] && name == "elif" {
        let (i, elif) = parse_if(tokens, i + 1, line)?;
        Ok((i, Tree::If { line, cond, then, elsë: vec![elif] }))
    } else {
        Ok((i, Tree::If { line, cond, then, elsë: vec![] }))
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
        i = if let Token::Separator { name: ';', .. } = &tokens[new_i] {
            new_i + 1
        } else {
            new_i
        };
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
        if let Token::Name { name: var2, .. } = &tokens[i] {
            let i = expect(tokens, i + 1, ':')?;
            let (i, expr) = parse_expr(tokens, i)?;
            let (i, body) = parse_block(tokens, i)?;
            Ok((i, Tree::For { line, key: Some(var1), value: var2.clone(), expr: Box::new(expr), body }))
        } else {
            err(line, "Expected a variable name".into())
        }
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
        Token::Name { line, name } if name == "import" => {
            parse_import(tokens, i + 1, *line)
        }
        Token::Name { line, name } if name == "private" => {
            if let Token::Name { name, .. } = &tokens[i + 1] {
                parse_const(tokens, i + 2, *line, name.clone(), true)
            } else {
                err(*line, "Expected a name after 'private'".into())
            }
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
    use std::collections::HashMap;

    #[test]
    fn test_empty_file() {
        let got = parse("").expect("empty input should parse");
        assert!(got.is_empty(), "empty input should produce zero declarations");
    }

    #[test]
    fn test_errors() {
        parse("x = 1").expect_err("missing semicolon");
        parse("x = for +item : vec {};").expect_err("unexpected operator");
        parse("x = for k, +v : map {};").expect_err("unexpected operator");
        parse("x = +123").expect_err("no unary operators");
        parse("x = (a: 5, a: 6);").expect_err("duplicate name");
        parse("x = (a: 5 b: 6);").expect_err("expected a comma");
        parse("x = record.+field;").expect_err("expected a name");
        parse("x = (a: 5, \"b\": 6);").expect_err("expected a name");
        parse("x = 123 ^^^ 123;").expect_err("invalid operator");
        parse("x = sqrt[3.14 y];").expect_err("unexpected name");
        parse("123 = x;").expect_err("invalid decl");
        parse("private 123 = x;").expect_err("extremely invalid decl");
        parse("x = (a: 1,").expect_err("unterminated record");
        parse("if true { 1").expect_err("missing closing brace");
        parse("for : arr { }").expect_err("missing loop variable");
        parse("import").expect_err("incomplete import");
    }

    #[test]
    fn test_import_and_const() {
        let src = r#" import "lib/foo"; x = 123; "#;

        let parsed = parse(src).expect("parse failed");
        assert_eq!(parsed.len(), 2);

        assert!(matches!(parsed[0].clone(), Decl::Import {
            path, ..
        } if path == "lib/foo"));

        assert!(matches!(parsed[1].clone(), Decl::Const {
            name,
            expr: Tree::Int { value, .. },
            ..
        } if name == "x" && value == 123));
    }

    #[test]
    fn test_atoms_int_real_text_bool() {
        use Decl::Const;
        use Tree::{Int, Real, Text, Bool};

        let src = r#"
            private a = 123;
            b = 3.14;
            c = "tri";
            d = true;
        "#;

        let parsed = parse(src).expect("parse atoms");
        assert_eq!(parsed.len(), 4);

        assert!(matches!(&parsed[0], Const { name, expr: Int { value: 123, .. }, private: true, .. } if name == "a"));
        assert!(matches!(&parsed[1], Const { name, expr: Real { value: 3.14, .. }, private: false, .. } if name == "b"));
        assert!(matches!(&parsed[2], Const { name, expr: Text { value, .. }, private: false, .. } if name == "c" && value == "tri"));
        assert!(matches!(&parsed[3], Const { name, expr: Bool { value: true, .. }, private: false, .. } if name == "d"));
    }

    #[test]
    fn test_records_empty_and_fields() {
        let src = r#" r0 = (); r1 = (a: 123, b: 3.14); "#;
        let parsed = parse(src).expect("parse records");

        let mut map = HashMap::new();
        for d in parsed.iter() {
            if let Decl::Const { name, expr, .. } = d {
                map.insert(name.clone(), expr.clone());
            }
        }

        assert_eq!(map.get("r0"), Some(&Tree::Record { line: 1, fields: HashMap::new() }));

        let mut fields = HashMap::new();
        fields.insert("a".to_string(), Tree::Int { line: 1, value: 123 });
        fields.insert("b".to_string(), Tree::Real { line: 1, value: 3.14 });
        assert_eq!(map.get("r1"), Some(&Tree::Record { line: 1, fields }));
    }

    #[test]
    fn test_var_with_namespace_and_access() {
        let src = r#" v = ns::id; a = ns::obj.field; "#;
        let parsed = parse(src).expect("parse ns var and access");

        if let Decl::Const { name, expr, .. } = &parsed[0] && name == "v" {
            assert_eq!(*expr, Tree::Var { line: 1, ns: Some("ns".to_string()), name: "id".to_string() });
        } else {
            panic!("v not parsed correctly")
        }

        if let Decl::Const { name, expr, .. } = &parsed[1] && name == "a" {
            let obj = Tree::Var { line: 1, ns: Some("ns".to_string()), name: "obj".to_string() };
            assert_eq!(*expr, Tree::Access { line: 1, object: Box::new(obj), field: "field".to_string() });
        } else {
            panic!("a not parsed correctly")
        }
    }

    #[test]
    fn test_binop_left_associative_and_assignment_expr() {
        // Tests: assignment with chain binops a = 1 + 2 + 3
        let src = r#" assign_me = a = (1 + 2) + 3; "#;
        let parsed = parse(src).expect("parse binop assign");

        let c = match parsed[0].clone() {
            Decl::Const { name, expr, .. } if name == "assign_me" => expr,
          _ => panic!("assign_me const not found"),
        };

        match c {
            Tree::Assign { name, expr, .. } => {
                assert_eq!(name, "a");

                match *expr {
                    Tree::BinOp { name: op1, lhs, rhs, .. } => {
                        assert_eq!(op1, "+");
                        match *lhs {
                            Tree::BinOp { name: op2, lhs: lhs2, rhs: rhs2, .. } => {
                                assert_eq!(op2, "+");
                                assert_eq!(*lhs2, Tree::Int { line: 1, value: 1 });
                                assert_eq!(*rhs2, Tree::Int { line: 1, value: 2 });
                            }
                            _ => panic!("left of top-level + is not a binop"),
                        }
                        assert_eq!(*rhs, Tree::Int { line: 1, value: 3 });
                    }
                    _ => panic!("assignment rhs not a BinOp"),
                }
            }
            _ => panic!("expected assignment expression"),
        }
    }

    #[test]
    fn test_access_and_indexing_chained() {
        let src = r#" x = a.b[1, 2].c[]; "#;
        let parsed = parse(src).expect("parse access/indexing chain");
        let expr = parsed.into_iter().find_map(|d| match d {
            Decl::Const { name, expr, .. } if name == "x" => Some(expr),
            _ => None,
        }).expect("const x missing");

        let expected = Tree::Call {
            line: 1, func: Box::new(Tree::Access {
                line: 1,
                object: Box::new(Tree::Call {
                    line: 1,
                    func: Box::new(Tree::Access { 
                        line: 1,
                        object: Box::new(Tree::Var {
                            line: 1,
                            ns: None,
                            name: "a".to_string()
                        }),
                        field: "b".to_string()
                    }),
                    args: vec![
                        Tree::Int { line: 1, value: 1 },
                        Tree::Int { line: 1, value: 2 }
                    ],
                }),
                field: "c".to_string()
            }), args: vec![]
        };
        assert_eq!(expr.clone(), expected);
    }

    #[test]
    fn test_if_else_chain() {
        let src = r#"
            c = if true { 1; } elif false { 2; } else { 3; };
            d = if true { 1; };
        "#;
        let parsed = parse(src).expect("parse if else chain");
        assert_eq!(parsed.len(), 2);

        let c = match &parsed[0] {
            Decl::Const { name, expr, .. } if name == "c" => expr.clone(),
            _ => panic!("c expected")
        };

        let d = match &parsed[1] {
            Decl::Const { name, expr, .. } if name == "d" => expr.clone(),
            _ => panic!("d expected")
        };

        match c {
            Tree::If { cond, then, elsë, .. } => {
                assert!(matches!(*cond, Tree::Bool { value: true, .. }));
                assert!(matches!(&then[..], &[Tree::Int { value: 1, .. }]));
                assert_eq!(elsë.len(), 1);
                match elsë[0].clone() {
                    Tree::If { cond, then, elsë, .. } => {
                        assert!(matches!(*cond, Tree::Bool { value: false, .. }));
                        assert!(matches!(&then[..], &[Tree::Int { value: 2, .. }]));
                        assert!(matches!(&elsë[..], &[Tree::Int { value: 3, .. }]));
                    }
                    _ => panic!("should be if")
                }
            }
            _ => panic!("should be if")
        }

        match d {
            Tree::If { cond, then, elsë, .. } => {
                assert!(matches!(*cond, Tree::Bool { value: true, .. }));
                assert!(matches!(&then[..], &[Tree::Int { value: 1, .. }]));
                assert!(elsë.is_empty());
            }
            _ => panic!("should be if")
        }
    }

    #[test]
    fn test_while_and_for_loops() {
        let src = r#" w = while true { 1 }; f1 = for v: arr { v }; f2 = for k, v: map { k }; "#;
        let parsed = parse(src).expect("parse loops");
        let mut found = std::collections::HashMap::new();
        for d in parsed.iter() {
            if let Decl::Const { name, expr, .. } = d {
                found.insert(name.clone(), expr.clone());
            }
        }

        match found.get("w") {
            Some(Tree::While { cond, body, .. }) => {
                assert_eq!(**cond, Tree::Bool { line: 1, value: true });
                assert_eq!(body, &vec![Tree::Int { line: 1, value: 1 }]);
            }
            _ => panic!("w not parsed into While"),
        }

        match found.get("f1") {
            Some(Tree::For { key, value, expr, body, .. }) => {
                assert_eq!(key, &None);
                assert_eq!(value, "v");
                assert_eq!(**expr, Tree::Var { line: 1, ns: None, name: "arr".to_string() });
                assert_eq!(body, &vec![Tree::Var { line: 1, ns: None, name: "v".to_string() }]);
            }
            _ => panic!("f1 not parsed into For"),
        }

        match found.get("f2") {
            Some(Tree::For { key, value, expr, body, .. }) => {
                assert_eq!(key, &Some("k".to_string()));
                assert_eq!(value, "v");
                assert_eq!(**expr, Tree::Var { line: 1, ns: None, name: "map".to_string() });
                assert_eq!(body, &vec![Tree::Var { line: 1, ns: None, name: "k".to_string() }]);
            }
            _ => panic!("f2 not parsed into For"),
        }
    }

    #[test]
    fn test_lambda_parsing_structure() {
        // We assert structural properties rather than exact equality of ret field.
        let src = r#" L = fun [x: int, y: real] int { x = x + 1 }; "#;
        let parsed = parse(src).expect("parse lambda");
        let lambda_expr = parsed.into_iter().find_map(|d| match d {
            Decl::Const { name, expr, .. } if name == "L" => Some(expr),
            _ => None,
        }).expect("const L not found");

        match lambda_expr.clone() {
            Tree::Lambda { args, ret: _rt, body, .. } => {
                // args should include x and y with their types (we used 123 and 3.14 as dummy type expressions in source)
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].0, "x");
                assert_eq!(args[1].0, "y");
                // body should contain an assignment expression
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Tree::Assign { name, expr, .. } => {
                        assert_eq!(name, "x");
                        // rhs should be BinOp
                        match **expr {
                            Tree::BinOp { name: ref op, .. } => assert_eq!(op, "+"),
                            _ => panic!("lambda assignment rhs is not binop"),
                        }
                    }
                    _ => panic!("lambda body not an assign"),
                }
            }
            _ => panic!("expected Lambda expression"),
        }
    }

    #[test]
    fn test_multiple_declarations_and_block_with_multiple_exprs() {
        let src = r#"
            import "top";
            a = 1;
            b = fun [] () { 1; 2; 3; };
            c = 3.14;
        "#;
        let parsed = parse(src).expect("parse multi-decls");

        assert_eq!(parsed.len(), 4);

        match &parsed[0] {
            Decl::Import { path, .. } => assert_eq!(path, "top"),
            _ => panic!("should be import")
        }

        match &parsed[2] {
            Decl::Const { name, expr: Tree::Lambda { body, .. }, .. } => {
                assert_eq!(name, "b");
                assert_eq!(body.len(), 3);
                assert!(matches!(body[0], Tree::Int { value: 1, .. }));
                assert!(matches!(body[1], Tree::Int { value: 2, .. }));
                assert!(matches!(body[2], Tree::Int { value: 3, .. }));
            }
            _ => panic!("should be lambda")
        }
    }
}
