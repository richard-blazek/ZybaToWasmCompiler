use std::collections::{HashMap, HashSet};

use crate::builtin::is_builtin_operator;
use crate::error::{err, Fallible};
use crate::lexer::{Token, tokenize};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Expr> },
    Var { line: i64, ns: Option<String>, name: String },
    Call { line: i64, func: Box<Expr>, args: Vec<Expr> },
    BinOp { line: i64, name: String, lhs: Box<Expr>, rhs: Box<Expr> },
    Access { line: i64, object: Box<Expr>, field: String },
    Lambda { line: i64, args: Vec<(String, Expr)>, return_type: Box<Expr>, body: Vec<Expr> },
    Assign { line: i64, name: String, expr: Box<Expr> },
    If { line: i64, cond: Box<Expr>, then: Vec<Expr>, otherwise: Vec<Expr> },
    While { line: i64, cond: Box<Expr>, body: Vec<Expr> },
    For { line: i64, key: Option<String>, value: String, expr: Box<Expr>, body: Vec<Expr> }
}

impl Expr {
    pub fn line(&self) -> i64 {
        use Expr::*;
        match self {
            Int { line, .. } | Real { line, .. } | Text { line, .. }
            | Bool { line, .. } | Record { line, .. } | Var { line, .. }
            | Call { line, .. } | BinOp { line, .. } | Access { line, .. }
            | Lambda { line, .. } | Assign { line, .. } | If { line, .. }
            | While { line, .. } | For { line, .. } => *line
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Import { line: i64, path: String },
    Const { line: i64, name: String, expr: Expr, private: bool },
}

fn expect(tokens: &[Token], i: usize, sep: char) -> Fallible<usize> {
    if let Token::Separator { name: c, .. } = tokens[i] && c == sep {
        Ok(i + 1)
    } else {
        err(tokens[i].line(), format!("Expected {:?} but got {:?}", sep, tokens[i]))
    }
}

fn parse_atom(tokens: &[Token], i: usize) -> Fallible<(usize, Expr)> {
    match &tokens[i] {
        Token::Int { line, value } => Ok((i + 1, Expr::Int { line: *line, value: *value })),
        Token::Real { line, value } => Ok((i + 1, Expr::Real { line: *line, value: *value })),
        Token::Text { line, value } => Ok((i + 1, Expr::Text { line: *line, value: value.clone() })),
        Token::Bool { line, value } => Ok((i + 1, Expr::Bool { line: *line, value: *value })),
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

fn parse_parentheses(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Expr)> {
    if let Token::Separator { name: ')', .. } = &tokens[i] {
        Ok((i + 1, Expr::Record { line, fields: HashMap::new() }))
    } else if let Some(Token::Separator { name: ':', .. }) = tokens.get(i + 1) {
        parse_record(tokens, i, line)
    } else {
        let (i, value) = parse_expr(tokens, i)?;
        let i = expect(tokens, i, ')')?;
        Ok((i, value))
    }
}

fn parse_key_value_pairs(tokens: &[Token], i: usize, end: char) -> Fallible<(usize, Vec<(String, Expr)>)> {
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

fn parse_record(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Expr)> {
    let (i, fields) = parse_key_value_pairs(tokens, i, ')')?;
    Ok((i, Expr::Record { line: line, fields: fields.into_iter().collect() }))
}

fn parse_var(tokens: &[Token], i: usize, line: i64, first_name: String) -> Fallible<(usize, Expr)> {
    if let (Some(Token::Separator { name: ':', .. }),
            Some(Token::Separator { name: ':', .. }),
            Some(Token::Name { name, .. })) = (tokens.get(i), tokens.get(i + 1), tokens.get(i + 2)) {
        Ok((i + 3, Expr::Var { line, ns: Some(first_name), name: name.clone() }))
    } else {
        Ok((i, Expr::Var { line, ns: None, name: first_name}))
    }
}

fn parse_arguments(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<(String, Expr)>)> {
    let i = expect(tokens, i, '[')?;
    parse_key_value_pairs(tokens, i, ']')
}

fn parse_fun(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Expr)> {
    let (i, args) = parse_arguments(tokens, i)?;
    let (i, ret) = parse_expr(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Expr::Lambda { line, args, return_type: Box::new(ret), body }))
}

fn parse_access(tokens: &[Token], i: usize, line: i64, object: Expr) -> Fallible<(usize, Expr)> {
    if let Token::Name { name, .. } = &tokens[i] {
        let access = Expr::Access {
            line: line,
            object: Box::new(object),
            field: name.clone(),
        };
        Ok((i+1, access))
    } else {
        err(line, format!("Expected a name after the dot"))?
    }
}

fn parse_call(tokens: &[Token], i: usize, line: i64, func: Expr) -> Fallible<(usize, Expr)> {
    if let Token::Separator { name: ']', .. } = &tokens[i] {
        return Ok((i + 1, Expr::Call { line: line, func: Box::new(func), args: vec![] }));
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
                return Ok((i + 1, Expr::Call { line: line, func: Box::new(func), args: args }));
            }
            token => err(token.line(), format!("Expected a comma or a parenthesis but got {:?}", token))?
        }
    }
}

fn parse_operand(tokens: &[Token], i: usize) -> Fallible<(usize, Expr)> {
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

fn parse_operation(tokens: &[Token], i: usize) -> Fallible<(usize, Expr)> {
    let (mut i, mut value) = parse_operand(tokens, i)?;
    loop {
        match &tokens[i] {
            Token::Operator { line, name } => {
                if !is_builtin_operator(name) {
                    err(*line, format!("There is no operator {}", name))?;
                }
                let (new_i, rhs) = parse_operand(tokens, i + 1)?;
                i = new_i;
                value = Expr::BinOp {
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

fn parse_if(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Expr)> {
    let (i, cond) = parse_expr(tokens, i)?;
    let (i, then) = parse_block(tokens, i)?;
    let cond = Box::new(cond);

    if let Token::Name { name, .. } = &tokens[i] && name == "else" {
        let (i, otherwise) = parse_block(tokens, i + 1)?;
        Ok((i, Expr::If { line, cond, then, otherwise }))
    } else if let Token::Name { name, .. } = &tokens[i] && name == "elif" {
        let (i, elif) = parse_if(tokens, i + 1, line)?;
        Ok((i, Expr::If { line, cond, then, otherwise: vec![elif] }))
    } else {
        Ok((i, Expr::If { line, cond, then, otherwise: vec![] }))
    }
}

fn parse_expr(tokens: &[Token], i: usize) -> Fallible<(usize, Expr)> {
    match (&tokens[i], tokens.get(i + 1)) {
        (Token::Name { line, name }, Some(Token::Operator { name: op, .. })) if op == "=" => {
            let (i, value) = parse_operation(tokens, i + 2)?;
            Ok((i, Expr::Assign { line: *line, name: name.clone(), expr: Box::new(value) }))
        }
        _ => parse_operation(tokens, i)
    }
}

fn parse_block(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<Expr>)> {
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

fn parse_while(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Expr)> {
    let (i, cond) = parse_expr(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Expr::While { line, cond: Box::new(cond), body }))
}

fn parse_for(tokens: &[Token], i: usize, line: i64, var1: String) -> Fallible<(usize, Expr)> {
    if let Token::Separator { name: ':', .. } = &tokens[i] {
        let (i, expr) = parse_expr(tokens, i + 1)?;
        let (i, body) = parse_block(tokens, i)?;
        Ok((i, Expr::For { line, key: None, value: var1, expr: Box::new(expr), body }))
    } else {
        let i = expect(tokens, i, ',')?;
        if let Token::Name { name: var2, .. } = &tokens[i] {
            let i = expect(tokens, i + 1, ':')?;
            let (i, expr) = parse_expr(tokens, i)?;
            let (i, body) = parse_block(tokens, i)?;
            Ok((i, Expr::For { line, key: Some(var1), value: var2.clone(), expr: Box::new(expr), body }))
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
    if let Token::Eof { .. } = token {
        true
    } else {
        false
    }
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
