use std::collections::{HashMap, HashSet};

use crate::builtin::is_builtin_operator;
use crate::error::{err, Fallible};
use crate::lexer::{Token, tokenize};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int { line: i64, value: i64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Value> },
    Var { line: i64, ns: Option<String>, name: String },
    Call { line: i64, func: Box<Value>, args: Vec<Value> },
    BinOp { line: i64, name: String, lhs: Box<Value>, rhs: Box<Value> },
    Access { line: i64, object: Box<Value>, field: String },
    Lambda { line: i64, args: Vec<(String, Value)>, return_type: Box<Value>, body: Vec<Statement> }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Value { line: i64, value: Value },
    Assign { line: i64, name: String, value: Value },
    If { line: i64, cond: Value, then: Vec<Statement>, otherwise: Vec<Statement> },
    While { line: i64, cond: Value, body: Vec<Statement> },
    For { line: i64, key: Option<String>, value: String, expr: Value, body: Vec<Statement> },
    Return { line: i64, value: Value }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Import { line: i64, path: String },
    Const { line: i64, name: String, value: Value, private: bool },
}

fn expect(tokens: &[Token], i: usize, sep: char) -> Fallible<usize> {
    if let Token::Separator { name: c, .. } = tokens[i] && c == sep {
        Ok(i + 1)
    } else {
        err(tokens[i].line(), format!("Expected {:?} but got {:?}", sep, tokens[i]))
    }
}

fn parse_atom(tokens: &[Token], i: usize) -> Fallible<(usize, Value)> {
    match &tokens[i] {
        Token::Int { line, value } => Ok((i + 1, Value::Int { line: *line, value: *value })),
        Token::Real { line, value } => Ok((i + 1, Value::Real { line: *line, value: *value })),
        Token::Text { line, value } => Ok((i + 1, Value::Text { line: *line, value: value.clone() })),
        Token::Bool { line, value } => Ok((i + 1, Value::Bool { line: *line, value: *value })),
        Token::Separator { line, name: '(' } => parse_parentheses(tokens, i + 1, *line),
        Token::Name { line, name } if name == "fun" => parse_fun(tokens, i + 1, *line),
        Token::Name { line, name } => parse_var(tokens, i + 1, *line, name.clone()),
        token => err(token.line(), format!("Expected a value, but got {:?}", token))
    }
}

fn parse_parentheses(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Value)> {
    if let Token::Separator { name: ')', .. } = &tokens[i] {
        Ok((i + 1, Value::Record { line, fields: HashMap::new() }))
    } else if let Some(Token::Separator { name: ':', .. }) = tokens.get(i + 1) {
        parse_record(tokens, i, line)
    } else {
        let (i, value) = parse_value(tokens, i)?;
        let i = expect(tokens, i, ')')?;
        Ok((i, value))
    }
}

fn parse_key_value_pairs(tokens: &[Token], i: usize, end: char) -> Fallible<(usize, Vec<(String, Value)>)> {
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
            let (new_i, value) = parse_value(tokens, i + 2)?;
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

fn parse_record(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Value)> {
    let (i, fields) = parse_key_value_pairs(tokens, i, ')')?;
    Ok((i, Value::Record { line: line, fields: fields.into_iter().collect() }))
}

fn parse_var(tokens: &[Token], i: usize, line: i64, first_name: String) -> Fallible<(usize, Value)> {
    if let (Some(Token::Separator { name: ':', .. }),
            Some(Token::Separator { name: ':', .. }),
            Some(Token::Name { name, .. })) = (tokens.get(i), tokens.get(i + 1), tokens.get(i + 2)) {
        Ok((i + 3, Value::Var { line, ns: Some(first_name), name: name.clone() }))
    } else {
        Ok((i, Value::Var { line, ns: None, name: first_name}))
    }
}

fn parse_arguments(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<(String, Value)>)> {
    let i = expect(tokens, i, '[')?;
    parse_key_value_pairs(tokens, i, ']')
}

fn parse_fun(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Value)> {
    let (i, args) = parse_arguments(tokens, i)?;
    let (i, ret) = parse_value(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Value::Lambda { line, args, return_type: Box::new(ret), body }))
}

fn parse_access(tokens: &[Token], i: usize, line: i64, object: Value) -> Fallible<(usize, Value)> {
    if let Token::Name { name, .. } = &tokens[i] {
        let access = Value::Access {
            line: line,
            object: Box::new(object),
            field: name.clone(),
        };
        Ok((i+1, access))
    } else {
        err(line, format!("Expected a name after the dot"))?
    }
}

fn parse_call(tokens: &[Token], i: usize, line: i64, func: Value) -> Fallible<(usize, Value)> {
    if let Token::Separator { name: ']', .. } = &tokens[i] {
        return Ok((i + 1, Value::Call { line: line, func: Box::new(func), args: vec![] }));
    }

    let mut i = i;
    let mut args = vec![];
    loop {
        let (new_i, value) = parse_value(tokens, i)?;
        i = new_i;
        args.push(value);

        match &tokens[i] {
            Token::Separator { name: ',', .. } => i += 1,
            Token::Separator { name: ']', .. } => {
                return Ok((i + 1, Value::Call { line: line, func: Box::new(func), args: args }));
            }
            token => err(token.line(), format!("Expected a comma or a parenthesis but got {:?}", token))?
        }
    }
}

fn parse_operand(tokens: &[Token], i: usize) -> Fallible<(usize, Value)> {
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

fn parse_value(tokens: &[Token], i: usize) -> Fallible<(usize, Value)> {
    let (mut i, mut value) = parse_operand(tokens, i)?;
    loop {
        match &tokens[i] {
            Token::Operator { line, name } => {
                if !is_builtin_operator(name) {
                    err(*line, format!("There is no operator {}", name))?;
                }
                let (new_i, rhs) = parse_operand(tokens, i + 1)?;
                i = new_i;
                value = Value::BinOp {
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

fn parse_if(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Statement)> {
    let (i, cond) = parse_value(tokens, i)?;
    let (i, then) = parse_block(tokens, i)?;
    if let Token::Name { name, .. } = &tokens[i] && name == "else" {
        let (i, otherwise) = parse_block(tokens, i + 1)?;
        Ok((i, Statement::If { line, cond, then, otherwise }))
    } else if let Token::Name { name, .. } = &tokens[i] && name == "elif" {
        let (i, elif) = parse_if(tokens, i + 1, line)?;
        Ok((i, Statement::If { line, cond, then, otherwise: vec![elif] }))
    } else {
        Ok((i, Statement::If { line, cond, then, otherwise: vec![] }))
    }
}

fn parse_statement(tokens: &[Token], i: usize) -> Fallible<(usize, Statement)> {
    match (&tokens[i], tokens.get(i + 1)) {
        (Token::Name { line, name }, _) if name == "if" => {
            parse_if(tokens, i + 1, *line)
        }
        (Token::Name { line, name }, _) if name == "while" => {
            parse_while(tokens, i + 1, *line)
        }
        (Token::Name { line, name }, _) if name == "return" => {
            parse_return(tokens, i + 1, *line)
        }
        (Token::Name { line, name }, Some(Token::Name { name: var, .. })) if name == "for" => {
            parse_for(tokens, i + 2, *line, var.clone())
        }
        (Token::Name { line, name }, Some(Token::Operator { name: op, .. })) if op == "=" => {
            let (i, value) = parse_value(tokens, i + 2)?;
            let i = expect(tokens, i, ';')?;
            Ok((i, Statement::Assign { line: *line, name: name.clone(), value }))
        }
        (token, _) => {
            let (i, value) = parse_value(tokens, i)?;
            let i = expect(tokens, i, ';')?;
            Ok((i, Statement::Value { line: token.line(), value }))
        }
    }
}

fn parse_block(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<Statement>)> {
    let mut i = expect(tokens, i, '{')?;
    let mut stmts = vec![];

    loop {
        if let Token::Separator { name: '}', .. } = &tokens[i] {
            return Ok((i + 1, stmts))
        }
        let (new_i, stmt) = parse_statement(tokens, i)?;
        i = new_i;
        stmts.push(stmt);
    }
}

fn parse_while(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Statement)> {
    let (i, cond) = parse_value(tokens, i)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Statement::While { line, cond, body }))
}

fn parse_return(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Statement)> {
    let (i, value) = parse_value(tokens, i)?;
    let i = expect(tokens, i, ';')?;
    Ok((i, Statement::Return { line, value }))
}

fn parse_for(tokens: &[Token], i: usize, line: i64, var1: String) -> Fallible<(usize, Statement)> {
    if let Token::Separator { name: ':', .. } = &tokens[i] {
        let (i, expr) = parse_value(tokens, i + 1)?;
        let (i, body) = parse_block(tokens, i)?;
        Ok((i, Statement::For { line, key: None, value: var1, expr, body }))
    } else {
        let i = expect(tokens, i, ',')?;
        if let Token::Name { name: var2, .. } = &tokens[i] {
            let i = expect(tokens, i + 1, ':')?;
            let (i, expr) = parse_value(tokens, i)?;
            let (i, body) = parse_block(tokens, i)?;
            Ok((i, Statement::For { line, key: Some(var1), value: var2.clone(), expr, body }))
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
        let (i, value) = parse_value(tokens, i + 1)?;
        Ok((i, Decl::Const { line, name, value, private }))
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
