use std::collections::{HashMap, HashSet};

use crate::error::{err, Fallible};
use crate::lexer::Token;

pub enum Value {
    Int { line: i64, value: u64 },
    Real { line: i64, value: f64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Record { line: i64, fields: HashMap<String, Value> },
    Var { line: i64, module: Option<String>, name: String },
    Call { line: i64, func: Box<Value>, args: Vec<Value> },
    BinOp { line: i64, name: String, lhs: Box<Value>, rhs: Box<Value> },
    Access { line: i64, object: Box<Value>, field: String },
    Lambda { line: i64, args: Vec<(String, Value)>, return_type: Box<Value>, body: Vec<Statement> }
}

impl Value {
    pub fn line(&self) -> i64 {
        match self {
            Value::Int { line, .. } => *line,
            Value::Real { line, .. } => *line,
            Value::Text { line, .. } => *line,
            Value::Bool { line, .. } => *line,
            Value::Record { line, .. } => *line,
            Value::Var { line, .. } => *line,
            Value::Call { line, .. } => *line,
            Value::BinOp { line, .. } => *line,
            Value::Access { line, .. } => *line,
            Value::Lambda { line, .. } => *line,
        }
    }
}

pub enum Statement {
    Value { line: i64, value: Value },
    Assignment { line: i64, name: String, value: Value },
    If { line: i64, cond: Value, then: Vec<Statement>, otherwise: Vec<Statement> },
    While { line: i64, cond: Value, body: Vec<Statement> },
    For { line: i64, key: Option<String>, value: String, expr: Value, body: Vec<Statement> },
    Return { line: i64, value: Value }
}

impl Statement {
    pub fn line(&self) -> i64 {
        match self {
            Statement::Value { line, .. } => *line,
            Statement::Assignment { line, .. } => *line,
            Statement::If { line, .. } => *line,
            Statement::While { line, .. } => *line,
            Statement::For { line, .. } => *line,
            Statement::Return { line, .. } => *line,
        }
    }
}

pub enum Declaration {
    Import { line: i64, path: String },
    Const { line: i64, name: String, value: Value, exported: bool },
    Extern { line: i64, name: String, type_: Value }
}

impl Declaration {
    pub fn line(&self) -> i64 {
        match self {
            Declaration::Import { line, .. } => *line,
            Declaration::Const { line, .. } => *line,
            Declaration::Extern { line, .. } => *line,
        }
    }
}

pub type File = Vec<Declaration>;

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
    if let (Some(Token::Name { .. }),
            Some(Token::Separator { name: ':', .. })) = (tokens.get(i), tokens.get(i + 1)) {
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
                Token::Separator { name: ',', .. } => (),
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
        Ok((i + 3, Value::Var { line, module: Some(first_name), name: name.clone() }))
    } else {
        Ok((i, Value::Var { line, module: None, name: first_name}))
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
            Token::Separator { name: ',', .. } => (),
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

fn parse_block(tokens: &[Token], i: usize) -> Fallible<(usize, Vec<Statement>)> {
    let mut i = expect(tokens, i, '{')?;
    let mut stmts = vec![];

    loop {
        if let Token::Separator { name: '}', .. } = &tokens[i] {
            return Ok((i + 1, stmts))
        }
        let (new_i, stmt) = parse_statement(tokens, i + 1)?;
        stmts.push(stmt);
        i = expect(tokens, new_i, ';')?;
    }
}

fn parse_while(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Statement)> {
    let (i, cond) = parse_value(tokens, i + 1)?;
    let (i, body) = parse_block(tokens, i)?;
    Ok((i, Statement::While { line, cond, body }))
}

fn parse_return(tokens: &[Token], i: usize, line: i64) -> Fallible<(usize, Statement)> {
    let (i, value) = parse_value(tokens, i + 1)?;
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
            Ok((i, Statement::Assignment { line: *line, name: name.clone(), value }))
        }
        (token, _) => {
            let (i, value) = parse_value(tokens, i)?;
            Ok((i, Statement::Value { line: token.line(), value }))
        }
    }
}

pub fn parse(tokens: &[Token]) -> Fallible<File> {
    parse_value(tokens, 0)?;
    todo!()
}
