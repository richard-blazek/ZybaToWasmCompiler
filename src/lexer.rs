use crate::error::{err, Fallible};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Real(f64),
    Int(u64),
    Text(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Literal(u32, Literal),
    Operator(u32, String),
    Separator(u32, char),
    Name(u32, String),
}

pub enum State {
    Comment,
    Empty,
    LiteralReal(u32, u64, u32),
    LiteralInt(u32, u64),
    LiteralText(String),
    Operator(String),
    Separator(char),
    Name(String),
}

fn parse_digit(c: char) -> u64 {
    match c {
        '0'..='9' => c as u64 - '0' as u64,
        'A'..='Z' => 10 + (c as u64 - 'A' as u64),
        'a'..='z' => 10 + (c as u64 - 'a' as u64),
        _ => 36,
    }
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_alnum(c: char) -> bool {
    is_alpha(c) || c.is_digit(10)
}

fn is_operator(c: char) -> bool {
    "+-*/%&|~^<>=!".contains(c)
}

fn is_separator(c: char) -> bool {
    "()[].{}:".contains(c)
}

fn start_token(line: u32, c: char) -> Fallible<(u32, State)> {
    match c {
        '"' => Ok((line, State::LiteralText(String::new()))),
        '#' => Ok((line, State::Comment)),
        c if c.is_digit(10) => Ok((line, State::LiteralInt(10, parse_digit(c)))),
        c if is_operator(c) => Ok((line, State::Operator(c.to_string()))),
        c if is_separator(c) => Ok((line, State::Separator(c))),
        c if is_alpha(c) => Ok((line, State::Name(c.to_string()))),
        c if c.is_whitespace() => Ok((line, State::Empty)),
        _ => err(line, format!("Invalid character: {}", c)),
    }
}

fn process_int(newline: u32, line: u32, radix: u32, n: u64, c: char) -> Fallible<Vec<(u32, State)>> {
    match c {
        '.' => Ok(vec![(line, State::LiteralReal(radix, n, 0))]),
        'r' => {
            if radix == 10 {
                Ok(vec![(line, State::LiteralInt(n as u32, 0))])
            } else {
                err(line, "Specifying radix twice".into())
            }
        }
        c if c.is_digit(radix) => {
            Ok(vec![(line, State::LiteralInt(radix, n * radix as u64 + parse_digit(c)))])
        }
        _ => {
            Ok(vec![(line, State::LiteralInt(radix, n)), start_token(newline, c)?])
        }
    }
}

fn process_input(input: &str) -> Fallible<Vec<(u32, State)>> {
    let mut tokens = Vec::new();

    for c in input.chars() {
        let (line, last_token) = match tokens.pop() {
            Some((line, last_token)) => (line, last_token),
            None => (0, State::Empty)
        };
        let line_new = if c == '\n' { line + 1 } else { line };

        match last_token {
            State::Comment => {
                let tok = if c == '\n' { State::Empty } else { State::Comment };
                tokens.push((line_new, tok));
            }
            State::Empty => {
                tokens.push(start_token(line_new, c)?);
            },
            State::LiteralText(mut s) => {
                if s.ends_with("\\") {
                    s.pop();
                    s.push(c);
                    tokens.push((line_new, State::LiteralText(s)));
                } else if c == '"' {
                    tokens.push((line, State::LiteralText(s)));
                    tokens.push((line_new, State::Empty));
                } else {
                    s.push(c);
                    tokens.push((line_new, State::LiteralText(s)));
                }
            },
            State::LiteralInt(radix, n) => {
                tokens.extend(process_int(line_new, line, radix, n, c)?);
            },
            State::LiteralReal(radix, n, exp) => {
                if c.is_digit(radix) {
                    tokens.push((line_new, State::LiteralReal(radix, n * radix as u64 + parse_digit(c), exp + 1)));
                } else {
                    tokens.push((line, State::LiteralReal(radix, n, exp)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            State::Name(name) => {
                if is_alnum(c) {
                    tokens.push((line_new, State::Name(name + c.to_string().as_str())));
                } else {
                    tokens.push((line, State::Name(name)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            State::Operator(op) => {
                if is_operator(c) {
                    tokens.push((line_new, State::Operator(op + c.to_string().as_str())));
                } else {
                    tokens.push((line, State::Operator(op)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            _ => {
                tokens.push((line, last_token));
                tokens.push(start_token(line_new, c)?);
            }
        }
    }
    Ok(tokens)
}

pub fn tokenize(input: &str) -> Fallible<Vec<Token>> {
    let tokens = process_input(input)?.into_iter().flat_map(|(line, state)| {
        match state {
            State::Empty | State::Comment => {
                Vec::new()
            }
            State::LiteralReal(radix, val, offset) => {
                vec![Token::Literal(line, Literal::Real(val as f64 / (radix as f64).powi(offset as i32)))]
            }
            State::LiteralInt(_, val) => {
                vec![Token::Literal(line, Literal::Int(val))]
            }
            State::LiteralText(s) => {
                vec![Token::Literal(line, Literal::Text(s))]
            }
            State::Name(name) => {
                if name == "true" || name == "false" {
                    vec![Token::Literal(line, Literal::Bool(name == "true"))]
                } else {
                    vec![Token::Name(line, name)]
                }
            }
            State::Operator(op) => {
                vec![Token::Operator(line, op)]
            }
            State::Separator(sep) => {
                vec![Token::Separator(line, sep)]
            }
        }
    }).collect();
    Ok(tokens)
}
