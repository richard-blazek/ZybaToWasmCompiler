use crate::error::{Fallible, err};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Real { line: i64, value: f64 },
    Int { line: i64, value: u64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Operator { line: i64, name: String },
    Separator { line: i64, name: char },
    Name { line: i64, name: String },
    Eof { line: i64 },
}

impl Token {
    pub fn line(&self) -> i64 {
        match self {
            Token::Real { line, .. } => *line,
            Token::Int { line, .. } => *line,
            Token::Text { line, .. } => *line,
            Token::Bool { line, .. } => *line,
            Token::Operator { line, .. } => *line,
            Token::Separator { line, .. } => *line,
            Token::Name { line, .. } => *line,
            Token::Eof { line } => *line,
        }
    }
}

enum State {
    Initial,
    Comment,
    Real(u32, u64, u32),
    Int(u32, u64),
    Text(String, bool),
    Separator(char),
    Operator(String),
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
    "()[].{}:,;".contains(c)
}

fn new_state(line: i64, c: char) -> Fallible<State> {
    match c {
        '"' => Ok(State::Text(String::new(), false)),
        '#' => Ok(State::Comment),
        c if c.is_digit(10) => Ok(State::Int(10, parse_digit(c))),
        c if is_operator(c) => Ok(State::Operator(c.to_string())),
        c if is_separator(c) => Ok(State::Separator(c)),
        c if is_alpha(c) => Ok(State::Name(c.to_string())),
        c if c.is_whitespace() => Ok(State::Initial),
        _ => err(line, format!("Invalid character: {}", c)),
    }
}

fn real_token(line: i64, radix: u32, mantissa: u64, exp: u32) -> Token {
    Token::Real {
        line: line,
        value: mantissa as f64 / (radix as f64).powi(exp as i32),
    }
}

fn name_token(line: i64, name: String) -> Token {
    if name == "true" || name == "false" {
        Token::Bool {
            line: line,
            value: name == "true",
        }
    } else {
        Token::Name { line, name }
    }
}

fn process_char(c: char, state: State, start_line: i64, current_line: i64) -> Fallible<(i64, State, Vec<Token>)> {
    match state {
        State::Comment => Ok((
            current_line,
            if c == '\n' {
                State::Initial
            } else {
                State::Comment
            },
            vec![],
        )),
        State::Initial => Ok((current_line, new_state(start_line, c)?, vec![])),
        State::Separator(s) => {
            let token = Token::Separator {
                line: start_line,
                name: s,
            };
            Ok((current_line, new_state(start_line, c)?, vec![token]))
        }
        State::Text(s, escape) => {
            if escape {
                Ok((
                    start_line,
                    State::Text(s + c.to_string().as_str(), false),
                    vec![],
                ))
            } else if c == '\\' {
                Ok((start_line, State::Text(s, true), vec![]))
            } else if c == '"' {
                Ok((
                    current_line,
                    State::Initial,
                    vec![Token::Text {
                        line: start_line,
                        value: s,
                    }],
                ))
            } else {
                Ok((
                    start_line,
                    State::Text(s + c.to_string().as_str(), false),
                    vec![],
                ))
            }
        }
        State::Int(radix, n) if c == '.' => Ok((current_line, State::Real(radix, n, 0), vec![])),
        State::Int(radix, n) if c == 'r' => {
            if radix != 10 {
                err(current_line, "Specifying radix twice".into())?;
            }
            Ok((current_line, State::Int(n as u32, 0), vec![]))
        }
        State::Int(radix, n) if c.is_digit(radix) => {
            let new_n = n * radix as u64 + parse_digit(c);
            Ok((current_line, State::Int(radix, new_n), vec![]))
        }
        State::Int(_, n) => {
            let token = Token::Int {
                line: start_line,
                value: n,
            };
            Ok((current_line, new_state(start_line, c)?, vec![token]))
        }
        State::Real(radix, n, exp) => {
            if c.is_digit(radix) {
                Ok((
                    current_line,
                    State::Real(radix, n * radix as u64 + parse_digit(c), exp + 1),
                    vec![],
                ))
            } else {
                Ok((
                    current_line,
                    new_state(start_line, c)?,
                    vec![real_token(current_line, radix, n, exp)],
                ))
            }
        }
        State::Name(name) => {
            if is_alnum(c) {
                Ok((current_line, State::Name(name + c.to_string().as_str()), vec![]))
            } else {
                Ok((
                    current_line,
                    new_state(start_line, c)?,
                    vec![name_token(current_line, name)],
                ))
            }
        }
        State::Operator(name) => {
            if is_operator(c) {
                Ok((current_line, State::Operator(name + c.to_string().as_str()), vec![]))
            } else {
                Ok((
                    current_line,
                    new_state(start_line, c)?,
                    vec![Token::Operator {
                        line: start_line,
                        name: name,
                    }],
                ))
            }
        }
    }
}

pub fn tokenize(input: &str) -> Fallible<Vec<Token>> {
    let mut tokens: Vec<Token> = vec![];
    let mut state = State::Initial;
    let mut token_line = 1i64;
    let mut line = 1i64;

    for c in input.chars() {
        line += if c == '\n' { 1 } else { 0 };

        let (new_line, new_state, new_tokens) = process_char(c, state, token_line, line)?;
        token_line = new_line;
        state = new_state;
        tokens.extend(new_tokens);
    }

    match state {
        State::Initial | State::Comment => (),
        State::Real(r, n, exp) => tokens.push(real_token(line, r, n, exp)),
        State::Int(_, value) => tokens.push(Token::Int { line, value }),
        State::Separator(name) => tokens.push(Token::Separator { line, name }),
        State::Operator(name) => tokens.push(Token::Operator { line, name }),
        State::Name(name) => tokens.push(name_token(line, name)),
        State::Text(_, _) => err(token_line, "Text not closed".into())?,
    }

    tokens.push(Token::Eof { line });
    Ok(tokens)
}
