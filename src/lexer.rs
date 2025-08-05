use crate::error::{err, Fallible};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Comment,
    Empty,
    LiteralReal(u32, u64, u32),
    LiteralInt(u32, u64),
    LiteralText(String),
    LiteralBool(bool),
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

fn start_token(line: u32, c: char) -> Fallible<(u32, Token)> {
    match c {
        '"' => Ok((line, Token::LiteralText(String::new()))),
        '#' => Ok((line, Token::Comment)),
        c if c.is_digit(10) => Ok((line, Token::LiteralInt(10, parse_digit(c)))),
        c if is_operator(c) => Ok((line, Token::Operator(c.to_string()))),
        c if is_separator(c) => Ok((line, Token::Separator(c))),
        c if is_alpha(c) => Ok((line, Token::Name(c.to_string()))),
        c if c.is_whitespace() => Ok((line, Token::Empty)),
        _ => err(line, format!("Invalid character: {}", c)),
    }
}

fn process_int(newline: u32, line: u32, radix: u32, n: u64, c: char) -> Fallible<Vec<(u32, Token)>> {
    match c {
        '.' => Ok(vec![(line, Token::LiteralReal(radix, n, 0))]),
        'b' => {
            if n == 0 || n == 1 {
                Ok(vec![(line, Token::LiteralBool(n == 1))])
            } else {
                err(line, format!("Invalid bool value {}", n))
            }
        }
        'r' => {
            if radix == 10 {
                Ok(vec![(line, Token::LiteralInt(n as u32, 0))])
            } else {
                err(line, "Specifying radix twice".into())
            }
        }
        c if c.is_digit(radix) => {
            Ok(vec![(line, Token::LiteralInt(radix, n * radix as u64 + parse_digit(c)))])
        }
        _ => {
            Ok(vec![(line, Token::LiteralInt(radix, n)), start_token(newline, c)?])
        }
    }
}

pub fn tokenize(input: &str) -> Fallible<Vec<(u32, Token)>> {
    let mut tokens = Vec::new();

    for c in input.chars() {
        let (line, last_token) = match tokens.pop() {
            Some((line, last_token)) => (line, last_token),
            None => (0, Token::Empty)
        };
        let line_new = if c == '\n' { line + 1 } else { line };

        match last_token {
            Token::Comment => {
                let tok = if c == '\n' { Token::Empty } else { Token::Comment };
                tokens.push((line_new, tok));
            }
            Token::Empty => {
                tokens.push(start_token(line_new, c)?);
            },
            Token::LiteralText(mut s) => {
                if s.ends_with("\\") {
                    s.pop();
                    s.push(c);
                    tokens.push((line_new, Token::LiteralText(s)));
                } else if c == '"' {
                    tokens.push((line, Token::LiteralText(s)));
                    tokens.push((line_new, Token::Empty));
                } else {
                    s.push(c);
                    tokens.push((line_new, Token::LiteralText(s)));
                }
            },
            Token::LiteralInt(radix, n) => {
                tokens.extend(process_int(line_new, line, radix, n, c)?);
            },
            Token::LiteralReal(radix, n, exp) => {
                if c.is_digit(radix) {
                    tokens.push((line_new, Token::LiteralReal(radix, n * radix as u64 + parse_digit(c), exp + 1)));
                } else {
                    tokens.push((line, Token::LiteralReal(radix, n, exp)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            Token::Name(name) => {
                if is_alnum(c) {
                    tokens.push((line_new, Token::Name(name + c.to_string().as_str())));
                } else {
                    tokens.push((line, Token::Name(name)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            Token::Operator(op) => {
                if is_operator(c) {
                    tokens.push((line_new, Token::Operator(op + c.to_string().as_str())));
                } else {
                    tokens.push((line, Token::Operator(op)));
                    tokens.push(start_token(line_new, c)?);
                }
            },
            _ => {
                tokens.push((line, last_token));
                tokens.push(start_token(line_new, c)?);
            }
        }
    }

    match tokens.last() {
        Some((_, Token::Empty | Token::Comment)) => { tokens.pop(); }
        None | Some(_) => { }
    }

    Ok(tokens)
}
