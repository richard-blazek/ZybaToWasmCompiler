use crate::error::{Fallible, err};

/*
<int> ::= [0-9]+
<real> ::= <int> "." [0-9]*
<bool> ::= "true" | "false"
<text> ::= "\"" <text_char>* "\""
<text_char> ::= [^"\\] | <text_esc>
<text_esc> ::= "\\\"" | "\\n" | "\\t" | "\\x" <text_hex> <text_hex>
<text_hex> ::= [0-9a-fA-F]
<operator> ::= [+*-/%&|~^<>=!]+
<separator> ::= [\(\)\[\]\.\{\}:,;]
<name> ::= [a-zA-Z] [a-zA-Z0-9_]
*/

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Real { line: i64, value: f64 },
    Int { line: i64, value: i64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Operator { line: i64, name: String },
    Separator { line: i64, name: char },
    Name { line: i64, name: String },
    Eof { line: i64 },
}

impl Token {
    pub fn line(&self) -> i64 {
        use Token::*;
        match self {
            Real { line, .. } | Int { line, .. } | Text { line, ..}
            | Bool { line, .. } | Operator { line, .. } | Eof { line }
            | Separator { line, .. } | Name { line, .. } => *line
        }
    }

    fn int(line: i64, value: u128) -> Token {
        Token::Int { line, value: value as i64 }
    }

    fn real(line: i64, mantissa: u128, exp: u32) -> Token {
        let value = mantissa as f64 / f64::powi(10.0, exp as i32);
        Token::Real { line, value }
    }

    fn name(line: i64, name: String) -> Token {
        if name == "true" || name == "false" {
            let value = name == "true";
            Token::Bool { line, value }
        } else {
            Token::Name { line, name }
        }
    }
}

enum State {
    Initial,
    Comment,
    Int(u128),
    Real(u128, u32),
    Text(String, Vec<char>),
    Separator(char),
    Operator(String),
    Name(String),
}

fn parse_digit(c: char) -> u8 {
    c.to_digit(36).unwrap_or(36) as u8
}

fn is_alnum(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_operator(c: char) -> bool {
    "+-*/%&|~^<>=!".contains(c)
}

fn is_separator(c: char) -> bool {
    "()[].{}:,;".contains(c)
}

fn new_state(line: i64, c: char) -> Fallible<State> {
    match c {
        '"' => Ok(State::Text(String::new(), vec![])),
        '#' => Ok(State::Comment),
        c if c.is_digit(10) => Ok(State::Int(parse_digit(c) as u128)),
        c if c.is_ascii_alphabetic() => Ok(State::Name(c.to_string())),
        c if c.is_whitespace() => Ok(State::Initial),
        c if is_operator(c) => Ok(State::Operator(c.to_string())),
        c if is_separator(c) => Ok(State::Separator(c)),
        _ => err(line, format!("Invalid character: {}", c)),
    }
}

fn add_char(mut s: String, c: char) -> String {
    s.push(c);
    s
}

fn next(c: char, state: State, start_line: i64, cur_line: i64) -> Fallible<(i64, State, Vec<Token>)> {
    match state {
        State::Comment if c == '\n' => Ok((cur_line, State::Initial, vec![])),
        State::Comment => Ok((cur_line, State::Comment, vec![])),
        State::Initial => {
            Ok((cur_line, new_state(start_line, c)?, vec![]))
        }
        State::Separator(s) => {
            let token = Token::Separator { line: start_line, name: s };
            Ok((cur_line, new_state(start_line, c)?, vec![token]))
        }
        State::Text(s, escape) if escape.is_empty() && c == '"' => {
            let token = Token::Text { line: start_line, value: s };
            Ok((cur_line, State::Initial, vec![token]))
        }
        State::Text(s, escape) => {
            let state = match (&escape[..], c) {
                (&[], '\\') => State::Text(s, vec!['\\']),
                (&[], _) => State::Text(add_char(s, c), vec![]),
                (&['\\'], '\"') => State::Text(add_char(s, '"'), vec![]),
                (&['\\'], '\\') => State::Text(add_char(s, '\\'), vec![]),
                (&['\\'], 'n') => State::Text(add_char(s, '\n'), vec![]),
                (&['\\'], 't') => State::Text(add_char(s, '\t'), vec![]),
                (&['\\'], 'x') => State::Text(s, vec!['\\', 'x']),
                (&['\\', 'x'], _) => {
                    if !c.is_digit(16) {
                        err(cur_line, format!("{} is not a hex digit", c))?;
                    }
                    State::Text(s, vec!['\\', 'x', c])
                }
                (&['\\', 'x', d], _) => {
                    if !c.is_digit(16) {
                        err(cur_line, format!("{} is not a hex digit", c))?;
                    }
                    let ord = (parse_digit(d) * 16 + parse_digit(c)) as char;
                    State::Text(add_char(s, ord), vec![])
                }
                _ => err(cur_line, format!("Invalid escape {}", c))?
            };
            Ok((start_line, state, vec![]))
        }
        State::Int(n) if c == '.' => Ok((cur_line, State::Real(n, 0), vec![])),
        State::Int(n) if c.is_ascii_digit() => {
            let n = n * 10 + parse_digit(c) as u128;
            if n > i64::MAX as u128 {
                err(cur_line, "Value too large to fit in 64 bits".into())?;
            }
            Ok((cur_line, State::Int(n), vec![]))
        }
        State::Int(n) => {
            let token = Token::int(start_line, n);
            Ok((cur_line, new_state(start_line, c)?, vec![token]))
        }
        State::Real(n, exp) if c.is_ascii_digit() => {
            let n = n * 10 + parse_digit(c) as u128;
            Ok((cur_line, State::Real(n, exp + 1), vec![]))
        }
        State::Real(n, exp) => {
            let token = Token::real(start_line, n, exp);
            Ok((cur_line, new_state(start_line, c)?, vec![token]))
        }
        State::Name(name) => {
            if is_alnum(c) {
                Ok((cur_line, State::Name(add_char(name, c)), vec![]))
            } else {
                Ok((
                    cur_line,
                    new_state(start_line, c)?,
                    vec![Token::name(start_line, name)],
                ))
            }
        }
        State::Operator(name) => {
            if is_operator(c) {
                Ok((cur_line, State::Operator(add_char(name, c)), vec![]))
            } else {
                Ok((
                    cur_line,
                    new_state(start_line, c)?,
                    vec![Token::Operator { line: start_line, name }],
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

        let (ln, st, new_tokens) = next(c, state, token_line, line)?;
        token_line = ln;
        state = st;
        tokens.extend(new_tokens);
    }

    match state {
        State::Initial | State::Comment => (),
        State::Real(n, exp) => tokens.push(Token::real(line, n, exp)),
        State::Int(value) => tokens.push(Token::int(line, value)),
        State::Separator(name) => tokens.push(Token::Separator { line, name }),
        State::Operator(name) => tokens.push(Token::Operator { line, name }),
        State::Name(name) => tokens.push(Token::name(line, name)),
        State::Text(_, _) => err(line, "Text literal not closed".into())?
    }

    tokens.push(Token::Eof { line });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use std::i64;

    use super::*;
    use proptest::prelude::*;

    fn digit() -> impl Strategy<Value = char> {
        prop::char::range('0', '9')
    }

    fn int_str() -> impl Strategy<Value = String> {
        prop::collection::vec(digit(), 1..10).prop_map(String::from_iter)
    }

    fn real_str() -> impl Strategy<Value = String> {
        (int_str(), prop::collection::vec(digit(), 0..10)).prop_map(|(whole, frac)| {
            format!("{}.{:}", whole, frac.into_iter().collect::<String>())
        })
    }

    fn bool_str() -> impl Strategy<Value = String> {
        prop_oneof![Just("true".to_string()), Just("false".to_string())]
    }

    fn name_str() -> impl Strategy<Value = String> {
        (
            prop_oneof![
                prop::char::range('a', 'z'),
                prop::char::range('A', 'Z'),
            ],
            prop::collection::vec(
                prop_oneof![
                    prop::char::range('a', 'z'),
                    prop::char::range('A', 'Z'),
                    digit(),
                    Just('_')
                ], 0..10
            )
        ).prop_map(|(c, rest)| {
            let mut s = String::new();
            s.push(c);
            s.extend(rest);
            s
        })
    }

    fn op_str() -> impl Strategy<Value = String> {
        prop::collection::vec(
            prop_oneof![
                Just('+'), Just('-'), Just('*'), Just('/'), Just('%'),
                Just('&'), Just('|'), Just('~'), Just('^'),
                Just('<'), Just('>'), Just('='), Just('!')
            ],
            1..5
        ).prop_map(|v| v.into_iter().collect())
    }

    fn sep_str() -> impl Strategy<Value = String> {
        prop_oneof![
            Just("(".to_string()), Just(")".to_string()),
            Just("[".to_string()), Just("]".to_string()),
            Just(".".to_string()), Just("{".to_string()), Just("}".to_string()),
            Just(":".to_string()), Just(",".to_string()), Just(";".to_string()),
        ]
    }

    fn any_str()  -> impl Strategy<Value = String> {
        prop::collection::vec(prop::char::any(), 0..20).prop_map(|cs| {
            String::from_iter(cs)
        })
    }

    fn token_eq(t: &Token, k: &Token) -> bool {
        if let Token::Real { line, value } = t {
            if let Token::Real { line: l2, value: v2 } = k {
                line == l2 && f64::abs(value - v2) < 0.0001
            } else {
                false
            }
        } else {
            t == k
        }
    }

    fn tokens_eq(t: &[Token], k: &[Token]) -> bool {
        t.iter().zip(k).all(|(t, k)| token_eq(t, k))
    }

    #[test]
    fn test_edge_cases() {
        assert_eq!(tokenize(&format!("{}", i64::MAX)).unwrap()[0], Token::Int { line: 1, value: i64::MAX });
        assert!(tokenize(&format!("{}", i64::MAX as u64 + 1)).is_err());
        assert!(tokenize("\"hello \\g world\"").is_err());
        assert!(tokenize("\"hello \\x7W world\"").is_err());
        assert!(tokenize("\"hello \\xW7 world\"").is_err());
        assert!(tokenize("\"hello world").is_err());
        assert!(tokenize("'hello world'").is_err());
    }

    proptest! {
        #[test]
        fn int_toks(toks in prop::collection::vec(int_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Int { line: 1, value: t.parse().unwrap() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let res = tokenize(&toks.join(" ")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn bool_toks(toks in prop::collection::vec(bool_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                assert!(t == "true" || t == "false");
                Token::Bool { line: 1, value: t == "true" }
            }).chain([Token::Eof { line: 1 }]).collect();
            let res = tokenize(&toks.join(" ")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn comment_toks(toks in prop::collection::vec(bool_str(), 1..20)) {
            let correct = tokenize(&toks.join("\n\n")).unwrap();
            let res = tokenize(&toks.join("# true\n# false\n")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn real_toks(toks in prop::collection::vec(real_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Real { line: 1, value: t.parse().unwrap() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let res = tokenize(&toks.join(" ")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn name_toks(toks in prop::collection::vec(name_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Name { line: 1, name: t.clone() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let res = tokenize(&toks.join(" ")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn op_toks(toks in prop::collection::vec(op_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Operator { line: 1, name: t.clone() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let res = tokenize(&toks.join(" ")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn sep_toks(toks in prop::collection::vec(sep_str(), 1..20)) {
            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Separator { line: 1, name: t.chars().next().unwrap() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let res = tokenize(&toks.join("")).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn text_toks(toks in prop::collection::vec(any_str(), 1..20)) {
            fn map_c(c: char) -> String {
                match c {
                    '\\' => "\\\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\t' => "\\t".to_string(),
                    '\"' => "\\\"".to_string(),
                    '\0'..='Z' => format!("\\x{:02x}", c as u8),
                    _ => c.to_string()
                }
            }

            fn escape(t: &str) -> String {
                Vec::from_iter(t.chars().map(|c| map_c(c))).join("")
            }

            let correct: Vec<_> = toks.iter().map(|t| {
                Token::Text { line: 1, value: t.clone() }
            }).chain([Token::Eof { line: 1 }]).collect();

            let input = toks.iter().map(|t| {
                "\"".to_string() + &escape(t) + "\""
            }).collect::<Vec<_>>().join("");

            let res = tokenize(&input).unwrap();
            prop_assert!(tokens_eq(&correct, &res));
        }

        #[test]
        fn err_chars(cs in prop::collection::vec(prop::char::any(), 1..20)) {
            for c in cs {
                prop_assert_eq!(tokenize(&c.to_string()).is_ok(),
                    c.is_ascii_alphanumeric() || is_separator(c)
                    || is_operator(c) || c.is_whitespace() || c == '#'
                );
            }
        }

        #[test]
        fn check_line(toks in prop::collection::vec(int_str(), 1..20)) {
            let input = toks.join("\n") + "\n";
            let mut line = 1;
            for t in tokenize(&input).unwrap() {
                prop_assert_eq!(t.line(), line);
                line += 1;
            }
        }
    }
}

