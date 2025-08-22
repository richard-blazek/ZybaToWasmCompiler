use crate::error::{Fallible, err};

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

    fn real(line: i64, radix: u32, mantissa: u128, exp: u32) -> Token {
        let value = mantissa as f64 / (radix as f64).powi(exp as i32);
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
    Real(u32, u128, u32),
    Int(u32, u128),
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
        c if c.is_digit(10) => Ok(State::Int(10, parse_digit(c) as u128)),
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
        State::Int(radix, n) if c == '.' => {
            Ok((cur_line, State::Real(radix, n, 0), vec![]))
        }
        State::Int(radix, n) if c == 'r' => {
            if radix != 10 {
                err(cur_line, "Specifying radix twice".into())?;
            }
            if n < 2 || n > 36 {
                err(cur_line, "Radix must be between 2 and 36".into())?;
            }
            Ok((cur_line, State::Int(n as u32, 0), vec![]))
        }
        State::Int(radix, n) if c.is_ascii_alphanumeric() => {
            if !c.is_digit(radix) {
                err(cur_line, format!("{} is not a {}-base digit", c, radix))?;
            }
            let new_n = n * radix as u128 + parse_digit(c) as u128;
            if new_n > i64::MAX as u128 {
                err(cur_line, "Value too large to fit in 64 bits".into())?;
            }
            Ok((cur_line, State::Int(radix, new_n), vec![]))
        }
        State::Int(_, n) => {
            let token = Token::int(start_line, n);
            Ok((cur_line, new_state(start_line, c)?, vec![token]))
        }
        State::Real(radix, n, exp) => {
            if c.is_digit(radix) {
                let n = n * radix as u128 + parse_digit(c) as u128;
                Ok((cur_line, State::Real(radix, n, exp + 1), vec![]))
            } else {
                Ok((
                    cur_line,
                    new_state(start_line, c)?,
                    vec![Token::real(start_line, radix, n, exp)],
                ))
            }
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
        State::Real(r, n, exp) => tokens.push(Token::real(line, r, n, exp)),
        State::Int(_, value) => tokens.push(Token::int(line, value)),
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
    use crate::lexer::{tokenize, Token};

    use proptest::prelude::*;

    fn is_close(a: f64, b: f64) -> bool {
        let tol = 1e-12_f64.max(1e-12_f64 * a.abs().max(b.abs()));
        (a - b).abs() <= tol
    }

    fn eof_line(tokens: &[Token]) -> Option<i64> {
        tokens.last().and_then(|t| {
            if let Token::Eof { line } = t { Some(*line) } else { None }
        })
    }

    fn strip_eof(mut tokens: Vec<Token>) -> (Vec<Token>, Option<i64>) {
        if matches!(tokens.last(), Some(Token::Eof { .. })) {
            if let Token::Eof { line } = tokens.pop().unwrap() {
                return (tokens, Some(line));
            }
        }
        (tokens, None)
    }

    fn to_base(mut v: u128, radix: u32) -> String {
        assert!((2..=36).contains(&radix));
        if v == 0 {
            return "0".to_string();
        }
        let mut s = String::new();
        while v > 0 {
            let d = (v % radix as u128) as u8;
            let ch = match d {
                0..=9 => (b'0' + d) as char,
                10..=35 => (b'A' + (d - 10)) as char,
                _ => unreachable!(),
            };
            s.push(ch);
            v /= radix as u128;
        }
        s.chars().rev().collect()
    }

    fn eval_radixed_real(int_digits: &str, frac_digits: &str, radix: u32) -> f64 {
        let r = radix as f64;
        let int_val = int_digits.chars().fold(0u128, |acc, c| {
            let v = digit_value(c);
            acc * radix as u128 + v as u128
        }) as f64;

        let mut frac = 0.0f64;
        let mut p = 1.0f64;
        for c in frac_digits.chars() {
            p *= r;
            frac += digit_value(c) as f64 / p;
        }
        int_val + frac
    }

    fn digit_value(c: char) -> u32 {
        match c {
            '0'..='9' => (c as u8 - b'0') as u32,
            'A'..='Z' => 10 + (c as u8 - b'A') as u32,
            'a'..='z' => 10 + (c as u8 - b'a') as u32,
            _ => 1000,
        }
    }

    #[test]
    fn ints_basic_and_leading_zeros() {
        let input = "0 7 42 0009";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _eof) = strip_eof(tokens);

        let expected = vec![
            Token::Int { line: 1, value: 0 },
            Token::Int { line: 1, value: 7 },
            Token::Int { line: 1, value: 42 },
            Token::Int { line: 1, value: 9 },
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn real_basic_variants() {
        let input = "1.0 0. 12.34 9999.";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _eof) = strip_eof(tokens);

        let expect_vals = [1.0, 0.0, 12.34, 9999.0];
        for (i, tok) in tokens.iter().enumerate() {
            match tok {
                Token::Real { line, value } => {
                    assert_eq!(*line, 1);
                    assert!(is_close(*value, expect_vals[i]));
                }
                _ => panic!("Expected Real at {i:?}, got {tok:?}"),
            }
        }
    }

    #[test]
    fn real_does_not_start_with_dot() {
        let input = ".5";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _eof) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Separator { line: 1, name: '.' },
                Token::Int { line: 1, value: 5 }
            ]
        );
    }

    #[test]
    fn radixed_ints_extremes() {
        let input = "2r101101";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(tokens, vec![Token::Int { line: 1, value: 0b101101 }]);

        let input = "36rZ";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(tokens, vec![Token::Int { line: 1, value: 35 }]);

        let input = "12r10A";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(tokens, vec![Token::Int { line: 1, value: 154 }]);
    }

    #[test]
    fn radixed_reals_examples() {
        let input = "12r10A.0";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        match &tokens[..] {
            [Token::Real { line, value }] => {
                assert_eq!(*line, 1);
                assert!(is_close(*value, 154.0));
            }
            _ => panic!("Expected single Real token, got {tokens:?}"),
        }

        let input = "2r1.1";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        match &tokens[..] {
            [Token::Real { value, .. }] => assert!(is_close(*value, 1.5)),
            _ => panic!("Expected single Real token, got {tokens:?}"),
        }

        let input = "16r10.";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        match &tokens[..] {
            [Token::Real { value, .. }] => assert!(is_close(*value, 16.0)),
            _ => panic!("Expected single Real token, got {tokens:?}"),
        }
    }

    #[test]
    fn names_and_bools() {
        let input = "true false";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Bool { line: 1, value: true },
                Token::Bool { line: 1, value: false }
            ]
        );

        let input = "truex false_ not_true";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Name { line: 1, name: "truex".to_string() },
                Token::Name { line: 1, name: "false_".to_string() },
                Token::Name { line: 1, name: "not_true".to_string() },
            ]
        );
    }

    #[test]
    fn name_rules_start_letter_then_alnum_underscore() {
        let input = "x X _x 9abc a_1_b2";
        let result = tokenize(input);

        assert!(result.is_err(), "Leading '_' or digit should be invalid Name");

        let input = "x a_1_b2";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Name { line: 1, name: "x".to_string() },
                Token::Name { line: 1, name: "a_1_b2".to_string() },
            ]
        );
    }

    #[test]
    fn operators_coalesce_and_minus_is_operator() {
        let input = "+++ == != <= >= && || << >> ~ ^ % ** //";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        let names: Vec<String> = tokens
            .into_iter()
            .map(|t| if let Token::Operator { name, .. } = t { name } else { panic!("Expected Operator") })
            .collect();

        assert_eq!(
            names,
            vec!["+++".to_string(),"==".to_string(),"!=".to_string(),"<=".to_string(),">=".to_string(),"&&".to_string(),"||".to_string(),"<<".to_string(),">>".to_string(),"~".to_string(),"^".to_string(),"%".to_string(),"**".to_string(),"//".to_string()]
        );

        let tokens = tokenize("-42").expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Operator { line: 1, name: "-".to_string() },
                Token::Int { line: 1, value: 42 }
            ]
        );
    }

    #[test]
    fn separators_singletons() {
        let input = "()[].{}:,;";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);

        let seps: Vec<char> = tokens
            .into_iter()
            .map(|t| if let Token::Separator { name, .. } = t { name } else { panic!("Expected Separator, got {t:?}") })
            .collect();

        assert_eq!(seps, vec!['(',')','[',']','.','{','}',':',',',';']);
    }

    #[test]
    fn dot_ambiguities() {
        let input = "12..34 12.r34";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);

        match &tokens[..] {
            [Token::Real { value: v1, .. },
             Token::Separator { name: '.', .. },
             Token::Int { value: 34, .. },
             Token::Real { value: v2, .. },
             Token::Name { name, .. }] =>
            {
                assert!(is_close(*v1, 12.0));
                assert!(is_close(*v2, 12.0));
                assert_eq!(name, "r34");
            }
            _ => panic!("Unexpected tokenization: {tokens:?}"),
        }
    }

    #[test]
    fn strings_basic_and_escapes() {
        let input = r#""hello" "a\nb\tc\"d\\e" "A\x41B""#;
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);

        match &tokens[..] {
            [Token::Text { value: v1, .. },
             Token::Text { value: v2, .. },
             Token::Text { value: v3, .. }] =>
            {
                assert_eq!(v1, "hello");
                assert_eq!(v2, "a\nb\tc\"d\\e");
                assert_eq!(v3, "AAB");
            }
            _ => panic!("Unexpected string tokens: {tokens:?}"),
        }
    }

    #[test]
    fn strings_errors_unterminated_and_illegal_escape() {
        assert!(tokenize(r#""abc"#).is_err());
        assert!(tokenize(r#""bad\q""#).is_err());
    }

    #[test]
    fn whitespace_and_line_numbers() {
        let input = "true\n42\t\n(  \n)  3.14  \nname";
        let tokens = tokenize(input).expect("lex");

        let eof = eof_line(&tokens);
        assert_eq!(eof, Some(5));

        let (tokens, _) = strip_eof(tokens);
        let mut got: Vec<(String, i64)> = Vec::new();
        for t in tokens {
            match t {
                Token::Bool { line, value } => got.push((format!("Bool({})", value), line)),
                Token::Int { line, value } => got.push((format!("Int({})", value), line)),
                Token::Separator { line, name } => got.push((format!("Sep({})", name), line)),
                Token::Real { line, value } => got.push((format!("Real({})", value), line)),
                Token::Name { line, name } => got.push((format!("Name({})", name), line)),
                other => panic!("Unexpected token here: {other:?}"),
            }
        }
        assert_eq!(
            got,
            vec![
                ("Bool(true)".to_string(), 1),
                ("Int(42)".to_string(), 2),
                ("Sep(()".to_string(), 3),
                ("Sep())".to_string(), 4),
                ("Real(3.14)".to_string(), 4),
                ("Name(name)".to_string(), 5),
            ]
        );
    }

    #[test]
    fn eof_present_even_on_empty_input() {
        let tokens = tokenize("").expect("lex");
        match &tokens[..] {
            [Token::Eof { line }] => assert_eq!(*line, 1),
            _ => panic!("Expected only Eof token, got {tokens:?}"),
        }
    }

    #[test]
    fn invalid_radix_and_digits() {
        assert!(tokenize("1r1").is_err());
        assert!(tokenize("37rZ").is_err());

        assert!(tokenize("2r102").is_err());
        assert!(tokenize("16r10G").is_err());
    }

    #[test]
    fn overflow_u64_should_error() {
        let too_big = "18446744073709551616";
        assert!(tokenize(too_big).is_err());
        assert!(tokenize("16r10000000000000000").is_err());
    }

    #[test]
    fn illegal_characters_should_error() {
        assert!(tokenize("@").is_err());
    }

    #[test]
    fn spaced_out_radix_is_not_a_radix_number() {
        let input = "10 r 1010";
        let tokens = tokenize(input).expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(
            tokens,
            vec![
                Token::Int { line: 1, value: 10 },
                Token::Name { line: 1, name: "r".to_string() },
                Token::Int { line: 1, value: 1010 },
            ]
        );
    }

    proptest! {
        #[test]
        fn prop_radixed_ints_match_decimal(radix in 2u32..=36u32, value in 0u128..=10_000_000u128) {
            let digits = to_base(value, radix);
            let input = format!("{radix}r{digits}");
            let tokens = tokenize(&input).expect("lex");
            let (tokens, _eof) = strip_eof(tokens);

            prop_assert_eq!(tokens.len(), 1);
            match &tokens[0] {
                Token::Int { value: v, .. } => prop_assert_eq!(*v as u128, value),
                other => prop_assert!(false, "Expected Int, got {other:?}"),
            }
        }

        #[test]
        fn prop_names_round_trip(start in "[A-Za-z]", rest in "[A-Za-z0-9_]{0,20}") {
            let name = format!("{start}{rest}");
            let tokens = tokenize(&name).expect("lex");
            let (tokens, _eof) = strip_eof(tokens);
            prop_assert_eq!(tokens.len(), 1);
            match &tokens[0] {
                Token::Name { line, name: n } => {
                    prop_assert_eq!(*line, 1);
                    prop_assert_eq!(n, &name);
                }
                other => prop_assert!(false, "Expected Name, got {other:?}"),
            }
        }

        #[test]
        fn prop_operators_coalesce(op in r"[+\-*/%&|~^<>=!]{1,10}") {
            let tokens = tokenize(&op).expect("lex");
            let (tokens, _eof) = strip_eof(tokens);
            prop_assert_eq!(tokens.len(), 1);
            match &tokens[0] {
                Token::Operator { name, .. } => prop_assert_eq!(name, &op),
                other => prop_assert!(false, "Expected Operator, got {other:?}"),
            }
        }

        #[test]
        fn prop_separators_single_char(c in r"[\(\)\[\]\.\{\}:,;]") {
            let tokens = tokenize(&c).expect("lex");
            let (tokens, _eof) = strip_eof(tokens);
            prop_assert_eq!(tokens.len(), 1);
            match &tokens[0] {
                Token::Separator { name, .. } => prop_assert_eq!(*name, c.chars().next().unwrap()),
                other => prop_assert!(false, "Expected Separator, got {other:?}"),
            }
        }

        #[test]
        fn prop_radixed_real_binary_exact(int_part in "[01]{1,16}", frac_part in "[01]{0,16}") {
            let input = if frac_part.is_empty() {
                format!("2r{int_part}.")
            } else {
                format!("2r{int_part}.{frac_part}")
            };

            let expected = eval_radixed_real(&int_part, &frac_part, 2);
            let tokens = tokenize(&input).expect("lex");
            let (tokens, _eof) = strip_eof(tokens);
            prop_assert_eq!(tokens.len(), 1);
            match &tokens[0] {
                Token::Real { value, .. } => prop_assert!(is_close(*value, expected), "got {}, expected {}", value, expected),
                other => prop_assert!(false, "Expected Real, got {other:?}"),
            }
        }
    }

    #[test]
    fn lowercase_radix_digits() {
        let tokens = tokenize("16r1aF").expect("lex");
        let (tokens, _) = strip_eof(tokens);
        assert_eq!(tokens, vec![Token::Int { line: 1, value: 0x1AF }]);
    }

    #[test]
    fn real_line_should_reflect_start_line() {
        let input = "x\n3.14\n";
        let toks = tokenize(input).expect("lex");
        let (toks, _eof) = strip_eof(toks);

        assert!(matches!(toks[0], Token::Name { line: 1, .. }));

        match &toks[1] {
            Token::Real { line, value } => {
                assert_eq!(*line, 2, "Real should keep its start line (2), not be bumped to 3");
                assert!(is_close(*value, 3.14));
            }
            other => panic!("Expected Real, got {other:?}"),
        }
    }

    #[test]
    fn unicode_digits_must_be_rejected_in_numbers() {
        assert!(
            tokenize("1\u{0663}").is_err(),
            "Non-ASCII digits should be rejected; current code silently parses to a wrong value"
        );

        assert!(tokenize("16r\u{0661}0").is_err());
    }

    #[test]
    fn lowercase_radix_digits_ok() {
        let toks = tokenize("16r1aF").expect("lex");
        let (toks, _eof) = strip_eof(toks);
        assert_eq!(toks, vec![Token::Int { line: 1, value: 0x1AF }]);
    }

    #[test]
    fn real_line_not_affected_by_space_terminator() {
        let input = "3.14 \n";
        let toks = tokenize(input).expect("lex");
        let (toks, _eof) = strip_eof(toks);
        match &toks[0] {
            Token::Real { line, value } => {
                assert_eq!(*line, 1);
                assert!(is_close(*value, 3.14));
            }
            other => panic!("Expected Real, got {other:?}"),
        }
    }

    #[test]
    fn eof_line_after_trailing_newline() {
        let toks = tokenize("a\n").expect("lex");
        match toks.last() {
            Some(Token::Eof { line }) => assert_eq!(*line, 2),
            other => panic!("Expected EOF, got {other:?}"),
        }
    }
}
