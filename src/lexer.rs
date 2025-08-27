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
    use super::*; // assumes `tokenize`, `Token`, and `TokenError` are in the parent module

    // --- Small helpers to make expected tokens concise ----------------------------------------
    fn t_int(line: i64, value: i64) -> Token { Token::Int { line, value } }
    fn t_real(line: i64, value: f64) -> Token { Token::Real { line, value } }
    fn t_text(line: i64, value: &str) -> Token { Token::Text { line, value: value.to_string() } }
    fn t_bool(line: i64, value: bool) -> Token { Token::Bool { line, value } }
    fn t_op(line: i64, name: &str) -> Token { Token::Operator { line, name: name.to_string() } }
    fn t_sep(line: i64, name: char) -> Token { Token::Separator { line, name } }
    fn t_name(line: i64, name: &str) -> Token { Token::Name { line, name: name.to_string() } }

    /// Assert the token stream equals `expected` followed by an EOF, ignoring the EOF line value.
    fn assert_tokens(input: &str, expected: &[Token]) -> Vec<Token> {
        let tokens = tokenize(input).expect("tokenize should succeed");
        assert!(!tokens.is_empty(), "lexer must produce at least EOF");
        match tokens.last().unwrap() {
            Token::Eof { .. } => {},
            other => panic!("last token must be EOF, got: {:?}", other),
        }
        assert_eq!(&tokens[..tokens.len()-1], expected, "token sequence (without EOF) mismatch");
        tokens
    }

    // --------------------------------- Smoke & whitespace -------------------------------------

    #[test]
    fn empty_input_yields_only_eof() {
        let tokens = tokenize("").expect("should succeed on empty");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0], Token::Eof { .. }));
        // If your lexer starts at line 1:
        if let Token::Eof { line } = tokens[0] { assert_eq!(line, 1); }
    }

    #[test]
    fn whitespace_ignored_and_lines_tracked() {
        // Two newlines total ⇒ final EOF line should be 3 (if starting at 1).
        let input = "   \t \n \t  \n  ";
        let toks = tokenize(input).expect("should succeed");
        assert!(matches!(toks.last(), Some(Token::Eof { .. })));
        if let Token::Eof { line } = toks.last().unwrap() {
            assert_eq!(*line, 3, "adjust this if your lexer uses different line tracking");
        }
    }

    // ----------------------------------- Integers & Reals -------------------------------------

    #[test]
    fn basic_integers_and_reals() {
        let expected = vec![
            t_int(1, 0),
            t_int(1, 7),
            t_int(1, 12345),
            t_real(1, 1.0),
            t_real(1, 1.0), // "1." is a valid <real> per grammar
            t_real(1, 0.0), // "0."
            t_real(1, 123.45),
        ];
        assert_tokens("0 7 12345 1.0 1. 0. 123.45", &expected);
    }

    #[test]
    fn real_trailing_dot_vs_separator_ambiguity() {
        // Per grammar, <real> ::= <int> "." [0-9]*, so "1.." tokenizes as REAL("1.") then '.'.
        let expected = vec![
            t_real(1, 1.0),
            t_sep(1, '.'),
            t_int(1, 2),
        ];
        assert_tokens("1..2", &expected);

        // "42.true" ⇒ REAL("42.") then NAME("true").
        let expected2 = vec![t_real(1, 42.0), t_bool(1, true)];
        assert_tokens("42.true", &expected2);
    }

    #[test]
    fn real_cannot_start_with_dot() {
        // ".5" is not a <real> by your grammar; it should be '.' then INT(5).
        let expected = vec![t_sep(1, '.'), t_int(1, 5)];
        assert_tokens(".5", &expected);
    }

    #[test]
    fn number_followed_by_letters_forms_separate_tokens() {
        // "1e3" is not scientific notation in this grammar; it’s INT(1) then NAME("e3").
        let expected = vec![t_int(1, 1), t_name(1, "e3")];
        assert_tokens("1e3", &expected);
    }

    #[test]
    fn underscores_in_numbers_are_errors() {
        // '_' is not allowed in <int>/<real>; should error on first unknown char.
        assert!(tokenize("1_2").is_err());
    }

    // Optionally test overflow if you parse ints strictly into i64.
    #[test]
    fn int_overflow_is_error_if_parsing_into_i64() {
        // i64::MAX = 9223372036854775807
        assert!(tokenize("9223372036854775807").is_ok());
        assert!(tokenize("9223372036854775808").is_err(), "adjust if you don't check overflow");
    }

    // ------------------------------------ Bools & Names ---------------------------------------

    #[test]
    fn bool_keywords_and_names() {
        let expected = vec![
            t_bool(1, true),
            t_bool(1, false),
            t_name(1, "True"),   // case-sensitive
            t_name(1, "FALSE"),  // case-sensitive
            t_name(1, "trueX"),  // not a bool (suffix)
            t_name(1, "xtrue"),
            t_name(1, "abc123_def"), // typical identifier
            t_name(1, "Z"),
        ];
        assert_tokens("true false True FALSE trueX xtrue abc123_def Z", &expected);
    }

    #[test]
    fn name_must_start_with_ascii_letter() {
        // Leading '_' or digit should not start a <name>; '_' here is unknown → error.
        assert!(tokenize("_x").is_err());
        // Starting with digit will produce an int followed by name if letter follows.
        let expected = vec![t_int(1, 3), t_name(1, "abc")];
        assert_tokens("3abc", &expected);
    }

    #[test]
    fn non_ascii_letters_are_errors_per_grammar() {
        // Grammar limits to [a-zA-Z]; e.g., 'č' should error.
        assert!(tokenize("čau").is_err(), "relax if you choose to support Unicode identifiers");
    }

    // ------------------------------------- Text strings ---------------------------------------

    #[test]
    fn basic_strings_and_supported_escapes() {
        let expected = vec![
            t_text(1, ""),
            t_text(1, "hello"),
            t_text(1, "line\nend"),     // \\n → newline
            t_text(1, "tab\tquote\""),  // \\t and \\" 
            t_text(1, "hex: Az"),       // \\x41 \\x7a → 'A' 'z'
        ];
        let input = r#""" "hello" "line\nend" "tab\tquote\"" "hex: \x41\x7a""#;
        assert_tokens(input, &expected);
    }

    #[test]
    fn multiline_text_with_actual_newline_affects_line_numbers() {
        // The string contains a real newline (not escaped), which should advance the lexer line.
        let input = "\"a\nb\"\nfalse";
        let tokens = assert_tokens(input, &[t_text(1, "a\nb"), t_bool(3, false)]);
        // EOF line should be 3 (start at 1, +1 for the newline inside the string, +1 for the newline after it)
        if let Token::Eof { line } = tokens.last().unwrap() {
            assert_eq!(*line, 3);
        }
    }

    #[test]
    fn string_errors_unterminated_and_bad_escapes() {
        assert!(tokenize("\"unterminated").is_err(), "missing closing quote must error");
        assert!(tokenize("\"bad\\escape\"").is_err(), "unknown escape must error");
        assert!(tokenize("\"bad\\x4G\"").is_err(), "non-hex digit in \\xHH must error");
        assert!(tokenize("\"short\\x4\"").is_err(), "incomplete \\xHH must error");
        assert!(tokenize("\"ends with backslash \\\\" ).is_err(), "dangling backslash must error");

        // Valid embedded quote:
        let expected = vec![t_text(1, "just\"ok")];
        assert_tokens(r#""just\"ok""#, &expected);
    }

    // ------------------------------------ Operators & Seps ------------------------------------

    #[test]
    fn operators_group_as_maximal_runs() {
        // Any run of [+*-/%&|~^<>=!]+ should become a single Operator token.
        let expected = vec![
            t_op(1, "++"),
            t_op(1, "+="),
            t_op(1, "==="),
            t_op(1, "!=="),
            t_op(1, "<===>"),
            t_op(1, "&||~^"),
        ];
        assert_tokens("++ += === !== <===> &||~^", &expected);
    }

    #[test]
    fn separators_all_supported() {
        let expected = vec![
            t_sep(1, '('), t_sep(1, ')'),
            t_sep(1, '['), t_sep(1, ']'),
            t_sep(1, '.'),
            t_sep(1, '{'), t_sep(1, '}'),
            t_sep(1, ':'),
            t_sep(1, ','),
            t_sep(1, ';'),
        ];
        assert_tokens("()[] . {} : , ;", &expected);
    }

    #[test]
    fn minus_before_number_is_operator_then_int() {
        let expected = vec![t_op(1, "-"), t_int(1, 123)];
        assert_tokens("-123", &expected);
    }

    // ------------------------------------ Errors / Unknowns -----------------------------------

    #[test]
    fn unknown_characters_error() {
        assert!(tokenize("@").is_err());
        assert!(tokenize("a?b").is_err());
        assert!(tokenize("\\").is_err());
        assert!(tokenize("$").is_err());
        assert!(tokenize("`").is_err());
    }

    // ---------------------------------- Mixed, multi-line -------------------------------------

    #[test]
    fn mixed_tokens_multiline_and_lines() {
        let input = r#"
true   42   7.5
name_one!=false,"text\nok":Z
( [ ] ) { } ; . :
"#;
        // Lines start at 1; input begins with a newline, so first token 'true' is on line 2.
        let expected = vec![
            t_bool(2, true),
            t_int(2, 42),
            t_real(2, 7.5),
            t_name(3, "name_one"),
            t_op(3, "!="),
            t_bool(3, false),
            t_sep(3, ','),
            t_text(3, "text\nok"),
            t_sep(3, ':'),
            t_name(3, "Z"),
            t_sep(4, '('), t_sep(4, '['), t_sep(4, ']'), t_sep(4, ')'),
            t_sep(4, '{'), t_sep(4, '}'),
            t_sep(4, ';'),
            t_sep(4, '.'),
            t_sep(4, ':'),
        ];
        let tokens = assert_tokens(input, &expected);

        // EOF should be on line 5 (lines 1..=5 due to leading newline and two more newlines)
        if let Token::Eof { line } = tokens.last().unwrap() {
            assert_eq!(*line, 5, "adjust if your EOF line policy differs");
        }
    }
}

#[cfg(test)]
mod prop_tests {
    use super::*;
    use proptest::prelude::*;

    fn digit() -> impl Strategy<Value = char> {
        prop::char::range('0', '9')
    }

    fn hexdigit() -> impl Strategy<Value = char> {
        prop_oneof![
            prop::char::range('0', '9'),
            prop::char::range('a', 'f'),
            prop::char::range('A', 'F'),
        ]
    }

    fn letter() -> impl Strategy<Value = char> {
        prop::char::any().prop_filter("letters", char::is_ascii_alphabetic)
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
        // first char = ASCII letter, rest = ASCII letter/digit/underscore
        (
            letter(),
            prop::collection::vec(
                prop_oneof![letter(), digit(), Just('_')], 0..10
            )
        ).prop_map(|(c, rest)| {
            let mut s = String::new();
            s.push(c);
            s.extend(rest);
            s
        })
    }

    fn operator_str() -> impl Strategy<Value = String> {
        prop::collection::vec(
            prop_oneof![
                Just('+'), Just('-'), Just('*'), Just('/'), Just('%'),
                Just('&'), Just('|'), Just('~'), Just('^'),
                Just('<'), Just('>'), Just('='), Just('!')
            ],
            1..5
        ).prop_map(|v| v.into_iter().collect())
    }

    fn separator_str() -> impl Strategy<Value = String> {
        prop_oneof![
            Just("(".to_string()), Just(")".to_string()),
            Just("[".to_string()), Just("]".to_string()),
            Just(".".to_string()), Just("{".to_string()), Just("}".to_string()),
            Just(":".to_string()), Just(",".to_string()), Just(";".to_string()),
        ]
    }

    fn text_str() -> impl Strategy<Value = String> {
        fn no_quote_or_backslash() -> impl Strategy<Value = String> {
            prop::char::any().prop_filter("exclude quotes/backslash", |c| {
                *c != '"' && *c != '\\'
            }).prop_map(|c| c.to_string())
        }

        // Generate simple text with valid escapes
        let char_strategy = prop_oneof![
            no_quote_or_backslash(),
            // allowed escapes
            Just("\\n".to_string()),
            Just("\\t".to_string()),
            Just("\\\"".to_string()),
            Just("\\\\".to_string()),
            (hexdigit(), hexdigit()).prop_map(|(a, b)| {
                format!("\\x{}{}", a, b)
            }),
        ];
        prop::collection::vec(char_strategy, 0..5)
            .prop_map(|parts| {
                format!("\"{}\"", parts.join(""))
            })
    }

    fn token_str() -> impl Strategy<Value = String> {
        prop_oneof![
            int_str(),
            real_str(),
            bool_str(),
            name_str(),
            operator_str(),
            separator_str(),
            text_str(),
        ]
    }

    // --- Property tests -----------------------------------------------------------------------

    proptest! {
        #[test]
        fn tokenize_ints(toks in prop::collection::vec(int_str(), 1..20)) {
            let correct = Vec::from_iter(toks.iter().map(|tok| {
                let n = i64::from_str_radix(tok, 10).unwrap();
                Token::Int { line: 1, value: n }
            }).chain([Token::Eof { line: 1 }]));

            let res = tokenize(&toks.join(" ")).unwrap();
            assert_eq!(correct, res, "ints not tokenized correctly")
        }

        #[test]
        fn tokenize_bools(toks in prop::collection::vec(bool_str(), 1..20)) {
            let correct = Vec::from_iter(toks.iter().map(|tok| {
                Token::Bool { line: 1, value: tok == "true" }
            }).chain([Token::Eof { line: 1 }]));

            let res = tokenize(&toks.join(" ")).unwrap();
            assert_eq!(correct, res, "bools not tokenized correctly")
        }

        #[test]
        fn tokenize_reals(toks in prop::collection::vec(real_str(), 1..20)) {
            let correct = Vec::from_iter(toks.iter().map(|tok| {
                let n: f64 = tok.parse().expect("unreachable");
                Token::Real { line: 1, value: n }
            }).chain([Token::Eof { line: 1 }]));

            let res = tokenize(&toks.join(" ")).expect("it should not fail");
            let eq = correct.iter().zip(res.iter()).all(|(check, result)| {
                if let Token::Real { value: v1, .. } = check {
                    if let Token::Real { value: v2, .. } = result {
                        f64::abs(v1 - v2) < 0.0001
                    } else {
                        false
                    }
                } else {
                    check == result
                }
            });
            assert!(eq, "reals not tokenized correctly")
        }

        #[test]
        fn tokenize_always_succeeds_on_valid_generated_tokens(
            toks in prop::collection::vec(token_str(), 1..20)
        ) {
            let input = toks.join(" ");
            let res = tokenize(&input);
            prop_assert!(res.is_ok(), "tokenize failed on: {}", input);

            let tokens = res.unwrap();
            // Always ends in EOF
            let ends_with_eof = matches!(tokens.last(), Some(Token::Eof { .. }));
            prop_assert!(ends_with_eof);

            // Every token (except EOF) matches grammar invariants
            for tok in &tokens[..tokens.len()-1] {
                match tok {
                    Token::Int { value, .. } => {
                        prop_assert!(*value >= 0, "ints should be nonnegative");
                    }
                    Token::Real { value, .. } => {
                        // real should contain a dot, so must not be NaN
                        prop_assert!(value.is_finite());
                    }
                    Token::Bool { .. } => {}
                    Token::Text { .. } => { }
                    Token::Operator { name, .. } => {
                        prop_assert!(!name.is_empty());
                        prop_assert!(name.chars().all(|c| "+*-/%&|~^<>=!".contains(c)));
                    }
                    Token::Separator { name, .. } => {
                        prop_assert!("()[] .}}{{:,;".contains(*name));
                    }
                    Token::Name { name, .. } => {
                        let mut chars = name.chars();
                        let first = chars.next().unwrap();
                        prop_assert!(first.is_ascii_alphabetic());
                        prop_assert!(chars.all(|c| c.is_ascii_alphanumeric() || c == '_'));
                    }
                    Token::Eof { .. } => unreachable!(),
                }
            }
        }
    }
}

