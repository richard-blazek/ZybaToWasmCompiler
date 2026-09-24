use crate::frontend::error::{Fallible, err};

// <input>     ::= (<blank> | <comment> | <token>)*
// <blank>     ::= any unicode whitespace character
// <comment>   ::= "#" [^\n]*
// <token>     ::= <real> | <int> | <bool> | <keyword> | <text> | <operator> | <separator> | <name>
// <real>      ::= [0-9]+ "." [0-9]*
// <int>       ::= [0-9]+
// <bool>      ::= "true" | "false"
// <keyword>   ::= "fun" | "if" | "elif" | "else" | "while" | "for" | "import" | "private"
// <name>      ::= ([a-zA-Z] [a-zA-Z0-9_]*) - <bool> - <keyword>
// <operator>  ::= <op_char>+
// <separator> ::= "(" | ")" | "[" | "]" | "{" | "}" | "." | ":" | "," | ";"
// <text>      ::= '"' (<text_char> | <text_esc>)* '"'
// <text_char> ::= [^"\\]
// <text_esc>  ::= "\\" ('"' | "\\" | "n" | "t" | "u" [0-9a-fA-F]{4} | "U" [0-9a-fA-F]{8})
// <op_char>   ::= "+" | "-" | "*" | "/" | "%" | "&" | "|" | "~" | "^" | "<" | ">" | "=" | "!"

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Real { line: i64, value: f64 },
    Int { line: i64, value: i64 },
    Text { line: i64, value: String },
    Bool { line: i64, value: bool },
    Operator { line: i64, name: String },
    Separator { line: i64, name: char },
    Name { line: i64, name: String },
    Fun { line: i64 },
    If { line: i64 },
    Elif { line: i64 },
    Else { line: i64 },
    While { line: i64 },
    For { line: i64 },
    Import { line: i64 },
    Private { line: i64 },
    Eof { line: i64 },
}

impl Token {
    pub fn line(&self) -> i64 {
        use Token::*;
        match self {
            Real { line, .. } | Int { line, .. } | Text { line, ..}
            | Bool { line, .. } | Operator { line, .. } | Eof { line }
            | Separator { line, .. } | Name { line, .. }
            | Fun { line } | If { line } | Elif { line } | Else { line }
            | While { line } | For { line } | Import { line } | Private { line } => *line
        }
    }

    fn int(line: i64, digits: &str) -> Fallible<Token> {
        match digits.parse() {
            Ok(value) => Ok(Token::Int { line, value }),
            Err(_) => err(line, "Value too large to fit in 64 bits".into()),
        }
    }

    fn real(line: i64, literal: &str) -> Fallible<Token> {
        match literal.parse::<f64>() {
            Ok(value) if value.is_finite() => Ok(Token::Real { line, value }),
            _ => err(line, "Value too large to fit in 64 bits".into()),
        }
    }

    fn name(line: i64, name: String) -> Token {
        match name.as_str() {
            "true" => Token::Bool { line, value: true },
            "false" => Token::Bool { line, value: false },
            "fun" => Token::Fun { line },
            "if" => Token::If { line },
            "elif" => Token::Elif { line },
            "else" => Token::Else { line },
            "while" => Token::While { line },
            "for" => Token::For { line },
            "import" => Token::Import { line },
            "private" => Token::Private { line },
            _ => Token::Name { line, name },
        }
    }
}

enum State {
    Initial,
    Comment,
    Int(String),
    Real(String),
    Text(String, Vec<char>),
    Separator(char),
    Operator(String),
    Name(String),
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
        c if c.is_digit(10) => Ok(State::Int(c.to_string())),
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
                (&['\\'], 'u' | 'U') => State::Text(s, vec!['\\', c]),
                (&['\\', kind, ref digits @ ..], _) if kind == 'u' || kind == 'U' => {
                    if !c.is_digit(16) {
                        err(cur_line, format!("{} is not a hex digit", c))?;
                    }
                    let hex: String = digits.iter().chain([&c]).collect();
                    if hex.len() < if kind == 'u' { 4 } else { 8 } {
                        let mut escape = escape.clone();
                        escape.push(c);
                        State::Text(s, escape)
                    } else if let Some(ch) = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()) {
                        State::Text(add_char(s, ch), vec![])
                    } else {
                        err(cur_line, format!("Invalid Unicode code point {}", hex))?
                    }
                }
                _ => err(cur_line, format!("Invalid escape {}", c))?
            };
            Ok((start_line, state, vec![]))
        }
        State::Int(digits) if c == '.' => Ok((cur_line, State::Real(add_char(digits, c)), vec![])),
        State::Int(digits) if c.is_ascii_digit() => Ok((cur_line, State::Int(add_char(digits, c)), vec![])),
        State::Int(digits) => {
            let token = Token::int(start_line, &digits)?;
            Ok((cur_line, new_state(start_line, c)?, vec![token]))
        }
        State::Real(literal) if c.is_ascii_digit() => Ok((cur_line, State::Real(add_char(literal, c)), vec![])),
        State::Real(literal) => {
            let token = Token::real(start_line, &literal)?;
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
        State::Real(literal) => tokens.push(Token::real(line, &literal)?),
        State::Int(digits) => tokens.push(Token::int(line, &digits)?),
        State::Separator(name) => tokens.push(Token::Separator { line, name }),
        State::Operator(name) => tokens.push(Token::Operator { line, name }),
        State::Name(name) => tokens.push(Token::name(line, name)),
        State::Text(_, _) => err(line, "Text literal not closed".into())?
    }

    tokens.push(Token::Eof { line });
    Ok(tokens)
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use proptest::prelude::*;

    const BLANKS: &str = " \n\r\t";
    const OP_CHARS: &str = "+-*/%&|~^<>=!";
    const SEPARATORS: &str = "()[]{}.:,;";
    const RESERVED: [&str; 10] = ["true", "false", "fun", "if", "elif", "else", "while", "for", "import", "private"];

    fn int(line: i64, value: i64) -> Token { Token::Int { line, value } }
    fn real(line: i64, value: f64) -> Token { Token::Real { line, value } }
    fn text(line: i64, value: &str) -> Token { Token::Text { line, value: value.to_string() } }
    fn name(line: i64, name: &str) -> Token { Token::Name { line, name: name.to_string() } }
    fn op(line: i64, name: &str) -> Token { Token::Operator { line, name: name.to_string() } }
    fn sep(line: i64, name: char) -> Token { Token::Separator { line, name } }
    fn eof(line: i64) -> Token { Token::Eof { line } }

    fn word(line: i64, word: &str) -> Token {
        match word {
            "true" => Token::Bool { line, value: true },
            "false" => Token::Bool { line, value: false },
            "fun" => Token::Fun { line },
            "if" => Token::If { line },
            "elif" => Token::Elif { line },
            "else" => Token::Else { line },
            "while" => Token::While { line },
            "for" => Token::For { line },
            "import" => Token::Import { line },
            "private" => Token::Private { line },
            _ => name(line, word),
        }
    }

    fn lex(src: &str) -> Vec<Token> {
        tokenize(src).unwrap_or_else(|e| panic!("{:?} should be accepted, got {:?}", src, e))
    }

    fn rejects(src: &str) {
        let result = tokenize(src);
        assert!(result.is_err(), "{:?} should be rejected, got {:?}", src, result);
    }

    fn line_of(src: &str) -> i64 {
        1 + src.matches('\n').count() as i64
    }

    #[derive(Debug, Clone)]
    enum Kind { Int(i64), Real(f64), Text(String), Word(String), Op(String), Sep(char) }

    impl Kind {
        fn token(&self, line: i64) -> Token {
            match self {
                Kind::Int(value) => int(line, *value),
                Kind::Real(value) => real(line, *value),
                Kind::Text(value) => text(line, value),
                Kind::Word(w) => word(line, w),
                Kind::Op(o) => op(line, o),
                Kind::Sep(c) => sep(line, *c),
            }
        }

        fn merges_with(&self, next: char) -> bool {
            match self {
                Kind::Word(_) => next.is_ascii_alphanumeric() || next == '_',
                Kind::Int(_) => next.is_ascii_digit() || next == '.',
                Kind::Real(_) => next.is_ascii_digit(),
                Kind::Op(_) => OP_CHARS.contains(next),
                Kind::Text(_) | Kind::Sep(_) => false,
            }
        }
    }

    fn int_lexeme() -> impl Strategy<Value = (String, Kind)> {
        "[0-9]{1,18}".prop_map(|s| { let value = s.parse().unwrap(); (s, Kind::Int(value)) })
    }

    fn real_lexeme() -> impl Strategy<Value = (String, Kind)> {
        "[0-9]{1,7}\\.[0-9]{0,7}".prop_map(|s| { let value = s.parse().unwrap(); (s, Kind::Real(value)) })
    }

    fn word_lexeme() -> impl Strategy<Value = (String, Kind)> {
        prop_oneof![
            "[a-zA-Z][a-zA-Z0-9_]{0,8}",
            prop::sample::select(RESERVED.to_vec()).prop_map(String::from),
        ].prop_map(|s| (s.clone(), Kind::Word(s)))
    }

    fn op_lexeme() -> impl Strategy<Value = (String, Kind)> {
        "[-+*/%&|~^<>=!]{1,4}".prop_map(|s| (s.clone(), Kind::Op(s)))
    }

    fn sep_lexeme() -> impl Strategy<Value = (String, Kind)> {
        prop::sample::select(SEPARATORS.chars().collect::<Vec<_>>()).prop_map(|c| (c.to_string(), Kind::Sep(c)))
    }

    fn text_piece() -> impl Strategy<Value = (String, char)> {
        prop_oneof![
            4 => any::<char>().prop_filter("must be escaped", |c| *c != '"' && *c != '\\').prop_map(|c| (c.to_string(), c)),
            1 => prop::sample::select(vec![("\\\"", '"'), ("\\\\", '\\'), ("\\n", '\n'), ("\\t", '\t')])
                .prop_map(|(s, c)| (s.to_string(), c)),
            1 => (any::<char>(), any::<bool>()).prop_filter("needs \\U", |(c, _)| (*c as u32) <= 0xFFFF)
                .prop_map(|(c, upper)| {
                    let s = if upper { format!("\\u{:04X}", c as u32) } else { format!("\\u{:04x}", c as u32) };
                    (s, c)
                }),
            1 => any::<char>().prop_map(|c| (format!("\\U{:08X}", c as u32), c)),
        ]
    }

    fn text_lexeme() -> impl Strategy<Value = (String, Kind)> {
        prop::collection::vec(text_piece(), 0..8).prop_map(|pieces| {
            let src = pieces.iter().map(|(s, _)| s.as_str()).collect::<String>();
            let value = pieces.iter().map(|(_, c)| *c).collect::<String>();
            (format!("\"{}\"", src), Kind::Text(value))
        })
    }

    fn lexeme() -> impl Strategy<Value = (String, Kind)> {
        prop_oneof![int_lexeme(), real_lexeme(), word_lexeme(), op_lexeme(), sep_lexeme(), text_lexeme()]
    }

    fn gap() -> impl Strategy<Value = String> {
        prop::collection::vec(prop_oneof![
            3 => prop::sample::select(BLANKS.chars().collect::<Vec<_>>()).prop_map(String::from),
            1 => "[^\n]{0,10}".prop_map(|c| format!("#{}\n", c)),
        ], 0..3).prop_map(|pieces| pieces.concat())
    }

    proptest! {
        #[test]
        fn tokenizes_like_reference_model(lexemes in prop::collection::vec((gap(), lexeme()), 0..20), end in gap()) {
            let mut src = String::new();
            let mut expected = vec![];
            for (i, (gap, (lexeme, kind))) in lexemes.iter().enumerate() {
                let first = lexeme.chars().next().unwrap();
                if gap.is_empty() && i > 0 && lexemes[i - 1].1.1.merges_with(first) {
                    src.push(' ');
                }
                src += gap;
                expected.push(kind.token(line_of(&src)));
                src += lexeme;
            }
            src += &end;
            expected.push(eof(line_of(&src)));
            prop_assert_eq!(tokenize(&src), Ok(expected), "source: {:?}", src);
        }

        #[test]
        fn real_values_are_correctly_rounded(src in "[0-9]{1,25}\\.[0-9]{0,30}") {
            let expected: f64 = src.parse().unwrap();
            prop_assert_eq!(tokenize(&src), Ok(vec![real(1, expected), eof(1)]));
        }

        #[test]
        fn single_character_is_accepted_iff_grammar_allows_it(c in any::<char>()) {
            let allowed = c.is_whitespace() || OP_CHARS.contains(c) || SEPARATORS.contains(c)
                || c.is_ascii_alphanumeric() || c == '#';
            prop_assert_eq!(tokenize(&c.to_string()).is_ok(), allowed, "character {:?}", c);
        }
    }

    #[test]
    fn whitespace_separates_tokens() {
        assert_eq!(lex("a \n\r\tb"), vec![name(1, "a"), name(2, "b"), eof(2)]);
        for c in ['\u{0B}', '\u{0C}', '\u{85}', '\u{A0}', '\u{2028}', '\u{3000}'] {
            assert_eq!(lex(&format!("a{}b", c)), vec![name(1, "a"), name(1, "b"), eof(1)]);
        }
    }

    #[test]
    fn characters_outside_alphabet_are_rejected() {
        for src in ["_", "_x", "x = _y", "'a'", "$", "@", "`", "?", "\\", "é", "\0", "x = 1 $ 2"] {
            rejects(src);
        }
    }

    #[test]
    fn int_literals() {
        assert_eq!(lex("0 007 42"), vec![int(1, 0), int(1, 7), int(1, 42), eof(1)]);
        assert_eq!(lex(&i64::MAX.to_string()), vec![int(1, i64::MAX), eof(1)]);
        rejects("9223372036854775808");
        rejects(&"9".repeat(45));
        assert_eq!(lex("123abc"), vec![int(1, 123), name(1, "abc"), eof(1)]);
        assert_eq!(lex("1;2"), vec![int(1, 1), sep(1, ';'), int(1, 2), eof(1)]);
    }

    #[test]
    fn real_literals() {
        assert_eq!(lex("1."), vec![real(1, 1.0), eof(1)]);
        assert_eq!(lex("0.5 3.25 10.0"), vec![real(1, 0.5), real(1, 3.25), real(1, 10.0), eof(1)]);
        assert_eq!(lex("1..2"), vec![real(1, 1.0), sep(1, '.'), int(1, 2), eof(1)]);
        assert_eq!(lex("1.2.3"), vec![real(1, 1.2), sep(1, '.'), int(1, 3), eof(1)]);
        assert_eq!(lex(".5"), vec![sep(1, '.'), int(1, 5), eof(1)]);
        assert_eq!(lex("1.5x"), vec![real(1, 1.5), name(1, "x"), eof(1)]);
        assert_eq!(lex("1.x"), vec![real(1, 1.0), name(1, "x"), eof(1)]);
    }

    #[test]
    fn real_with_whole_part_beyond_i64() {
        let src = "99999999999999999999.5";
        assert_eq!(tokenize(src), Ok(vec![real(1, src.parse().unwrap()), eof(1)]));
    }

    #[test]
    fn real_too_large_is_rejected() {
        rejects(&format!("{}.0", "9".repeat(400)));
    }

    #[test]
    fn real_with_long_fraction() {
        let src = format!("0.{}", "1".repeat(45));
        assert_eq!(tokenize(&src), Ok(vec![real(1, src.parse().unwrap()), eof(1)]));
    }

    #[test]
    fn real_values_are_nearest_f64() {
        for src in ["0.1", "123.456", "0.00000000000000000000001", "9007199254740993.0", "1.7976931348623157"] {
            assert_eq!(tokenize(src), Ok(vec![real(1, src.parse().unwrap()), eof(1)]), "source: {}", src);
        }
    }

    #[test]
    fn keywords_and_bools() {
        assert_eq!(lex("fun if elif else while for import private true false"), vec![
            Token::Fun { line: 1 }, Token::If { line: 1 }, Token::Elif { line: 1 }, Token::Else { line: 1 },
            Token::While { line: 1 }, Token::For { line: 1 }, Token::Import { line: 1 }, Token::Private { line: 1 },
            Token::Bool { line: 1, value: true }, Token::Bool { line: 1, value: false }, eof(1),
        ]);
    }

    #[test]
    fn words_resembling_keywords_are_names() {
        for w in ["iff", "fun_", "If", "TRUE", "true1", "elsee", "form", "imports", "privat", "x_1", "a", "Z9_"] {
            assert_eq!(lex(w), vec![name(1, w), eof(1)]);
        }
        assert_eq!(lex("if(x)"), vec![Token::If { line: 1 }, sep(1, '('), name(1, "x"), sep(1, ')'), eof(1)]);
    }

    #[test]
    fn operators_use_maximal_munch() {
        for c in OP_CHARS.chars() {
            assert_eq!(lex(&c.to_string()), vec![op(1, &c.to_string()), eof(1)]);
        }
        assert_eq!(lex(OP_CHARS), vec![op(1, OP_CHARS), eof(1)]);
        assert_eq!(lex("x=-1"), vec![name(1, "x"), op(1, "=-"), int(1, 1), eof(1)]);
        assert_eq!(lex("a==b"), vec![name(1, "a"), op(1, "=="), name(1, "b"), eof(1)]);
        assert_eq!(lex("< ="), vec![op(1, "<"), op(1, "="), eof(1)]);
        assert_eq!(lex("+#-\n-"), vec![op(1, "+"), op(2, "-"), eof(2)]);
    }

    #[test]
    fn separators_are_single_characters() {
        let expected: Vec<_> = SEPARATORS.chars().map(|c| sep(1, c)).chain([eof(1)]).collect();
        assert_eq!(lex(SEPARATORS), expected);
        assert_eq!(lex("::"), vec![sep(1, ':'), sep(1, ':'), eof(1)]);
    }

    #[test]
    fn text_literals() {
        assert_eq!(lex(r#""""#), vec![text(1, ""), eof(1)]);
        assert_eq!(lex(r#""a\"b\\c\nd\te""#), vec![text(1, "a\"b\\c\nd\te"), eof(1)]);
        assert_eq!(lex(r#""\u0041\u006a\u006A\u0000\u00e9\U0001F600""#), vec![text(1, "Ajj\0é😀"), eof(1)]);
        assert_eq!(lex(r#""\U00000041\U0010FFFF\uFFFF""#), vec![text(1, "A\u{10FFFF}\u{FFFF}"), eof(1)]);
        assert_eq!(lex(r##""# not a comment""##), vec![text(1, "# not a comment"), eof(1)]);
        assert_eq!(lex("\"tab\there, é, $\""), vec![text(1, "tab\there, é, $"), eof(1)]);
        assert_eq!(lex(r#""a""b"x"#), vec![text(1, "a"), text(1, "b"), name(1, "x"), eof(1)]);
    }

    #[test]
    fn invalid_text_literals() {
        for src in [r#""\g""#, r#""\r""#, r#""\x41""#, r#""\X41""#, r#""\u004""#, r#""\u00G0""#, r#""\u0G00""#,
                    r#""\U0000004""#, r#""\U0000004G""#, r#""\uD800""#, r#""\uDFFF""#, r#""\U00110000""#,
                    r#""\UFFFFFFFF""#, "\"\\", "\"\\u", "\"\\u004", "\"\\U0001", "\"abc", "\"\\\"", "'abc'"] {
            rejects(src);
        }
    }

    #[test]
    fn text_spanning_lines() {
        assert_eq!(lex("a \"x\ny\n\" b\nc"), vec![name(1, "a"), text(1, "x\ny\n"), name(3, "b"), name(4, "c"), eof(4)]);
    }

    #[test]
    fn comments() {
        assert_eq!(lex("# only a comment"), vec![eof(1)]);
        assert_eq!(lex("a # \"unclosed \\q $ é\n b"), vec![name(1, "a"), name(2, "b"), eof(2)]);
        assert_eq!(lex("a#b\nc"), vec![name(1, "a"), name(2, "c"), eof(2)]);
        assert_eq!(lex("1#\n#\n2"), vec![int(1, 1), int(3, 2), eof(3)]);
    }

    #[test]
    fn line_numbers() {
        assert_eq!(lex(""), vec![eof(1)]);
        assert_eq!(lex("\n\n"), vec![eof(3)]);
        assert_eq!(lex("a\r\nb\n\n(\n\"t\""), vec![name(1, "a"), name(2, "b"), sep(4, '('), text(5, "t"), eof(5)]);
        assert_eq!(lex("1\n2.5\nx\n+\n;\n\"s\"\nif\n"), vec![
            int(1, 1), real(2, 2.5), name(3, "x"), op(4, "+"), sep(5, ';'), text(6, "s"), Token::If { line: 7 }, eof(8),
        ]);
    }

    #[test]
    fn error_lines() {
        assert_eq!(tokenize("a$").unwrap_err().line, 1);
        assert_eq!(tokenize("a\n\n$").unwrap_err().line, 3);
        assert_eq!(tokenize("(\n$").unwrap_err().line, 2);
        assert_eq!(tokenize("x\n\"a\n\\q\"").unwrap_err().line, 3);
    }
}
