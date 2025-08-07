use crate::lexer::tokenize;

mod error;
mod lexer;
mod parser;

fn main() {
    let source_code = "25r100
        hello world + 1
        # hola
        \"Hello, \\\" world!\\\"\"
        123guten_5_Tag_";

    match tokenize(source_code) {
        Ok(tokens) => {
            println!("tokens: {:?}", tokens);
            let x = parser::parse(&tokens);
        },
        Err(error::Error{line: ln, message: msg}) => {
            panic!("{}:{}", ln, msg);
        }
    };
}
