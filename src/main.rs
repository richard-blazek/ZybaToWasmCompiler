use crate::lexer::tokenize;

mod lexer;
mod error;

fn main() {
    let source_code = "25r100
        hello world + 1
        # hola
        \"Hello, \\\" world!\\\"\"
        123guten_5_Tag_";

    match tokenize(source_code) {
        Ok(v) => {
            println!("tokens: {:?}", v);
        },
        Err(error::Error{line: ln, message: msg}) => {
            panic!("{}:{}", ln, msg);
        }
    };
}
