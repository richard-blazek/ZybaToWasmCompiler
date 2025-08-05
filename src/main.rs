use crate::lexer::tokenize;

mod lexer;
mod error;

fn main() {
    let x = match tokenize("hello world + 1") {
        Ok(v) => v,
        Err(error::Error{line: ln, message: msg}) => panic!("{}:{}", ln, msg)
    };

    println!("Hello, world! {}", x[0].0);
}
