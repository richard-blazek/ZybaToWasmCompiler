use crate::lexer::tokenize;

mod error;
mod lexer;
mod parser;

fn main() {
    let source_code = "
        factorial = fun[n: int] int {
            result = 1
            for i : n {
                result = result * i
            }
            result
        }";

    let tokens = match tokenize(source_code) {
        Ok(tokens) => tokens,
        Err(e) => panic!("{:?}", e)
    };
    let file = match parser::parse(&tokens) {
        Ok(file) => file,
        Err(e) => panic!("{:?}", e)
    };

    println!("tokens: {:?}\n\n", tokens);
    println!("file: {:?}", file);
}
