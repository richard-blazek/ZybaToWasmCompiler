#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub line: i64,
    pub message: String,
}

pub type Fallible<T> = Result<T, Error>;

pub fn err<T>(line: i64, message: String) -> Fallible<T> {
    Err(Error { line, message })
}
