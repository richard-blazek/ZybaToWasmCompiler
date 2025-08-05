#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub line: u32,
    pub message: String,
}

pub type Fallible<T> = Result<T, Error>;

pub fn err<T>(line: u32, message: String) -> Fallible<T> {
    Err(Error { line, message })
}
