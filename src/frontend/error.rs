#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub file: String,
    pub line: i64,
    pub message: String,
}

impl Error {
    pub fn in_file(mut self, file: &str) -> Error {
        if self.file.is_empty() {
            self.file = file.to_string();
        }
        self
    }
}

pub type Fallible<T> = Result<T, Error>;

pub fn err<T>(line: i64, message: String) -> Fallible<T> {
    Err(Error { file: String::new(), line, message })
}
