mod frontend;
mod midend;
mod backend;

fn main() {
    let path = match std::env::args().skip(1).next() {
        None => {
            eprintln!("Error: source file path expected");
            std::process::exit(1);
        },
        Some(path) => path,
    };

    let fs: &dyn frontend::FS = &frontend::system_fs();
    let (main_fn, decls) = match frontend::compile(fs, &path) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("Error: {:?}", error);
            std::process::exit(1);
        }
    };

    let program = midend::codegen(&main_fn, decls);
    let wasm = backend::to_wasm(program);
    println!("{}", wasm);
}
