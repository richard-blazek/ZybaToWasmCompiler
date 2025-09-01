use std::collections::HashMap;

mod frontend;
mod midend;
mod backend;

fn main() {
    let mut files = HashMap::new();
    files.insert("math.zyba".to_string(), "
    factorial = fun[n: Int] Int {
        result = 1;
        i = 1;
        while i <= n {
            result = result * i;
            i = i + 1;
        }
        result
    };
    
    pi = 3.142;
    ".to_string());

    files.insert("funcs.zyba".to_string(), "
    import \"math.zyba\";

    private circleArea = fun[radius: Real] Real {
        radius * radius * math::pi
    };

    isPrime = fun[n: Int] Bool {
        prime = true;
        if n < 2 {
            prime = false;
        } else {
            i = 2;
            while i * i <= n && prime {
                prime = n % i != 0;
                i = i + 1;
            }
        }
        prime
    };

    sum = fun[n: Array[Int]] Int {
        total = 0;
        for index, value : n {
            total = total + value;
        }
        total
    };

    range = fun[n: Int] Array[Int] {
        result = array[Int, n];
        i = 0;
        while i < n {
            set[result, i, i + 1];
            i = i + 1;
        }
        result
    };
    
    main = fun[] () {
        nums_to_120 = range[math::factorial[5]];
        for i, num : nums_to_120 {
            print[chr[65 + (i % 26)]];
        }
    };".to_string());

    files.insert("hello.zyba".to_string(), "
    main = fun[] () {
        result = array[Int, 10];
        i = 1
        while i <= 10 {
            print[\"Hey\"];
            i = i + 1;
        }
    };
    ".to_string());

    let fs: &dyn frontend::FS = &frontend::playground_fs(files);

    let (main_fn, decls) = match frontend::compile(fs, "funcs.zyba") {
        Ok((main_path, files)) => {
            (main_path, files)
        },
        Err(error) => {
            panic!("Error: {:?}", error)
        }
    };

    let program = midend::codegen(&main_fn, decls);
    let wasm = backend::to_wasm(program);
    println!("{}", wasm);
}
