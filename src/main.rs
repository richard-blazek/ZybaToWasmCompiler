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

    private circle = fun[radius: Real] (r: Real, c: Real, a: Real) {
        t = ();
        u = t | (r: radius);
        p = u | (c: 2.0 * radius * math::pi);
        l = p | (a: radius * radius * math::pi);
        l
    };

    range = fun[n: Int] Array[Int] {
        result = array[Int, n];
        for i, value : result {
            set[result, i, i + 1];
        }
        result
    };
    
    main = fun[] () {
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

        c = circle[4.0];

        nums_to_120 = range[math::factorial[(8 >> 1) | 1]];
        for num : nums_to_120 {
            print[num, \" \", isPrime[num], \"\\n\"];
        }
    };".to_string());

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
