use std::collections::HashMap;

mod error;
mod lexer;
mod parser;
mod filesystem;
mod loader;
mod nameres;
mod builtin;
mod typecheck;

mod highlevel_ir;

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

    files.insert("main.zyba".to_string(), "
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
            while i * i <= n & prime {
                prime = n % i != 0;
                i = i + 1;
            }
        }
        prime
    };

    sum = fun[n: List[Int]] Int {
        total = 0;
        for index, value : n {
            total = total + value;
        }
        total
    };

    range = fun[n: Int] List[Int] {
        result = list[Int];
        i = 0;
        while i < n {
            insert[result, len[result], i + 1];
            i = i + 1;
        }
        result
    };

    private concat = fun[LoL: List[List[Int]]] List[Int] {
        result = list[Int];
        for lst : LoL {
            for item : lst {
                insert[result, len[result], item];
            }
        }
        result
    };

    merge = fun[a: List[Int], b: List[Int]] List[Int] {
        result = list[Int];
        i = 0;
        while (i < len[a]) & (i < len[b]) {
            insert[result, len[result], get[a, i]];
            insert[result, len[result], get[b, i]];
            i = i + 1;
        }
        result
    };
    
    main = fun[] () {
        nums_to_120 = range[math::factorial[5]];
        for i, num : nums_to_120 {
            print[text[i]];
            print[text[num]];
        }
    };".to_string());

    let fs = filesystem::playground_fs(files);

    let (main_path, files) = match loader::load(&fs, "main.zyba") {
        Ok((main_path, files)) => {
            (main_path, files)
        },
        Err(e) => panic!("{:?}", e)
    };

    println!("Main: {}", main_path);
    for (name, decls) in files.iter() {
        println!("File: {}", name);
        println!("Content: {:?}", decls);
    }

    let (main_fn, decls) = match nameres::name_resolution(main_path, files) {
        Ok((main_fn, decls)) => {
            (main_fn, decls)
        }
        Err(e) => panic!("{:?}", e)
    };

    println!("Main function: {}", main_fn);
    for (name, value) in decls.iter() {
        println!("Declaration: {}", name);
        println!("Value: {:?}", value);
    }

    let decls = match typecheck::check(decls) {
        Ok(decls) => decls,
        Err(e) => panic!("Error: {:?}", e)
    };
    println!("Ok");

    highlevel_ir::codegen(&main_fn, decls);
}
