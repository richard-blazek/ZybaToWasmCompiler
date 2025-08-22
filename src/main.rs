use std::collections::HashMap;

mod error;
mod lexer;
mod parser;
mod filesystem;
mod scope;
mod loader;
mod builtin;
mod typecheck;

fn main() {
    let mut files = HashMap::new();
    files.insert("math.zyba".to_string(), "
    factorial = fun[n: Int] Int {
        result = 1;
        for i : n {
            result = result * i;
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

    print = fun[x: Int] () {};

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
        result = List[Int];
        i = 0;
        while i < n {
            result.append[i + 1];
            i = i + 1;
        }
        result
    };

    private concat = fun[LoL: List[List[Int]]] List[Int] {
        result = List[Int];
        for item : LoL {
            result.append[item];
        }
        result
    };

    merge = fun[a: List[Int], b: List[Int]] List[Int] {
        result = List[Int];
        i = 0;
        while (i < a.count) & (i < b.count) {
            result.append[a.get[i], b.get[i]];
            i = i + 1;
        }
        result
    };
    
    main = fun[] () {
        nums_to_120 = range[math::factorial[5]];
        for i, num : nums_to_120 {
            print[i];
            print[num];
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

    let (main_fn, decls) = match scope::name_resolution(main_path, files) {
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

    match typecheck::check(decls) {
        Ok(_) => println!("Ok"),
        Err(e) => println!("Error: {:?}", e)
    };
}
