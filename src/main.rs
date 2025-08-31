use std::collections::HashMap;

mod frontend;
mod midend;

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

    let fs = frontend::playground_fs(files);

    let (main_fn, decls) = match frontend::compile(&fs, "main.zyba") {
        Ok((main_path, files)) => {
            (main_path, files)
        },
        Err(error) => {
            panic!("Error: {:?}", error)
        }
    };

    println!("Main function: {}", main_fn);
    for (name, value) in decls.iter() {
        println!("Declaration: {}", name);
        println!("Value: {:?}", value);
    }

    let program = midend::codegen(&main_fn, decls);
    let main_id = program.entry();
    let main = &program.funcs()[main_id];

    println!("Main (#{}) - locals used: {:?}", main_id, main.locals());
    for (i, instr) in main.code().iter().enumerate() {
        println!("{}: {:?}", i, instr)
    }
}
