use std::collections::HashMap;

mod error;
mod lexer;
mod parser;
mod name_resolution;
mod loader;

fn main() {
    let mut files = HashMap::new();
    files.insert("math.zyba".to_string(), "
    factorial = fun[n: int] int {
        result = 1;
        for i : n {
            result = result * i;
        };
        return result;
    };".to_string());

    files.insert("main.zyba".to_string(), "
    import \"math.zyba\";

    private circleArea = fun[radius: real] real {
        return radius * radius * maths.pi;
    };

    isPrime = fun[n: int] bool {
        prime = true;
        if n < 2 {
            prime = false;
        } else {
            i = 2;
            while i * i <= n && prime {
                prime = n % i != 0;
                i = i + 1;
            };
        };
        return prime;
    };

    sum = fun[n: int.list] int {
        total = 0;
        for index, value : n {
            total = total + value;
        };
        return total;
    };

    range = fun[n: int] int.list {
        result = int.list;
        i = 0;
        while i < n {
            result.append[i + 1];
            i = i + 1;
        };
        return result;
    };

    private concat = fun[LoL: int.list.list] int.list {
        result = int.list;
        for item : LoL {
            result.append[item];
        };
        return result;
    };

    merge = fun[a: int.list, b: int.list] int.list {
        result = int.list;
        i = 0;
        while (i < a.count) && (i < b.count) {
            result.append[a.get[i], b.get[i]];
            i = i + 1;
        };
        return result;
    };".to_string());

    let fs = loader::playground_fs(files);

    match loader::load(&fs, "main.zyba") {
        Ok((main_path, files)) => {
            println!("Main: {}", main_path);
            for (name, decls) in files {
                println!("File: {}", name);
                println!("Content: {:?}", decls);
            }
        },
        Err(e) => panic!("{:?}", e)
    };
}
