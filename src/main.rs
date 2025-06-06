use std::{
    env, fs,
    io::{Write, stdin, stdout},
    process::exit,
};

use lexer::lexer;

mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap()),
        _ => {
            println!("Usage: lox [script]");
            exit(1);
        }
    }
}

fn run_prompt() {
    loop {
        let mut s = String::new();

        print!("> ");
        let _ = stdout().flush();

        stdin().read_line(&mut s).expect("could not read string");

        run(s.trim().to_string());
    }
}

fn run_file(file: &String) {
    let source = fs::read_to_string(file).expect("error reading file");
    let _ = run(source);
}

fn run(code: String) {
    let tokens = lexer(&code);

    if tokens.is_err() {
        panic!("Err: {:?}", tokens);
    }

    let tokens = tokens.unwrap();

    println!("Code:\n\n{code}\n\nTokens:\n\n{:?}", tokens);
}
