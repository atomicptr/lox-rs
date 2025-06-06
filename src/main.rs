use std::{
    env, fs,
    io::{Write, stdin, stdout},
    process::exit,
};

use lexer::{LexerError, lexer, print_lexer_error};

mod errormsg;
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

        let s = s.trim().to_string();

        let res = run(&s);
        if res.is_err() {
            print_lexer_error(&s, res.unwrap_err());
        }
    }
}

fn run_file(file: &String) {
    let source = fs::read_to_string(file).expect("error reading file");
    let res = run(&source);

    if res.is_err() {
        print_lexer_error(&source, res.unwrap_err());
    }
}

fn run(code: &String) -> Result<(), LexerError> {
    let tokens = lexer(&code)?;

    println!("Code:\n\n{code}\n\nTokens:\n\n{:?}", tokens);

    Ok(())
}
