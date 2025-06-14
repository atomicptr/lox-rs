use std::{
    env, fs,
    io::{Write, stdin, stdout},
    process::exit,
};

use interpreter::Interpreter;
use lexer::lexer;
use parser::Value;

use crate::{
    errormsg::{LoxError, handle_lox_error},
    parser::print_stmt,
    resolver::resolve,
};

mod builtins;
mod errormsg;
mod interpreter;
mod lexer;
mod parser;
mod resolver;

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
    let mut interpreter = Interpreter::default();

    println!("Welcome to \x1b[1mlox-rs\x1b[0m!");

    loop {
        let mut s = String::new();

        print!("\x1b[31;1m>>>\x1b[0m ");
        let _ = stdout().flush();

        stdin().read_line(&mut s).expect("could not read string");

        let mut s = s.trim().to_string();

        // an unfinished statement
        if !s.ends_with(";") && !s.ends_with("}") {
            s += ";";
        }

        match run(&mut interpreter, &s) {
            Ok(val) => println!("{}", val),
            Err(err) => handle_lox_error(&s, err),
        }
    }
}

fn run_file(file: &String) {
    let source = fs::read_to_string(file).expect("error reading file");

    let mut interpreter = Interpreter::default();

    match run(&mut interpreter, &source) {
        Ok(_) => {}
        Err(err) => handle_lox_error(&source, err),
    }
}

fn run(interpreter: &mut Interpreter, code: &String) -> Result<Value, LoxError> {
    let tokens = lexer(&code)?;

    println!("Tokens {:?}", tokens);

    let stmts = parser::parse(tokens)?;

    for stmt in stmts.iter() {
        print_stmt(stmt, 0, None);
    }

    resolve(interpreter, &stmts)?;

    Ok(interpreter.run(&stmts)?)
}
