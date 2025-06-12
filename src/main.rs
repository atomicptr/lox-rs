use std::{
    env, fs,
    io::{Write, stdin, stdout},
    process::exit,
};

use errormsg::{print_interpreter_error, print_lexer_error, print_parser_error};
use interpreter::{Env, Interpreter, RuntimeError};
use lexer::{LexerError, lexer};
use parser::{ParserError, print_stmt};

mod errormsg;
mod interpreter;
mod lexer;
mod parser;

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

    loop {
        let mut s = String::new();

        print!("> ");
        let _ = stdout().flush();

        stdin().read_line(&mut s).expect("could not read string");

        let s = s.trim().to_string();

        match run(&mut interpreter, &s) {
            Ok(()) => {}
            Err(LoxError::LexerError(err)) => print_lexer_error(&s, err),
            Err(LoxError::ParserError(err)) => print_parser_error(&s, err),
            Err(LoxError::InterpreterError(err)) => print_interpreter_error(&s, err),
        }
    }
}

fn run_file(file: &String) {
    let source = fs::read_to_string(file).expect("error reading file");

    let mut interpreter = Interpreter::default();

    match run(&mut interpreter, &source) {
        Ok(()) => {}
        Err(LoxError::LexerError(err)) => print_lexer_error(&source, err),
        Err(LoxError::ParserError(err)) => print_parser_error(&source, err),
        Err(LoxError::InterpreterError(err)) => print_interpreter_error(&source, err),
    }
}

enum LoxError {
    LexerError(LexerError),
    ParserError(ParserError),
    InterpreterError(RuntimeError),
}

impl From<LexerError> for LoxError {
    fn from(value: LexerError) -> Self {
        LoxError::LexerError(value)
    }
}

impl From<ParserError> for LoxError {
    fn from(value: ParserError) -> Self {
        LoxError::ParserError(value)
    }
}

impl From<RuntimeError> for LoxError {
    fn from(value: RuntimeError) -> Self {
        LoxError::InterpreterError(value)
    }
}

fn run(interpreter: &mut Interpreter, code: &String) -> Result<(), LoxError> {
    let tokens = lexer(&code)?;

    let stmts = parser::parse(tokens)?;

    for stmt in stmts.iter() {
        print_stmt(stmt, 0);
    }

    let value = interpreter.run(&stmts)?;

    println!("{}", value);

    Ok(())
}
