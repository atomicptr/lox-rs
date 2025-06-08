use crate::{interpreter::InterpreterError, lexer::LexerError, parser::ParserError};

fn pos_from_index(source: &str, index: usize) -> Option<(usize, usize)> {
    let mut line = 0;
    let mut col = 0;
    let mut current = 0;

    for c in source.chars() {
        if current == index {
            return Some((line, col));
        }

        if c == '\n' {
            col = 0;
            line += 1;
        } else {
            col += 1;
        }

        current += 1;
    }

    None
}

fn get_line(source: &str, line: usize) -> Option<String> {
    let lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();
    lines.get(line).map(|s| s.clone())
}

fn print_annotated_line(
    line: &String,
    linecol: Option<(usize, usize)>,
    annotation: Option<String>,
) {
    let mut l = 0;
    let mut c = 0;

    match linecol {
        Some((linenum, col)) => {
            l = linenum;
            c = col;

            println!("\t \x1b[37;2m{:3} |\x1b[0m {line}", linenum)
        }
        None => println!("\t {line}"),
    }

    if let Some(annotation) = annotation {
        let whitespace = " ".repeat(format!("\t {:3} | ", l).len() + c - 1);
        println!("\t{whitespace}\x1b[31m└─── {annotation}\x1b[0m"); // TODO: if too long split into multiple lines
    }
}

pub fn print_lexer_error(source: &String, err: LexerError) {
    let (message, index) = match err {
        LexerError::UnexpectedCharacter(c, index) => (format!("unexpected character {c}"), index),
        LexerError::UnterminatedString(index) => ("unterminated string".to_string(), index),
        LexerError::TrailingDot(index) => ("trailing dot".to_string(), index),
        LexerError::CouldNotParseNumber(str, err, index) => (
            format!("could not parse '{str}' into number: {:?}", err),
            index,
        ),
    };

    print_error_at(source, index, message.as_str());
}

pub fn print_parser_error(source: &String, err: ParserError) {
    let (message, index) = match err {
        ParserError::UnexpectedToken(token, index) => {
            (format!("unexpected token '{:?}'", token), index)
        }
        ParserError::CouldntFindRParen(index) => {
            ("could not find ')' after expression.".to_string(), index)
        }
        ParserError::ExpectedExpression(index) => ("expected expression".to_string(), index),
    };

    print_error_at(source, index, message.as_str());
}

pub fn print_interpreter_error(source: &String, err: InterpreterError) {
    let (message, index) = match err {
        InterpreterError::TypeError(val, index) => {
            (format!("value '{:?}' can't be used here", val), index)
        }
        InterpreterError::BinaryOpUnaryTypeError(value, op, index) => (
            format!("operator '{op}' can't be used with '{:?}'", value),
            index,
        ),
        InterpreterError::BinaryOpTyperError(lhs, op, rhs, index) => (
            format!(
                "operator '{op}' for '{:?}' and '{:?}' is not allowed",
                lhs, rhs
            ),
            index,
        ),
        InterpreterError::UnaryOpTypeError(value, op, index) => (
            format!("unary operator '{op}' can not be used with {:?}", value),
            index,
        ),
        InterpreterError::DivByZero(index) => ("division by zero".to_string(), index),
    };

    print_error_at(source, index, message.as_str());
}

pub fn print_error_at(source: &str, index: usize, error: &str) {
    // if index goes beyond source code, append one character and set index to the last char
    let (index, source) = if index >= source.len() {
        (source.len(), format!("{source} "))
    } else {
        (index, source.to_string())
    };

    if let Some((line, col)) = pos_from_index(&source, index) {
        if let Some(line_text) = get_line(&source, line) {
            println!("\n");

            for i in (1..6).rev() {
                if let Some(diff) = line.checked_sub(i) {
                    if let Some(prev) = get_line(&source, diff) {
                        print_annotated_line(&prev, Some((1 + line - i, col)), None);
                    }
                }
            }

            print_annotated_line(&line_text, Some((1 + line, col)), Some(error.to_string()));

            for i in 1..6 {
                if let Some(diff) = line.checked_add(i) {
                    if let Some(prev) = get_line(&source, diff) {
                        print_annotated_line(&prev, Some((1 + line + i, col)), None);
                    }
                }
            }

            println!("");
            return;
        }

        panic!("could not find line: {line} col: {col}, error: {error}");
    }

    panic!("could not find index: {index}, error: {error}");
}
