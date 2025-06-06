#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    NotEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(f64),
    Bool(bool),
    And,
    Class,
    Else,
    If,
    Fun,
    For,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,
    Eof,
}

pub fn lexer(source: &str) -> Result<Vec<(Token, usize)>, usize> {
    let mut tokens = vec![];

    let mut current = 0;
    // let mut line = 0;

    let data: Vec<char> = source.chars().collect();

    while current < data.len() {
        if let Some(c) = data.get(current) {
            let token = match c {
                ' ' | '\n' | '\r' | '\t' => None,
                '(' => Some((Token::LParen, current)),
                ')' => Some((Token::RParen, current)),
                '{' => Some((Token::LBrace, current)),
                '}' => Some((Token::RBrace, current)),
                ',' => Some((Token::Comma, current)),
                '.' => Some((Token::Dot, current)),
                '-' => Some((Token::Minus, current)),
                '+' => Some((Token::Plus, current)),
                ';' => Some((Token::Semicolon, current)),
                '*' => Some((Token::Star, current)),
                '!' => {
                    if let Some(c) = data.get(current + 1) {
                        match c {
                            '=' => {
                                current += 1;
                                Some((Token::NotEqual, current))
                            }
                            _ => Some((Token::Bang, current)),
                        }
                    } else {
                        None
                    }
                }
                '=' => {
                    if let Some(c) = data.get(current + 1) {
                        match c {
                            '=' => {
                                current += 1;
                                Some((Token::EqualEqual, current))
                            }
                            _ => Some((Token::Equal, current)),
                        }
                    } else {
                        None
                    }
                }
                '>' => {
                    if let Some(c) = data.get(current + 1) {
                        match c {
                            '=' => {
                                current += 1;
                                Some((Token::GreaterEqual, current))
                            }
                            _ => Some((Token::Greater, current)),
                        }
                    } else {
                        None
                    }
                }
                '<' => {
                    if let Some(c) = data.get(current + 1) {
                        match c {
                            '=' => {
                                current += 1;
                                Some((Token::LessEqual, current))
                            }
                            _ => Some((Token::Less, current)),
                        }
                    } else {
                        None
                    }
                }
                '/' => {
                    if let Some(c) = data.get(current + 1) {
                        current += 1;

                        if *c == '/' {
                            // a comment, so lets go to the end of the line
                            while let Some(c) = data.get(current + 1) {
                                current += 1;

                                if *c == '\n' {
                                    break;
                                }
                            }
                            None
                        } else {
                            Some((Token::Slash, current))
                        }
                    } else {
                        None
                    }
                }
                '"' => {
                    if let Some(_) = data.get(current + 1) {
                        let mut tmp_curr = current + 1;

                        while let Some(c) = data.get(tmp_curr) {
                            if *c == '"' {
                                let slice = &data[(current + 1)..tmp_curr];
                                tokens.push(Some((Token::String(slice.iter().collect()), current)));
                                current = tmp_curr;
                                break;
                            }

                            tmp_curr += 1;
                        }
                        None
                    } else {
                        print_error_at(&source, current, "Unterminated string");
                        panic!("Unterminated string!");
                    }
                }
                '0'..'9' => {
                    if let Some(_) = data.get(current + 1) {
                        let mut tmp_curr = current + 1;

                        let mut pre = vec![c.clone()];
                        let mut post = vec![];

                        let mut has_dot = false;

                        while let Some(c) = data.get(tmp_curr) {
                            match (c, has_dot) {
                                ('0'..'9', false) => pre.push(c.clone()),
                                ('0'..'9', true) => post.push(c.clone()),
                                ('.', false) => has_dot = true,
                                ('.', true) => {
                                    print_error_at(&source, tmp_curr, "Unexpected '.'");
                                    panic!("Unexpected '.'");
                                }
                                _ => {
                                    // number is over
                                    break;
                                }
                            }

                            tmp_curr += 1;
                        }

                        // has trailing dot
                        if has_dot && post.is_empty() {
                            print_error_at(&source, current, "trailing dot is not allowed");
                            panic!("trailing dot is not allowed");
                        }

                        let pre: String = pre.iter().collect();
                        let post: String = post.iter().collect();

                        match format!("{}.{}", pre, post).parse::<f64>() {
                            Ok(num) => {
                                let start = current;
                                current = tmp_curr;
                                Some((Token::Number(num), start))
                            }
                            Err(err) => {
                                print_error_at(
                                    &source,
                                    current,
                                    format!("could not parse number {:?}", err).as_str(),
                                );
                                panic!("could not parse number");
                            }
                        }
                    } else {
                        None
                    }
                }

                // identifier matching
                c if c.is_alphanumeric() => {
                    let mut ident = vec![c.clone()];

                    let mut tmp_curr = current + 1;

                    while let Some(c) = data.get(tmp_curr) {
                        if !c.is_alphanumeric() && *c != '_' {
                            break;
                        }

                        ident.push(c.clone());

                        tmp_curr += 1;
                    }

                    current = tmp_curr;

                    let ident: String = ident.iter().collect();

                    Some((
                        match ident.as_str() {
                            "and" => Token::And,
                            "class" => Token::Class,
                            "else" => Token::Else,
                            "false" => Token::Bool(false),
                            "for" => Token::For,
                            "fun" => Token::Fun,
                            "if" => Token::If,
                            "nil" => Token::Nil,
                            "or" => Token::Or,
                            "print" => Token::Print,
                            "return" => Token::Return,
                            "super" => Token::Super,
                            "this" => Token::This,
                            "true" => Token::Bool(true),
                            "var" => Token::Var,
                            "while" => Token::While,
                            _ => Token::Identifier(ident),
                        },
                        current,
                    ))
                }

                // nothing matches, error
                c => {
                    print_error_at(source, current, format!("Unknown character: {c}").as_str());
                    panic!("Unknown character {c}");
                }
            };

            tokens.push(token);
        } else {
            tokens.push(Some((Token::Eof, current)));
        }

        // println!("Tokens until {current}: {:?}", tokens);

        current += 1;
    }

    Ok(tokens.into_iter().flatten().collect())
}

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

fn print_error_at(source: &str, index: usize, error: &str) {
    if let Some((line, col)) = pos_from_index(&source, index) {
        if let Some(line_text) = get_line(&source, line) {
            println!("\n");

            // TODO: automatically determine how much context we wanna show
            if let Some(diff) = line.checked_sub(2) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line - 2);
                }
            }

            if let Some(diff) = line.checked_sub(1) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line - 1);
                }
            }

            println!("\t{} |{line_text}", line);
            println!(
                "\t{}^--- {error}",
                " ".repeat(format!("{} |", line).len() + col)
            );

            if let Some(diff) = line.checked_add(1) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line + 1);
                }
            }

            if let Some(diff) = line.checked_add(2) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line + 2);
                }
            }

            println!("");
            return;
        }

        panic!("could not find line: {line} col: {col}");
    }

    panic!("could not find index: {index}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_test_parens() {
        let tokens = lexer("({( )})").expect("could parse");
        let mut tokens = tokens.iter();

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LParen);
        assert_eq!(*pos, 0);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LBrace);
        assert_eq!(*pos, 1);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LParen);
        assert_eq!(*pos, 2);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RParen);
        assert_eq!(*pos, 4);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RBrace);
        assert_eq!(*pos, 5);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RParen);
        assert_eq!(*pos, 6);
    }

    #[test]
    fn lex_test_string() {
        let tokens = lexer("\"this is a test\", \"lox\"").expect("could parse");
        let mut tokens = tokens.iter();

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::String("this is a test".to_string()));
        assert_eq!(*pos, 0);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Comma);
        assert_eq!(*pos, 16);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::String("lox".to_string()));
        assert_eq!(*pos, 18);
    }

    #[test]
    fn lex_test_numbers() {
        let tokens = lexer("1.234 12.34 123.4 1234.0 1234").expect("couldnt parse");
        let mut tokens = tokens.iter();

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(1.234));
        assert_eq!(*pos, 0);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(12.34));
        assert_eq!(*pos, 6);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(123.4));
        assert_eq!(*pos, 12);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(1234.0));
        assert_eq!(*pos, 18);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(1234.0));
        assert_eq!(*pos, 25);
    }
}
