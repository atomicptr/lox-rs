use crate::errormsg::print_error_at;

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
                        match *c {
                            '/' => {
                                // a comment, so lets go to the end of the line
                                while let Some(c) = data.get(current + 1) {
                                    current += 1;

                                    if *c == '\n' {
                                        break;
                                    }
                                }
                                None
                            }

                            // a multiline comment
                            '*' => {
                                while let Some(c) = data.get(current + 1) {
                                    if *c == '*' {
                                        if let Some('/') = data.get(current + 2) {
                                            // end of multiline comment found
                                            current += 2;
                                            break;
                                        }
                                    }

                                    current += 1;
                                }
                                None
                            }

                            // just a free standing slash
                            _ => Some((Token::Slash, current)),
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

                        let start = current;

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
                                current = tmp_curr - 1;
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

                    let start = current;

                    let mut tmp_curr = current + 1;

                    while let Some(c) = data.get(tmp_curr) {
                        if !c.is_alphanumeric() && *c != '_' {
                            break;
                        }

                        ident.push(c.clone());

                        tmp_curr += 1;
                    }

                    current = tmp_curr - 1;

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
                        start,
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

        println!(
            "Tokens until {current}: {:?}",
            tokens
                .iter()
                .filter(|t| t.is_some())
                .map(|t| t.clone().unwrap())
                .collect::<Vec<(Token, usize)>>()
        );

        current += 1;
    }

    Ok(tokens.into_iter().flatten().collect())
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

        assert_eq!(0, tokens.len(), "handled all tokens");
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

        assert_eq!(0, tokens.len(), "handled all tokens");
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

        assert_eq!(0, tokens.len(), "handled all tokens");
    }

    #[test]
    fn lex_test_comments() {
        let tokens = lexer("777 // lucky number").expect("could be parsed");
        assert_eq!(1, tokens.len());

        let tokens =
            lexer("777 /* lucky number but\n\n\talso the answer */ 42").expect("could be parsed");

        assert_eq!(2, tokens.len());
    }

    #[test]
    fn lex_test_mini_program() {
        let tokens = lexer(
            "var x = 1337;\nvar y = 3;\nif ((x + y) == 1340) {\n\tprint \"HIT\";\n}\nprint 13.37;\n",
        )
        .expect("able to parse");

        let mut tokens = tokens.iter();

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Var);
        assert_eq!(*pos, 0);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Identifier("x".to_string()));
        assert_eq!(*pos, 4);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Equal);
        assert_eq!(*pos, 6);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(1337.0));
        assert_eq!(*pos, 8);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Semicolon);
        assert_eq!(*pos, 12);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Var);
        assert_eq!(*pos, 14);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Identifier("y".to_string()));
        assert_eq!(*pos, 18);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Equal);
        assert_eq!(*pos, 20);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(3.0));
        assert_eq!(*pos, 22);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Semicolon);
        assert_eq!(*pos, 23);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::If);
        assert_eq!(*pos, 25);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LParen);
        assert_eq!(*pos, 28);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LParen);
        assert_eq!(*pos, 29);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Identifier("x".to_string()));
        assert_eq!(*pos, 30);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Plus);
        assert_eq!(*pos, 32);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Identifier("y".to_string()));
        assert_eq!(*pos, 34);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RParen);
        assert_eq!(*pos, 35);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::EqualEqual);
        assert_eq!(*pos, 38);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(1340.0));
        assert_eq!(*pos, 40);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RParen);
        assert_eq!(*pos, 44);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::LBrace);
        assert_eq!(*pos, 46);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Print);
        assert_eq!(*pos, 49);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::String("HIT".to_string()));
        assert_eq!(*pos, 55);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Semicolon);
        assert_eq!(*pos, 60);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::RBrace);
        assert_eq!(*pos, 62);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Print);
        assert_eq!(*pos, 64);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Number(13.37));
        assert_eq!(*pos, 70);

        let (t, pos) = tokens.next().unwrap();
        assert_eq!(*t, Token::Semicolon);
        assert_eq!(*pos, 75);

        assert_eq!(0, tokens.len(), "handled all tokens");
    }
}
