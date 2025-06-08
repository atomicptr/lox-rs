use crate::lexer::Token;

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub enum BinaryOp {
    Eq,
    NotEq,
    Lesser,
    LesserEq,
    Greater,
    GreaterEq,
    Plus,
    Minus,
    Mul,
    Div,
}

impl TryFrom<Token> for BinaryOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::EqualEqual => Ok(BinaryOp::Eq),
            Token::NotEqual => Ok(BinaryOp::NotEq),
            Token::Less => Ok(BinaryOp::Lesser),
            Token::LessEqual => Ok(BinaryOp::LesserEq),
            Token::Greater => Ok(BinaryOp::Greater),
            Token::GreaterEqual => Ok(BinaryOp::GreaterEq),
            Token::Plus => Ok(BinaryOp::Plus),
            Token::Minus => Ok(BinaryOp::Minus),
            Token::Star => Ok(BinaryOp::Mul),
            Token::Slash => Ok(BinaryOp::Div),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl TryFrom<Token> for UnaryOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Bang => Ok(UnaryOp::Not),
            Token::Minus => Ok(UnaryOp::Neg),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(UnaryOp, Box<Expr>),
}

pub fn parse(tokens: Vec<(Token, usize)>) -> Result<Expr, ParserError> {
    let mut p = Parser::from(tokens);
    p.expression()
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<(Token, usize)>,
    index: usize,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, usize),
    ExpectedExpression(usize),
    CouldntFindRParen(usize),
}

/*

Expression Grammar:
-------------------

expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

*/

impl Parser {
    fn from(tokens: Vec<(Token, usize)>) -> Self {
        Self { tokens, index: 0 }
    }

    // expression     → equality ;
    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;

        while self.matches(&[Token::NotEqual, Token::EqualEqual]) {
            let operator = self
                .previous()
                .map(|(t, _)| t.clone())
                .unwrap()
                .try_into()
                .unwrap();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;

        while self.matches(&[
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let operator = self
                .previous()
                .map(|(t, _)| t.clone())
                .unwrap()
                .try_into()
                .unwrap();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while self.matches(&[Token::Minus, Token::Plus]) {
            let operator = self
                .previous()
                .map(|(t, _)| t.clone())
                .unwrap()
                .try_into()
                .unwrap();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while self.matches(&[Token::Slash, Token::Star]) {
            let operator = self
                .previous()
                .map(|(t, _)| t.clone())
                .unwrap()
                .try_into()
                .unwrap();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // unary          → ( "!" | "-" ) unary
    //                   | primary ;
    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[Token::Bang, Token::Minus]) {
            let operator = self
                .previous()
                .map(|(t, _)| t.clone())
                .unwrap()
                .try_into()
                .unwrap();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary()
    }

    // primary        → NUMBER | STRING | "true" | "false" | "nil"
    //                 | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[Token::Bool(false)]) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }

        if self.matches(&[Token::Bool(true)]) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }

        if self.matches(&[Token::Nil]) {
            return Ok(Expr::Literal(Value::Nil));
        }

        if let Some((current, _)) = self.current() {
            if let Token::String(str) = current {
                let str = str.clone();
                let _ = self.advance();
                return Ok(Expr::Literal(Value::String(str)));
            }

            if let Token::Number(num) = current {
                let num = num.clone();
                let _ = self.advance();
                return Ok(Expr::Literal(Value::Number(num)));
            }
        }

        if self.matches(&[Token::LParen]) {
            let expr = self.expression()?;

            if let Some(_) = self.consume(Token::RParen) {
                return Ok(Expr::Grouping(Box::new(expr)));
            }

            // TODO: get the index
            return Err(ParserError::CouldntFindRParen(0));
        }

        let (_, index) = self.current().unwrap();
        Err(ParserError::ExpectedExpression(index.clone()))
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some((Token::Semicolon, _)) = self.previous() {
                return;
            }

            if let Some((current, _)) = self.current() {
                match current {
                    Token::Class
                    | Token::Fun
                    | Token::Var
                    | Token::For
                    | Token::If
                    | Token::While
                    | Token::Print
                    | Token::Return => {
                        return;
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
        }
    }

    fn current(&self) -> Option<&(Token, usize)> {
        self.tokens.get(self.index)
    }

    fn previous(&self) -> Option<&(Token, usize)> {
        self.index
            .checked_sub(1)
            .and_then(|index| self.tokens.get(index))
    }

    fn matches(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.is_at_end() {
                return false;
            }

            if let Some((current, _)) = self.current() {
                if current.is_same_type(token) {
                    self.advance();
                    return true;
                }
            }
        }

        return false;
    }

    fn is_at_end(&self) -> bool {
        if let Some((Token::Eof, _)) = self.current() {
            return true;
        }

        return false;
    }

    fn advance(&mut self) -> Option<&(Token, usize)> {
        if !self.is_at_end() {
            self.index += 1;
        }
        self.previous()
    }

    fn consume(&mut self, token: Token) -> Option<&(Token, usize)> {
        if let Some((current, _)) = self.current() {
            if token == *current {
                return self.advance();
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_expression() {
        let ast = parse(vec![
            (Token::Number(5.0), 0),
            (Token::Plus, 1),
            (Token::Number(10.0), 2),
        ])
        .expect("can parse");

        if let Expr::Binary(a, BinaryOp::Plus, b) = ast {
            let a = *a;
            let b = *b;

            let matched = match (a, b) {
                (Expr::Literal(Value::Number(5.0)), Expr::Literal(Value::Number(10.0))) => true,
                _ => false,
            };

            assert!(matched, "parsed AST is wrong");

            return;
        }

        panic!("parsed AST is wrong");
    }
}
