use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
    interpreter::{Env, Interpreter, RuntimeError},
    lexer::Token,
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Func(Fn),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Value::String(s) => format!("\"{s}\""),
            Value::Number(n) => format!("{n}"),
            Value::Bool(b) => format!("{b}"),
            Value::Func(fun) => format!("{fun}"),
            Value::Nil => "nil".to_string(),
        };

        write!(f, "{res}")?;

        Ok(())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

#[derive(Debug, Clone)]
pub enum Fn {
    LoxFunc(String, Vec<String>, Vec<Stmt>),
    NativeFunc(NativeFn),
}

impl Fn {
    pub fn arity(&self) -> usize {
        match self {
            Fn::LoxFunc(_, params, _) => params.len(),
            Fn::NativeFunc(fun) => match fun {
                NativeFn::ZeroArity(_) => 0,
                NativeFn::OneArity(_) => 1,
                NativeFn::TwoArity(_) => 2,
            },
        }
    }
}

impl Display for Fn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Fn::LoxFunc(name, params, _) => {
                format!("<fun {name}/{}({})>", self.arity(), params.join(", "))
            }
            Fn::NativeFunc(_) => format!("<native fun/{}(...)>", self.arity()),
        };

        write!(f, "{res}")?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum NativeFn {
    ZeroArity(fn(usize) -> Result<Value, RuntimeError>),
    OneArity(fn(usize, Value) -> Result<Value, RuntimeError>),
    TwoArity(fn(usize, Value, Value) -> Result<Value, RuntimeError>),
}

impl NativeFn {
    pub fn as_value(self) -> Value {
        Value::Func(Fn::NativeFunc(self))
    }
}

#[derive(Debug, Copy, Clone)]
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            BinaryOp::Eq => "=",
            BinaryOp::NotEq => "==",
            BinaryOp::Lesser => "<",
            BinaryOp::LesserEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
        };

        write!(f, "{res}")?;

        Ok(())
    }
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

#[derive(Debug, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            LogicalOp::Or => "or",
            LogicalOp::And => "and",
        };

        write!(f, "{res}")?;

        Ok(())
    }
}

impl TryFrom<Token> for LogicalOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Or => Ok(LogicalOp::Or),
            Token::And => Ok(LogicalOp::And),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        };

        write!(f, "{res}")?;

        Ok(())
    }
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

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>, usize),
    Grouping(Box<Expr>, (usize, usize)),
    Literal(Value, usize),
    Logical(Box<Expr>, LogicalOp, Box<Expr>, usize),
    Unary(UnaryOp, Box<Expr>, usize),
    Variable(String, usize),
    Assignment(String, Box<Expr>, usize),
    Call(Box<Expr>, Vec<Box<Expr>>, usize),
}

impl Expr {
    pub fn token_index(&self) -> usize {
        match self {
            Expr::Binary(_, _, _, index) => index.clone(),
            Expr::Grouping(_, (start, _)) => start.clone(),
            Expr::Literal(_, index) => index.clone(),
            Expr::Unary(_, _, index) => index.clone(),
            Expr::Variable(_, index) => index.clone(),
            Expr::Assignment(_, _, index) => index.clone(),
            Expr::Logical(_, _, _, index) => index.clone(),
            Expr::Call(_, _, index) => index.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Box<Expr>),
    Expr(Box<Expr>),
    Var(String, Box<Expr>),
    Block(Vec<Stmt>),
    Func(String, Vec<String>, Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Break(usize),
    Continue(usize),
    Return(Option<Box<Expr>>, usize),
}

pub fn parse(tokens: Vec<(Token, usize)>) -> Result<Vec<Stmt>, Vec<ParserError>> {
    let mut p = Parser::from(tokens);
    p.program()
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<(Token, usize)>,
    index: usize,
    loop_depth: Vec<()>,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, usize),
    ExpectedExpression(usize),
    CouldntFindRParen(usize),
    ExpectedSemicolonAfterStmt(usize),
    ExpectedSemicolonAfterExpr(usize),
    InvalidAssignmentTarget(usize),
    ExpectedRBraceAfterBlock(usize),
    ExpectedLParenAfter(String, usize),
    ExpectedRParenAfter(String, usize),
    ExpectedSemicolonAfterLoopCondition(usize),
    MaximumArgsExceeded(usize),
    ExpectedName(String, usize),
    ExpectedLBraceBeforeBody(String, usize),
}

/*

Expression Grammar:
-------------------

program        → declaration* EOF ;
declaration    → funDecl | varDecl | statement;
funDecl        → "fun" function ;
function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
statement      → exprStmt | "break" | "continue" | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
exprStmt       → expression ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
printStmt      → "print" expression ";" ;
returnStmt     → "return" expression? ";" ;
whileStmt      → "while" "(" expression ")" statement ;
block          → "{" declaration* "}" ;
expression     → assignment ;
assignment     → IDENTIFIER "=" assignment | logic_or;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" )* ;
arguments      → expression ( "," expression )* ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" | IDENTIFIER ;

*/

impl Parser {
    fn from(tokens: Vec<(Token, usize)>) -> Self {
        Self {
            tokens,
            index: 0,
            loop_depth: Vec::default(),
        }
    }

    // program        → declaration* EOF ;
    fn program(&mut self) -> Result<Vec<Stmt>, Vec<ParserError>> {
        let mut stmts = vec![];

        let mut errors = vec![];

        while !self.is_at_end() {
            let stmt = self.declaration();

            if stmt.is_err() {
                errors.push(stmt.unwrap_err());
                self.sync();
                continue;
            }

            stmts.push(stmt.unwrap());
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(stmts)
    }

    // declaration    → varDecl | statement;
    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[Token::Fun]) {
            self.function()
        } else if self.matches(&[Token::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    // function       → IDENTIFIER "(" parameters? ")" block ;
    fn function(&mut self) -> Result<Stmt, ParserError> {
        if let Some((Token::Identifier(name), index)) =
            self.consume(Token::Identifier("".to_string()))
        {
            let name = name.clone();
            let index = index.clone();

            if self.consume_isnt(Token::LParen) {
                return Err(ParserError::ExpectedLParenAfter(
                    format!("function {name}"),
                    index,
                ));
            }

            let mut params = vec![];

            let (curr, _) = self.current().unwrap();

            if *curr != Token::RParen {
                if let Some((Token::Identifier(name), _)) =
                    self.consume(Token::Identifier("".to_string()))
                {
                    params.push(name.clone());
                }

                while self.matches(&[Token::Comma]) {
                    if let Some((Token::Identifier(name), index)) =
                        self.consume(Token::Identifier("".to_string()))
                    {
                        if params.len() >= 255 {
                            return Err(ParserError::MaximumArgsExceeded(index.clone()));
                        }
                        params.push(name.clone());
                    }
                }
            }

            if self.consume_isnt(Token::RParen) {
                return Err(ParserError::ExpectedRParenAfter(
                    format!("function {name}"),
                    index,
                ));
            }

            if self.consume_isnt(Token::LBrace) {
                let (_, index) = self.previous().unwrap();
                return Err(ParserError::ExpectedLBraceBeforeBody(
                    format!("function {name}"),
                    index.clone(),
                ));
            }

            let body = self.block()?;

            return Ok(Stmt::Func(name, params, body));
        }

        let (_, index) = self.previous().unwrap();
        Err(ParserError::ExpectedName(
            "function".to_string(),
            index.clone(),
        ))
    }

    // varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        if let Some((Token::Identifier(name), index)) =
            self.consume(Token::Identifier("".to_string()))
        {
            let name = name.clone();
            let index = index.clone();

            let initializer = if self.matches(&[Token::Equal]) {
                Some(self.expression()?)
            } else {
                None
            };

            if let Some(_) = self.consume(Token::Semicolon) {
                return Ok(Stmt::Var(
                    name,
                    Box::new(initializer.unwrap_or(Expr::Literal(Value::Nil, index.clone()))),
                ));
            }

            // TODO: after var decl
            return Err(ParserError::ExpectedSemicolonAfterStmt(index));
        }

        // syntax error not an identifier after var
        let (token, index) = self.current().unwrap();
        Err(ParserError::UnexpectedToken(token.clone(), index.clone()))
    }

    // statement      → exprStmt | forStmt | ifStmt | printStmt | whileStmt | block ;
    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[Token::Break, Token::Continue]) {
            let (token, index) = self.previous().unwrap();
            let (token, index) = (token.clone(), index.clone());

            if !self.inside_loop() {
                Err(ParserError::UnexpectedToken(token, index))
            } else if self.consume_is(Token::Semicolon) {
                match token {
                    Token::Break => Ok(Stmt::Break(index)),
                    Token::Continue => Ok(Stmt::Continue(index)),
                    _ => unreachable!(),
                }
            } else {
                Err(ParserError::ExpectedSemicolonAfterStmt(index))
            }
        } else if self.matches(&[Token::For]) {
            self.for_stmt()
        } else if self.matches(&[Token::If]) {
            self.if_stmt()
        } else if self.matches(&[Token::Print]) {
            self.print_stmt()
        } else if self.matches(&[Token::Return]) {
            self.return_stmt()
        } else if self.matches(&[Token::While]) {
            self.while_stmt()
        } else if self.matches(&[Token::LBrace]) {
            let stmts = self.block()?;
            Ok(Stmt::Block(stmts))
        } else {
            self.expr_stmt()
        }
    }

    // block          → "{" declaration* "}" ;
    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];

        while !self.is_at_end() {
            if let Some((token, _)) = self.current() {
                if *token == Token::RBrace {
                    break;
                }

                stmts.push(self.declaration()?);
            }
        }

        if let Some(_) = self.consume(Token::RBrace) {
            return Ok(stmts);
        }

        let (_, index) = self.previous().unwrap();
        Err(ParserError::ExpectedRBraceAfterBlock(index.clone()))
    }

    // exprStmt       → expression ";" ;
    fn expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;

        if let Some(_) = self.consume(Token::Semicolon) {
            Ok(Stmt::Expr(Box::new(expr)))
        } else {
            Err(ParserError::ExpectedSemicolonAfterExpr(expr.token_index()))
        }
    }

    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    fn for_stmt(&mut self) -> Result<Stmt, ParserError> {
        if self.consume_isnt(Token::LParen) {
            let (_, index) = self.previous().unwrap();
            return Err(ParserError::ExpectedLParenAfter(
                "for".to_string(),
                index.clone(),
            ));
        }

        let initializer = if self.matches(&[Token::Semicolon]) {
            None
        } else if self.matches(&[Token::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expr_stmt()?)
        };

        let (cond_token, cond_index) = self.current().unwrap();
        let cond_index = cond_index.clone();

        let condition = if *cond_token != Token::Semicolon {
            Some(self.expression()?)
        } else {
            None
        };

        if self.consume_isnt(Token::Semicolon) {
            return Err(ParserError::ExpectedSemicolonAfterLoopCondition(cond_index));
        }

        let (incr_token, incr_index) = self.current().unwrap();
        let incr_index = incr_index.clone();

        let incr = if *incr_token != Token::Semicolon {
            Some(self.expression()?)
        } else {
            None
        };

        if self.consume_isnt(Token::RParen) {
            return Err(ParserError::ExpectedRParenAfter(
                "for clause".to_string(),
                incr_index,
            ));
        }

        self.loop_depth.push(());
        let body = self.statement()?;
        self.loop_depth.pop().unwrap();

        // transform for loop into while loop

        // transform body into while loop with condition (or infinite loop if condition is empty)
        let body = Stmt::While(
            Box::new(condition.unwrap_or(Expr::Literal(Value::Bool(true), cond_index))),
            Box::new(body),
            // put incr into the "after" special block
            incr.map(|stmt| Box::new(Stmt::Expr(Box::new(stmt)))),
        );

        // prepend initializer in front of while loop
        if let Some(initializer) = initializer {
            return Ok(Stmt::Block(vec![initializer, body]));
        }

        Ok(body)
    }

    // ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        if let Some(_) = self.consume(Token::LParen) {
            let condition = self.expression()?;

            if let Some(_) = self.consume(Token::RParen) {
                let then_branch = self.statement()?;
                let else_branch = if self.matches(&[Token::Else]) {
                    Some(Box::new(self.statement()?))
                } else {
                    None
                };

                return Ok(Stmt::If(
                    Box::new(condition),
                    Box::new(then_branch),
                    else_branch,
                ));
            }

            return Err(ParserError::ExpectedRParenAfter(
                "if condition".to_string(),
                condition.token_index(),
            ));
        }

        let (_, index) = self.previous().unwrap();
        Err(ParserError::ExpectedLParenAfter(
            "if".to_string(),
            index.clone(),
        ))
    }

    // printStmt      → "print" expression ";" ;
    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;

        if let Some(_) = self.consume(Token::Semicolon) {
            Ok(Stmt::Print(Box::new(expr)))
        } else {
            Err(ParserError::ExpectedSemicolonAfterStmt(expr.token_index()))
        }
    }

    // returnStmt     → "return" expression? ";" ;
    fn return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let (_, return_index) = self.previous().unwrap();
        let return_index = return_index.clone();

        let (curr, _) = self.current().unwrap();

        let expr = if *curr != Token::Semicolon {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        if self.consume_isnt(Token::Semicolon) {
            return Err(ParserError::ExpectedSemicolonAfterStmt(return_index));
        }

        Ok(Stmt::Return(expr, return_index))
    }

    // whileStmt      → "while" "(" expression ")" statement ;
    fn while_stmt(&mut self) -> Result<Stmt, ParserError> {
        if let Some(_) = self.consume(Token::LParen) {
            let condition = self.expression()?;

            if let Some(_) = self.consume(Token::RParen) {
                self.loop_depth.push(());
                let body = self.statement()?;
                self.loop_depth.pop().unwrap();

                return Ok(Stmt::While(Box::new(condition), Box::new(body), None));
            }

            return Err(ParserError::ExpectedRParenAfter(
                "while condition".to_string(),
                condition.token_index(),
            ));
        }

        let (_, index) = self.previous().unwrap();
        Err(ParserError::ExpectedLParenAfter(
            "while".to_string(),
            index.clone(),
        ))
    }

    // expression     → assignment ;
    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    // assignment     → IDENTIFIER "=" assignment | logic_or;
    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.logic_or()?;

        if self.matches(&[Token::Equal]) {
            let (_, eq_index) = self.previous().unwrap();
            let index = eq_index.clone();

            let value = self.assignment()?;

            if let Expr::Variable(name, index) = expr {
                return Ok(Expr::Assignment(name, Box::new(value), index));
            }

            return Err(ParserError::InvalidAssignmentTarget(index));
        }

        Ok(expr)
    }

    // logic_or       → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.logic_and()?;

        while self.matches(&[Token::Or]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();

            let rhs = self.logic_and()?;

            expr = Expr::Logical(Box::new(expr), operator, Box::new(rhs), op_index);
        }

        Ok(expr)
    }

    // logic_and      → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;

        while self.matches(&[Token::And]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();

            let rhs = self.logic_and()?;

            expr = Expr::Logical(Box::new(expr), operator, Box::new(rhs), op_index);
        }

        Ok(expr)
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;

        while self.matches(&[Token::NotEqual, Token::EqualEqual]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right), op_index);
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
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right), op_index);
        }

        Ok(expr)
    }

    // term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;

        while self.matches(&[Token::Minus, Token::Plus]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right), op_index);
        }

        Ok(expr)
    }

    // factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;

        while self.matches(&[Token::Slash, Token::Star]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right), op_index);
        }

        Ok(expr)
    }

    // unary          → ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[Token::Bang, Token::Minus]) {
            let (operator, op_index) = self
                .previous()
                .map(|(t, i)| (t.clone().try_into().unwrap(), i.clone()))
                .unwrap();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right), op_index));
        }

        self.call()
    }

    // call           → primary ( "(" arguments? ")" )* ;
    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(&[Token::LParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut args = vec![];

        let (curr, index) = self.current().unwrap();
        let index = index.clone();

        if *curr != Token::RParen {
            let expr = self.expression()?;
            args.push(Box::new(expr));

            while self.matches(&[Token::Comma]) {
                let expr = self.expression()?;
                if args.len() >= 255 {
                    return Err(ParserError::MaximumArgsExceeded(expr.token_index()));
                }
                args.push(Box::new(expr));
            }
        }

        if let Some((_, index)) = self.consume(Token::RParen) {
            return Ok(Expr::Call(Box::new(callee), args, index.clone()));
        }

        Err(ParserError::ExpectedRParenAfter(
            "function".to_string(),
            index,
        ))
    }

    // primary        → NUMBER | STRING | "true" | "false" | "nil"
    //                | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[Token::Nil]) {
            let (_, index) = self.current().unwrap();
            return Ok(Expr::Literal(Value::Nil, *index));
        }

        if let Some((current, index)) = self.current() {
            let index = index.clone();

            if let Token::Bool(b) = current {
                let b = b.clone();
                let _ = self.advance();
                return Ok(Expr::Literal(Value::Bool(b), index));
            }

            if let Token::String(str) = current {
                let str = str.clone();
                let _ = self.advance();
                return Ok(Expr::Literal(Value::String(str), index));
            }

            if let Token::Number(num) = current {
                let num = num.clone();
                let _ = self.advance();
                return Ok(Expr::Literal(Value::Number(num), index));
            }
        }

        if self.matches(&[Token::Identifier("".to_string())]) {
            let (token, index) = self.previous().unwrap();

            if let Token::Identifier(name) = token {
                return Ok(Expr::Variable(name.clone(), index.clone()));
            }

            panic!("this should not happen");
        }

        if self.matches(&[Token::LParen]) {
            let expr = self.expression()?;

            let (_, start) = self.current().unwrap();
            let start = start.clone();

            if let Some((_, end)) = self.consume(Token::RParen) {
                let end = end.clone();
                return Ok(Expr::Grouping(Box::new(expr), (start, end)));
            }

            return Err(ParserError::CouldntFindRParen(expr.token_index()));
        }

        let (_, index) = self.current().unwrap();
        Err(ParserError::ExpectedExpression(index.clone()))
    }

    fn sync(&mut self) {
        self.advance();

        // pop one if available
        self.loop_depth.pop();

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
            if token.is_same_type(current) {
                return self.advance();
            }
        }
        None
    }

    fn consume_is(&mut self, token: Token) -> bool {
        if let Some(_) = self.consume(token) {
            true
        } else {
            false
        }
    }

    fn consume_isnt(&mut self, token: Token) -> bool {
        !self.consume_is(token)
    }

    fn inside_loop(&self) -> bool {
        !self.loop_depth.is_empty()
    }
}

pub fn print_stmt(stmt: &Stmt, indent_level: usize) {
    let indent = " ".repeat(indent_level * 4);

    match stmt {
        Stmt::Print(expr) => {
            println!("{indent}Print");
            print_expr(expr, indent_level + 1);
        }
        Stmt::Expr(expr) => {
            println!("{indent}Expr");
            print_expr(expr, indent_level + 1);
        }
        Stmt::Var(name, expr) => {
            println!("{indent}Set var '{name}':");
            print_expr(expr, indent_level + 1);
        }
        Stmt::Block(stmts) => {
            println!("{indent}Block:");

            for stmt in stmts.iter() {
                print_stmt(stmt, indent_level + 1);
            }
        }
        Stmt::If(expr, then_branch, else_branch) => {
            println!("{indent}If:");

            print_expr(expr, indent_level + 1);
            print_stmt(then_branch, indent_level + 1);

            if let Some(else_branch) = else_branch {
                print_stmt(else_branch, indent_level + 1);
            }
        }
        Stmt::While(condition, body, after) => {
            println!("{indent}While:");

            print_expr(condition, indent_level + 1);
            print_stmt(body, indent_level + 1);

            if let Some(after) = after {
                print_stmt(after, indent_level + 1);
            }
        }
        Stmt::Break(_) => println!("{indent}BREAK"),
        Stmt::Continue(_) => println!("{indent}CONTINUE"),
        Stmt::Func(name, params, body) => {
            println!("{indent}Func {name}({})", params.join(", "));

            for stmt in body {
                print_stmt(stmt, indent_level + 1);
            }
        }
        Stmt::Return(expr, _) => {
            println!("{indent}Return");

            if let Some(expr) = expr {
                print_expr(expr, indent_level + 1);
            }
        }
    };
}

pub fn print_expr(expr: &Expr, indent_level: usize) {
    let indent = " ".repeat(indent_level * 4);

    match expr {
        Expr::Binary(left, op, right, _) => {
            println!("{indent}Binary (Operator: {:?})", op);
            print_expr(left, indent_level + 1);
            print_expr(right, indent_level + 1);
        }
        Expr::Grouping(inner_expr, _) => {
            println!("{indent}Group:");
            print_expr(inner_expr, indent_level + 1);
        }
        Expr::Unary(op, expr, _) => {
            println!("{indent}Unary: (Operator {:?})", op);
            print_expr(expr, indent_level + 1);
        }
        Expr::Literal(value, _) => match value {
            Value::String(s) => println!("{indent}Literal (String) = {s}"),
            Value::Number(n) => println!("{indent}Literal (Number) = {n}"),
            Value::Bool(b) => println!("{indent}Literal (Bool) = {b}"),
            Value::Func(fun) => println!("{indent}Literal (Func) = {fun}"),
            Value::Nil => println!("{indent}Literal (Nil)"),
        },
        Expr::Variable(name, _) => println!("{indent}var {name}"),
        Expr::Assignment(name, expr, _) => println!("{indent}assign var {name} = {:?}", expr),
        Expr::Logical(lhs, op, rhs, _) => {
            println!("{indent}Logical (Operator {:?})", op);
            print_expr(lhs, indent_level + 1);
            print_expr(rhs, indent_level + 1);
        }
        Expr::Call(callee, args, _) => {
            println!("{indent}Call Func");
            print_expr(callee, indent_level + 1);

            for arg in args {
                print_expr(arg, indent_level + 2);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
