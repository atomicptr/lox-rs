use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    errormsg::print_error_at,
    interpreter::{Env, RuntimeError},
    lexer::Token,
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Func(Fn, Option<Rc<Value>>),
    Class(
        String,
        Option<Rc<Value>>,
        Rc<RefCell<HashMap<String, Value>>>,
    ),
    Instance(u64, Box<Value>, Rc<RefCell<HashMap<String, Value>>>),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Value::String(s) => format!("\"{s}\""),
            Value::Number(n) => format!("{n}"),
            Value::Bool(b) => format!("{b}"),
            Value::Func(fun, _) => format!("{fun}"),
            Value::Class(name, parent, _) => match parent {
                Some(parent) => format!("<class {name} {}>", parent.as_ref()),
                None => format!("<class {name}>"),
            },
            Value::Instance(id, class, props) => match class.as_ref() {
                Value::Class(name, _, _) => format!(
                    "{name} #{id} {{{}}}",
                    props
                        .borrow()
                        .iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                _ => unreachable!(),
            },
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
    LoxFunc(Option<String>, Vec<String>, Vec<Stmt>, Rc<RefCell<Env>>),
    LoxMethod(String, Vec<String>, Vec<Stmt>, Rc<RefCell<Env>>),
    NativeFunc(fn(usize, &Vec<Value>) -> Result<Value, RuntimeError>, usize),
}

impl Fn {
    pub fn arity(&self) -> usize {
        match self {
            Fn::LoxFunc(_, params, _, _) => params.len(),
            Fn::LoxMethod(_, params, _, _) => params.len(),
            Fn::NativeFunc(_, arity) => arity.clone(),
        }
    }
}

impl Display for Fn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Fn::LoxFunc(name, params, _, _) => {
                format!(
                    "<fun {}/{}({})>",
                    name.clone().unwrap_or(String::from("closure")),
                    self.arity(),
                    params.join(", "),
                )
            }
            Fn::LoxMethod(name, params, _, _) => {
                format!("<method {name}/{}({})>", self.arity(), params.join(", "),)
            }
            Fn::NativeFunc(_, _) => format!("<native fun/{}(...)>", self.arity()),
        };

        write!(f, "{res}")?;

        Ok(())
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
    Assign(String, Box<Expr>, usize),
    Call(Box<Expr>, Vec<Box<Expr>>, usize),
    Closure(Vec<(String, usize)>, Vec<Stmt>, usize),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>, usize),
    ReadProperty(Box<Expr>, String, usize),
    WriteProperty(Box<Expr>, String, Box<Expr>, usize),
    This(usize),
    Super(String, usize),
}

impl Expr {
    pub fn token_index(&self) -> usize {
        match self {
            Expr::Binary(_, _, _, index) => *index,
            Expr::Grouping(_, (start, _)) => *start,
            Expr::Literal(_, index) => *index,
            Expr::Unary(_, _, index) => *index,
            Expr::Variable(_, index) => *index,
            Expr::Assign(_, _, index) => *index,
            Expr::Logical(_, _, _, index) => *index,
            Expr::Call(_, _, index) => *index,
            Expr::Closure(_, _, index) => *index,
            Expr::Ternary(_, _, _, index) => *index,
            Expr::ReadProperty(_, _, index) => *index,
            Expr::WriteProperty(_, _, _, index) => *index,
            Expr::This(index) => *index,
            Expr::Super(_, index) => *index,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Box<Expr>, usize),
    Expr(Box<Expr>),
    Var(String, Option<Box<Expr>>, usize),
    Block(Vec<Stmt>),
    Func(String, Vec<(String, usize)>, Vec<Stmt>, usize),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>, usize),
    While(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>, usize),
    Break(usize),
    Continue(usize),
    Return(Option<Box<Expr>>, usize),
    Class(String, Option<Box<Expr>>, Vec<Stmt>, usize),
}

impl Stmt {
    pub fn stmt_index(&self) -> usize {
        match self {
            Stmt::Print(_, index) => index.clone(),
            Stmt::Expr(expr) => expr.token_index(),
            Stmt::Var(_, _, index) => index.clone(),
            Stmt::Block(stmts) => stmts
                .first()
                .expect("tried to access empty block")
                .stmt_index(),
            Stmt::Func(_, _, _, index) => index.clone(),
            Stmt::If(_, _, _, index) => index.clone(),
            Stmt::While(_, _, _, index) => index.clone(),
            Stmt::Break(index) => index.clone(),
            Stmt::Continue(index) => index.clone(),
            Stmt::Return(_, index) => index.clone(),
            Stmt::Class(_, _, _, index) => index.clone(),
        }
    }
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
    ExpectedLBraceBeforeBody(String, usize),
    ExpectedName(String, usize),
    ExpectedMemberNameAfterDot(usize),
    ExpectedDotAfterSuper(usize),
}

impl Parser {
    fn from(tokens: Vec<(Token, usize)>) -> Self {
        Self {
            tokens,
            index: 0,
            loop_depth: Vec::default(),
        }
    }

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

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[Token::Class]) {
            self.class()
        } else if self.matches(&[Token::Fun]) {
            self.function()
        } else if self.matches(&[Token::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class(&mut self) -> Result<Stmt, ParserError> {
        let class_index = self.previous_index().unwrap();

        if let Some((name, index)) = self.consume_identifier() {
            let superclass = if let Some((_, index)) = self.consume(Token::Less) {
                let index = index.clone();

                if let Some((superclass, superclass_index)) = self.consume_identifier() {
                    Some(Box::new(Expr::Variable(superclass, superclass_index)))
                } else {
                    return Err(ParserError::ExpectedName(String::from("superclass"), index));
                }
            } else {
                None
            };

            if self.consume_isnt(Token::LBrace) {
                return Err(ParserError::ExpectedLBraceBeforeBody(
                    format!("class {name}"),
                    index,
                ));
            }

            let mut methods = vec![];

            // parse body
            while !self.current_is(Token::RBrace) {
                let method = self.function()?;
                methods.push(method);
            }

            if self.consume_isnt(Token::RBrace) {
                return Err(ParserError::ExpectedRBraceAfterBlock(index.clone()));
            }

            return Ok(Stmt::Class(name, superclass, methods, class_index));
        }

        Err(ParserError::ExpectedName("class".to_string(), class_index))
    }

    fn function(&mut self) -> Result<Stmt, ParserError> {
        let fn_index = self.previous_index().unwrap();

        if let Some((name, _)) = self.consume_identifier() {
            let params = self.parameters(format!("function {name}"))?;

            if self.consume_isnt(Token::LBrace) {
                let (_, index) = self.previous().unwrap();
                return Err(ParserError::ExpectedLBraceBeforeBody(
                    format!("function {name}"),
                    index.clone(),
                ));
            }

            let body = self.block()?;

            return Ok(Stmt::Func(name, params, body, fn_index));
        }

        Ok(Stmt::Expr(Box::new(self.function_expr()?)))
    }

    fn function_expr(&mut self) -> Result<Expr, ParserError> {
        let (_, prev_index) = self.previous().unwrap();
        let prev_index = prev_index.clone();

        let params = self.parameters(String::from("closure"))?;

        if self.consume_isnt(Token::LBrace) {
            let (_, index) = self.previous().unwrap();
            return Err(ParserError::ExpectedLBraceBeforeBody(
                String::from("closure"),
                index.clone(),
            ));
        }

        let body = self.block()?;

        Ok(Expr::Closure(params, body, prev_index))
    }

    fn parameters(&mut self, what: String) -> Result<Vec<(String, usize)>, ParserError> {
        if self.consume_isnt(Token::LParen) {
            let (_, index) = self.previous().unwrap();
            return Err(ParserError::ExpectedLParenAfter(what, index.clone()));
        }

        let mut params = vec![];

        let (curr, _) = self.current().unwrap();

        if *curr != Token::RParen {
            if let Some((name, index)) = self.consume_identifier() {
                params.push((name, index));
            }

            while self.matches(&[Token::Comma]) {
                if let Some((name, index)) = self.consume_identifier() {
                    if params.len() >= 255 {
                        return Err(ParserError::MaximumArgsExceeded(index));
                    }
                    params.push((name, index));
                }
            }
        }

        if self.consume_isnt(Token::RParen) {
            let (_, index) = self.previous().unwrap();
            return Err(ParserError::ExpectedRParenAfter(what, index.clone()));
        }

        Ok(params)
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        if let Some((name, index)) = self.consume_identifier() {
            let initializer = if self.matches(&[Token::Equal]) {
                Some(self.expression()?)
            } else {
                None
            };

            if self.consume_is(Token::Semicolon) {
                return Ok(Stmt::Var(
                    name,
                    initializer.map(|expr| Box::new(expr)),
                    index,
                ));
            }

            // TODO: after var decl
            return Err(ParserError::ExpectedSemicolonAfterStmt(index));
        }

        // syntax error not an identifier after var
        let (token, index) = self.current().unwrap();
        Err(ParserError::UnexpectedToken(token.clone(), index.clone()))
    }

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

        if self.consume_is(Token::RBrace) {
            return Ok(stmts);
        }

        let (_, index) = self.previous().unwrap();
        Err(ParserError::ExpectedRBraceAfterBlock(index.clone()))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;

        if self.consume_is(Token::Semicolon) {
            Ok(Stmt::Expr(Box::new(expr)))
        } else {
            Err(ParserError::ExpectedSemicolonAfterExpr(expr.token_index()))
        }
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParserError> {
        let for_index = self.previous_index().unwrap();

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
            for_index,
        );

        // prepend initializer in front of while loop
        if let Some(initializer) = initializer {
            return Ok(Stmt::Block(vec![initializer, body]));
        }

        Ok(body)
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        let if_index = self.previous_index().unwrap();

        if self.consume_is(Token::LParen) {
            let condition = self.expression()?;

            if self.consume_is(Token::RParen) {
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
                    if_index,
                ));
            }

            return Err(ParserError::ExpectedRParenAfter(
                "if condition".to_string(),
                condition.token_index(),
            ));
        }

        Err(ParserError::ExpectedLParenAfter("if".to_string(), if_index))
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let print_index = self.previous_index().unwrap();

        let expr = self.expression()?;

        if self.consume_is(Token::Semicolon) {
            Ok(Stmt::Print(Box::new(expr), print_index))
        } else {
            Err(ParserError::ExpectedSemicolonAfterStmt(expr.token_index()))
        }
    }

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

    fn while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let while_index = self.previous_index().unwrap();

        if self.consume_is(Token::LParen) {
            let condition = self.expression()?;

            if self.consume_is(Token::RParen) {
                self.loop_depth.push(());
                let body = self.statement()?;
                self.loop_depth.pop().unwrap();

                return Ok(Stmt::While(
                    Box::new(condition),
                    Box::new(body),
                    None,
                    while_index,
                ));
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

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.ternary()
    }

    fn ternary(&mut self) -> Result<Expr, ParserError> {
        let expr = self.assignment()?;

        if self.matches(&[Token::QuestionMark]) {
            let (_, qm_index) = self.previous().unwrap();
            let qm_index = qm_index.clone();

            let then_expr = self.expression()?;

            if self.consume_isnt(Token::Colon) {
                panic!("ternary needs :");
            }

            let else_expr = self.expression()?;

            return Ok(Expr::Ternary(
                Box::new(expr),
                Box::new(then_expr),
                Box::new(else_expr),
                qm_index,
            ));
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.logic_or()?;

        if self.matches(&[Token::Equal]) {
            let (_, eq_index) = self.previous().unwrap();
            let index = eq_index.clone();

            let value = self.assignment()?;

            if let Expr::Variable(name, index) = expr {
                return Ok(Expr::Assign(name, Box::new(value), index));
            }

            if let Expr::ReadProperty(lhs, name, index) = expr {
                return Ok(Expr::WriteProperty(lhs, name, Box::new(value), index));
            }

            return Err(ParserError::InvalidAssignmentTarget(index));
        }

        Ok(expr)
    }

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

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(&[Token::LParen]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(&[Token::Dot]) {
                if let Some((name, index)) = self.consume_identifier() {
                    expr = Expr::ReadProperty(Box::new(expr), name, index)
                } else {
                    let index = self.previous_index().unwrap();
                    return Err(ParserError::ExpectedMemberNameAfterDot(index));
                }
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

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[Token::Nil]) {
            let (_, index) = self.current().unwrap();
            return Ok(Expr::Literal(Value::Nil, *index));
        }

        if self.consume_is(Token::Fun) {
            let expr = self.function_expr()?;
            return Ok(expr);
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

        if let Some((_, index)) = self.consume(Token::This) {
            return Ok(Expr::This(index.clone()));
        }

        if let Some((_, super_index)) = self.consume(Token::Super) {
            let super_index = super_index.clone();

            if self.consume_isnt(Token::Dot) {
                return Err(ParserError::ExpectedDotAfterSuper(super_index.clone()));
            }

            if let Some((name, index)) = self.consume_identifier() {
                return Ok(Expr::Super(name, index));
            }

            return Err(ParserError::ExpectedName(
                "superclass method".to_string(),
                super_index.clone(),
            ));
        }

        if let Some((name, index)) = self.consume_identifier() {
            return Ok(Expr::Variable(name, index));
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

    fn previous_index(&self) -> Option<usize> {
        self.previous().map(|(_, index)| index.clone())
    }

    fn matches(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            assert!(!token.is_value_type(), "cant consume value types");

            if self.is_at_end() {
                return false;
            }

            if let Some((current, _)) = self.current() {
                if current == token {
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

    fn current_is(&self, token: Token) -> bool {
        assert!(!token.is_value_type(), "cant test against value types");

        if self.is_at_end() {
            return false;
        }

        if let Some((curr, _)) = self.current() {
            return *curr == token;
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
        assert!(!token.is_value_type(), "cant consume value types");

        if let Some((current, _)) = self.current() {
            if token == *current {
                return self.advance();
            }
        }
        None
    }

    fn consume_identifier(&mut self) -> Option<(String, usize)> {
        if let Some((Token::Identifier(name), index)) = self.current() {
            let name = name.clone();
            let index = index.clone();

            self.advance();

            return Some((name, index));
        }

        None
    }

    fn consume_is(&mut self, token: Token) -> bool {
        self.consume(token).is_some()
    }

    fn consume_isnt(&mut self, token: Token) -> bool {
        !self.consume_is(token)
    }

    fn inside_loop(&self) -> bool {
        !self.loop_depth.is_empty()
    }
}

pub fn print_stmt(stmt: &Stmt, indent_level: usize, prefix: Option<String>) {
    let indent = " ".repeat(indent_level * 4);
    let prefix = prefix.unwrap_or_default();

    match stmt {
        Stmt::Print(expr, _) => {
            println!("{indent}{prefix}Print");
            print_expr(expr, indent_level + 1, None);
        }
        Stmt::Expr(expr) => {
            println!("{indent}{prefix}Expr");
            print_expr(expr, indent_level + 1, None);
        }
        Stmt::Var(name, expr, _) => {
            println!("{indent}{prefix}Set Var {name}");

            if let Some(expr) = expr {
                print_expr(expr, indent_level + 1, None);
            }
        }
        Stmt::Block(stmts) => {
            println!("{indent}{prefix}Block:");

            for stmt in stmts.iter() {
                print_stmt(stmt, indent_level + 1, None);
            }
        }
        Stmt::If(expr, then_branch, else_branch, _) => {
            println!("{indent}{prefix}If:");

            print_expr(expr, indent_level + 1, Some(String::from("Cond: ")));
            print_stmt(then_branch, indent_level + 1, Some(String::from("Then: ")));

            if let Some(else_branch) = else_branch {
                print_stmt(else_branch, indent_level + 1, Some(String::from("Else: ")));
            }
        }
        Stmt::While(condition, body, after, _) => {
            println!("{indent}{prefix}While:");

            print_expr(condition, indent_level + 1, None);
            print_stmt(body, indent_level + 1, None);

            if let Some(after) = after {
                print_stmt(after, indent_level + 1, None);
            }
        }
        Stmt::Break(_) => println!("{indent}BREAK"),
        Stmt::Continue(_) => println!("{indent}CONTINUE"),
        Stmt::Func(name, params, body, _) => {
            println!(
                "{indent}{prefix}Func {name}({})",
                params
                    .iter()
                    .map(|(name, _)| name.as_str())
                    .collect::<Vec<&str>>()
                    .join(", ")
            );

            for stmt in body {
                print_stmt(stmt, indent_level + 1, None);
            }
        }
        Stmt::Return(expr, _) => {
            println!("{indent}{prefix}Return");

            if let Some(expr) = expr {
                print_expr(expr, indent_level + 1, None);
            }
        }
        Stmt::Class(name, superclass, methods, _) => {
            println!("{indent}{prefix}Class {name}");

            if let Some(superclass) = superclass {
                print_expr(
                    superclass,
                    indent_level + 1,
                    Some(String::from("Extends: ")),
                );
            }

            for m in methods {
                print_stmt(m, indent_level + 1, Some(String::from("Method: ")));
            }
        }
    };
}

pub fn print_expr(expr: &Expr, indent_level: usize, prefix: Option<String>) {
    let indent = " ".repeat(indent_level * 4);
    let prefix = prefix.unwrap_or_default();

    match expr {
        Expr::Binary(left, op, right, _) => {
            println!("{indent}{prefix}Binary (Operator: {:?})", op);
            print_expr(left, indent_level + 1, None);
            print_expr(right, indent_level + 1, None);
        }
        Expr::Grouping(inner_expr, _) => {
            println!("{indent}{prefix}Group:");
            print_expr(inner_expr, indent_level + 1, None);
        }
        Expr::Unary(op, expr, _) => {
            println!("{indent}{prefix}Unary: (Operator {:?})", op);
            print_expr(expr, indent_level + 1, None);
        }
        Expr::Literal(value, index) => match value {
            Value::String(s) => println!("{indent}{prefix}String = {s}"),
            Value::Number(n) => println!("{indent}{prefix}Number = {n}"),
            Value::Bool(b) => {
                println!("{indent}{prefix}Bool = {b}")
            }
            Value::Func(fun, _) => println!("{indent}{prefix}func {fun}"),
            Value::Class(name, _, methods) => {
                println!("{indent}{prefix}class {name}");

                for (name, fun) in methods.borrow().iter() {
                    print_expr(
                        &Expr::Literal(fun.clone(), index.clone()),
                        indent_level + 1,
                        Some(format!("Method {name}: ")),
                    );
                }
            }
            Value::Instance(id, class, props) => match class.as_ref() {
                Value::Class(name, _, _) => {
                    println!("{indent}{prefix}instance #{id} of class {name}");

                    for (name, value) in props.borrow().iter() {
                        print_expr(
                            &Expr::Literal(value.clone(), index.clone()),
                            indent_level + 1,
                            Some(format!("Property '{name}'")),
                        );
                    }
                }
                _ => unreachable!(),
            },
            Value::Nil => println!("{indent}{prefix}Nil"),
        },
        Expr::Variable(name, _) => println!("{indent}{prefix}Var {name}"),
        Expr::Assign(name, expr, _) => {
            println!("{indent}{prefix}assign var {name} = {:?}", expr)
        }
        Expr::Logical(lhs, op, rhs, _) => {
            println!("{indent}{prefix}Logical (Operator {:?})", op);
            print_expr(lhs, indent_level + 1, None);
            print_expr(rhs, indent_level + 1, None);
        }
        Expr::Call(callee, args, _) => {
            println!("{indent}{prefix}Call Func");
            print_expr(callee, indent_level + 1, None);

            for arg in args {
                print_expr(arg, indent_level + 2, None);
            }
        }
        Expr::Closure(params, body, _) => {
            println!(
                "{indent}{prefix}Closure fun({})",
                params
                    .iter()
                    .map(|(name, _)| name.as_str())
                    .collect::<Vec<&str>>()
                    .join(", ")
            );

            for stmt in body {
                print_stmt(stmt, indent_level + 1, None);
            }
        }
        Expr::Ternary(condition, then_expr, else_expr, _) => {
            println!("{indent}{prefix}Ternary:");

            print_expr(&condition, indent_level + 1, Some(String::from("Cond: ")));
            print_expr(&then_expr, indent_level + 1, Some(String::from("Then: ")));
            print_expr(&else_expr, indent_level + 1, Some(String::from("Else: ")));
        }
        Expr::ReadProperty(lhs, name, _) => {
            println!("{indent}{prefix}Read Property: .{name}");
            print_expr(lhs, indent_level + 1, None);
        }
        Expr::WriteProperty(lhs, name, rhs, _) => {
            println!("{indent}{prefix}Write Property .{name}");
            print_expr(lhs, indent_level + 1, Some(String::from("Set: ")));
            print_expr(rhs, indent_level + 1, Some(String::from("To: ")));
        }
        Expr::This(_) => println!("{indent}{prefix}This"),
        Expr::Super(method, _) => println!("{indent}{prefix}Super Method {method}"),
    }
}

pub fn print_parser_error(source: &String, err: &ParserError) {
    let (message, index) = match err {
        ParserError::UnexpectedToken(token, index) => {
            (format!("unexpected token '{:?}'", token), index)
        }
        ParserError::CouldntFindRParen(index) => {
            ("could not find ')' after expression.".to_string(), index)
        }
        ParserError::ExpectedExpression(index) => ("expected expression".to_string(), index),
        ParserError::ExpectedSemicolonAfterStmt(index) => {
            ("expected ';' after statement".to_string(), index)
        }
        ParserError::ExpectedSemicolonAfterExpr(index) => {
            ("expected ';' after expression".to_string(), index)
        }
        ParserError::InvalidAssignmentTarget(index) => {
            ("invalid assignment target".to_string(), index)
        }
        ParserError::ExpectedRBraceAfterBlock(index) => {
            ("expected '}' after block".to_string(), index)
        }
        ParserError::ExpectedLParenAfter(what, index) => {
            (format!("expected '(' after '{what}'"), index)
        }
        ParserError::ExpectedRParenAfter(what, index) => {
            (format!("expected ')' after '{what}'"), index)
        }
        ParserError::ExpectedSemicolonAfterLoopCondition(index) => {
            ("expected ';' after loop condition".to_string(), index)
        }
        ParserError::MaximumArgsExceeded(index) => {
            ("can't have more than 255 arguments".to_string(), index)
        }
        ParserError::ExpectedLBraceBeforeBody(what, index) => {
            (format!("expected '{{' before {what} body"), index)
        }
        ParserError::ExpectedName(what, index) => (format!("expected {what} name"), index),
        ParserError::ExpectedMemberNameAfterDot(index) => {
            ("expected member name after '.'".to_string(), index)
        }
        ParserError::ExpectedDotAfterSuper(index) => {
            ("expected '.' after 'super'".to_string(), index)
        }
    };

    print_error_at(source, index.clone(), message.as_str());
}
