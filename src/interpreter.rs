use std::collections::HashMap;

use crate::parser::{BinaryOp, Expr, Stmt, UnaryOp, Value};

#[derive(Debug, Default)]
pub struct Env {
    vars: HashMap<String, Value>,
}

#[derive(Debug, Default)]
pub struct Interpreter {
    env: Env,
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError(Value, usize),
    BinaryOpUnaryTypeError(Value, BinaryOp, usize),
    BinaryOpTyperError(Value, BinaryOp, Value, usize),
    UnaryOpTypeError(Value, UnaryOp, usize),
    DivByZero(usize),
    UnknownVariable(String, usize),
}

impl Interpreter {
    pub fn run(&mut self, stmts: &Vec<Stmt>) -> Result<Value, RuntimeError> {
        let mut value = Value::Nil;

        for stmt in stmts.iter() {
            value = self.evaluate_stmt(stmt)?;
        }

        Ok(value)
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Print(expr, _) => {
                let value = self.evaluate_expr(expr)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Stmt::Expr(expr, _) => self.evaluate_expr(expr),
            Stmt::Var(name, expr, _) => {
                let value = self.evaluate_expr(expr)?;
                self.env.vars.insert(name.clone(), value);
                Ok(Value::Nil)
            }
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Binary(lhs, op, rhs, index) => {
                let index = index.clone();
                let lhs_index = lhs.token_index();
                let lhs = self.evaluate_expr(lhs)?;

                let rhs_index = rhs.token_index();
                let rhs = self.evaluate_expr(rhs)?;

                match (&lhs, op, &rhs) {
                    // number operations
                    (Value::Number(a), op, Value::Number(b)) => match op {
                        BinaryOp::Eq => Ok(Value::Bool(f64_equals(a, b))),
                        BinaryOp::NotEq => Ok(Value::Bool(!f64_equals(a, b))),
                        BinaryOp::Lesser => Ok(Value::Bool(a < b)),
                        BinaryOp::LesserEq => Ok(Value::Bool(a < b || f64_equals(a, b))),
                        BinaryOp::Greater => Ok(Value::Bool(a > b)),
                        BinaryOp::GreaterEq => Ok(Value::Bool(a >= b || f64_equals(a, b))),
                        BinaryOp::Plus => Ok(Value::Number(a + b)),
                        BinaryOp::Minus => Ok(Value::Number(a - b)),
                        BinaryOp::Mul => Ok(Value::Number(a * b)),
                        BinaryOp::Div if f64_equals(b, &0.0) => {
                            Err(RuntimeError::DivByZero(rhs_index))
                        }
                        BinaryOp::Div => Ok(Value::Number(a / b)),
                    },

                    // string operations
                    (Value::String(a), op, Value::String(b)) => match op {
                        BinaryOp::Eq => Ok(Value::Bool(a == b)),
                        BinaryOp::NotEq => Ok(Value::Bool(a != b)),
                        BinaryOp::Lesser => Ok(Value::Bool(a < b)),
                        BinaryOp::LesserEq => Ok(Value::Bool(a <= b)),
                        BinaryOp::Greater => Ok(Value::Bool(a > b)),
                        BinaryOp::GreaterEq => Ok(Value::Bool(a >= b)),
                        BinaryOp::Plus => Ok(Value::String(format!("{a}{b}"))),
                        _ => Err(RuntimeError::BinaryOpTyperError(
                            lhs,
                            op.clone(),
                            rhs,
                            index,
                        )),
                    },

                    // string concat with num
                    (Value::String(a), BinaryOp::Plus, Value::Number(b)) => {
                        Ok(Value::String(format!("{a}{b}")))
                    }

                    // string mul
                    (Value::String(a), BinaryOp::Mul, Value::Number(b)) => {
                        Ok(Value::String(a.repeat(b.round().abs() as usize)))
                    }

                    // bool operations
                    (Value::Bool(a), BinaryOp::Eq, Value::Bool(b)) => Ok(Value::Bool(a == b)),
                    (Value::Bool(a), BinaryOp::NotEq, Value::Bool(b)) => Ok(Value::Bool(a != b)),

                    // nil operations
                    // ? == nil is always false, unless its nil == nil
                    // similarly ? != nil is always true
                    (Value::Nil, BinaryOp::Eq, Value::Nil) => Ok(Value::Bool(true)),
                    (Value::Nil, BinaryOp::NotEq, Value::Nil) => Ok(Value::Bool(false)),
                    // ? == nil, ? != nil
                    (_, BinaryOp::Eq, Value::Nil) => Ok(Value::Bool(false)),
                    (_, BinaryOp::NotEq, Value::Nil) => Ok(Value::Bool(true)),
                    // nil == ?, nil != ?
                    (Value::Nil, BinaryOp::Eq, _) => Ok(Value::Bool(false)),
                    (Value::Nil, BinaryOp::NotEq, _) => Ok(Value::Bool(true)),

                    // precise type errors
                    (Value::Number(_), op, val) => Err(RuntimeError::BinaryOpUnaryTypeError(
                        val.clone(),
                        op.clone(),
                        rhs_index,
                    )),
                    (val, op, Value::Number(_)) => Err(RuntimeError::BinaryOpUnaryTypeError(
                        val.clone(),
                        op.clone(),
                        lhs_index,
                    )),
                    (Value::String(_), BinaryOp::Eq, val) => Err(
                        RuntimeError::BinaryOpUnaryTypeError(val.clone(), BinaryOp::Eq, rhs_index),
                    ),
                    (val, BinaryOp::Eq, Value::String(_)) => Err(
                        RuntimeError::BinaryOpUnaryTypeError(val.clone(), BinaryOp::Eq, lhs_index),
                    ),
                    (Value::String(_), BinaryOp::NotEq, val) => {
                        Err(RuntimeError::BinaryOpUnaryTypeError(
                            val.clone(),
                            BinaryOp::NotEq,
                            rhs_index,
                        ))
                    }
                    (val, BinaryOp::NotEq, Value::String(_)) => {
                        Err(RuntimeError::BinaryOpUnaryTypeError(
                            val.clone(),
                            BinaryOp::NotEq,
                            lhs_index,
                        ))
                    }
                    (Value::String(_), BinaryOp::Plus, val) => {
                        Err(RuntimeError::BinaryOpUnaryTypeError(
                            val.clone(),
                            BinaryOp::Plus,
                            rhs_index,
                        ))
                    }
                    (val, BinaryOp::Plus, Value::String(_)) => {
                        Err(RuntimeError::BinaryOpUnaryTypeError(
                            val.clone(),
                            BinaryOp::Plus,
                            lhs_index,
                        ))
                    }

                    // if nothing else matches throw an binary op type error
                    _ => Err(RuntimeError::BinaryOpTyperError(
                        lhs,
                        op.clone(),
                        rhs,
                        index,
                    )),
                }
            }
            Expr::Grouping(expr, _) => self.evaluate_expr(expr),
            Expr::Literal(value, _) => Ok(value.clone()),
            Expr::Unary(op, rhs, _) => {
                let rhs_index = rhs.token_index();
                let rhs = self.evaluate_expr(rhs)?;

                match (op, &rhs) {
                    (UnaryOp::Neg, Value::Number(num)) => Ok(Value::Number(-num)),
                    (UnaryOp::Neg, _) => Err(RuntimeError::UnaryOpTypeError(
                        rhs.clone(),
                        op.clone(),
                        rhs_index,
                    )),
                    (UnaryOp::Not, value) => Ok(Value::Bool(!is_truthy(&value))),
                }
            }
            Expr::Variable(name, index) => match self.env.vars.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
            },
            Expr::Assignment(name, expr, index) => match self.env.vars.get(name) {
                Some(_) => {
                    let value = self.evaluate_expr(expr)?;

                    self.env.vars.insert(name.clone(), value.clone());

                    Ok(value)
                }
                None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
            },
        }
    }
}
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    }
}

fn f64_equals(a: &f64, b: &f64) -> bool {
    (a - b).abs() <= f64::EPSILON
}
