use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{BinaryOp, Expr, Stmt, UnaryOp, Value};

#[derive(Debug, Default)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Value>,
}

impl Env {
    pub fn get_var(&self, name: &String) -> Option<Value> {
        if let Some(value) = self.vars.get(name) {
            return Some(value.clone());
        }

        match &self.parent {
            Some(env) => env.borrow().get_var(name),
            None => None,
        }
    }

    pub fn define(&mut self, name: &String, value: Value) {
        self.vars.insert(name.clone(), value);
    }

    pub fn assign(
        &mut self,
        name: &String,
        value: Value,
        index: &usize,
    ) -> Result<(), RuntimeError> {
        // if the var exists in our scope
        if let Some(_) = self.vars.get(name) {
            self.vars.insert(name.clone(), value);
            return Ok(());
        }

        // if var exists in parent env, assign there
        if let Some(env) = &mut self.parent {
            return env.borrow_mut().assign(name, value, index);
        }

        Err(RuntimeError::UnknownVariable(name.clone(), index.clone()))
    }

    pub fn create_child(parent_env: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            parent: Some(parent_env.clone()),
            ..Default::default()
        }))
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
}

#[derive(Debug)]
pub enum RuntimeError {
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
            value = self.evaluate_stmt(stmt, self.env.clone())?;
        }

        Ok(value)
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.evaluate_expr(expr, env)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Stmt::Expr(expr) => self.evaluate_expr(expr, env),
            Stmt::Var(name, expr) => {
                let value = self.evaluate_expr(expr, env.clone())?;
                env.borrow_mut().define(name, value);
                Ok(Value::Nil)
            }
            Stmt::Block(stmts) => {
                self.evaluate_block(stmts, env)?;
                Ok(Value::Nil)
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let value = self.evaluate_expr(condition, env.clone())?;

                if is_truthy(&value) {
                    self.evaluate_stmt(then_branch, env)?;
                } else if let Some(else_branch) = else_branch {
                    self.evaluate_stmt(else_branch, env)?;
                }

                Ok(Value::Nil)
            }
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Binary(lhs, op, rhs, index) => {
                let index = index.clone();
                let lhs_index = lhs.token_index();
                let lhs = self.evaluate_expr(lhs, env.clone())?;

                let rhs_index = rhs.token_index();
                let rhs = self.evaluate_expr(rhs, env.clone())?;

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
            Expr::Grouping(expr, _) => self.evaluate_expr(expr, env),
            Expr::Literal(value, _) => Ok(value.clone()),
            Expr::Unary(op, rhs, _) => {
                let rhs_index = rhs.token_index();
                let rhs = self.evaluate_expr(rhs, env)?;

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
            Expr::Variable(name, index) => match env.borrow().get_var(name) {
                Some(value) => Ok(value.clone()),
                None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
            },
            Expr::Assignment(name, expr, index) => {
                let var = env.borrow().get_var(name);
                match var {
                    Some(_) => {
                        let value = self.evaluate_expr(expr, env.clone())?;
                        env.borrow_mut().assign(name, value.clone(), index)?;
                        Ok(value)
                    }
                    None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
                }
            }
        }
    }

    fn evaluate_block(
        &mut self,
        stmts: &Vec<Stmt>,
        env: Rc<RefCell<Env>>,
    ) -> Result<(), RuntimeError> {
        let new_env = Env::create_child(env);

        for stmt in stmts.iter() {
            let _ = self.evaluate_stmt(stmt, new_env.clone())?;
        }

        Ok(())
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
