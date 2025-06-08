use crate::parser::{BinaryOp, Expr, UnaryOp, Value};

#[derive(Debug)]
pub enum InterpreterError {}

pub fn interpret(expr: &Expr) -> Result<Value, InterpreterError> {
    evaluate(expr)
}

fn evaluate(expr: &Expr) -> Result<Value, InterpreterError> {
    match expr {
        Expr::Binary(lhs, op, rhs) => {
            let lhs = evaluate(lhs)?;
            let rhs = evaluate(rhs)?;

            match (&lhs, op, &rhs) {
                // number operations
                (Value::Number(a), op, Value::Number(b)) => match op {
                    BinaryOp::Eq => Ok(Value::Bool(*a == *b)),
                    BinaryOp::NotEq => Ok(Value::Bool(*a != *b)),
                    BinaryOp::Lesser => Ok(Value::Bool(*a < *b)),
                    BinaryOp::LesserEq => Ok(Value::Bool(*a <= *b)),
                    BinaryOp::Greater => Ok(Value::Bool(*a > *b)),
                    BinaryOp::GreaterEq => Ok(Value::Bool(*a >= *b)),
                    BinaryOp::Plus => Ok(Value::Number(*a + *b)),
                    BinaryOp::Minus => Ok(Value::Number(*a - *b)),
                    BinaryOp::Mul => Ok(Value::Number(*a * *b)),
                    BinaryOp::Div => Ok(Value::Number(*a / *b)),
                },

                // string operations
                (Value::String(a), BinaryOp::Eq, Value::String(b)) => Ok(Value::Bool(a == b)),
                (Value::String(a), BinaryOp::NotEq, Value::String(b)) => Ok(Value::Bool(a != b)),
                // string concat
                (Value::String(a), BinaryOp::Plus, Value::String(b)) => {
                    Ok(Value::String(format!("{a}{b}")))
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

                // if nothing else matches throw an binary op type error
                _ => panic!(
                    "syntax error: Can't evaluate {:?} {:?} {:?}",
                    &lhs, op, &rhs
                ),
            }
        }
        Expr::Grouping(expr) => evaluate(expr),
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Unary(op, rhs) => {
            let right = evaluate(rhs)?;

            match (op, right) {
                (UnaryOp::Neg, Value::Number(num)) => Ok(Value::Number(-num)),
                (UnaryOp::Neg, _) => panic!("can only negate numbers"), // TODO: implement error
                (UnaryOp::Not, value) => Ok(Value::Bool(!is_truthy(&value))),
            }
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
