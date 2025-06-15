use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    interpreter::{RuntimeError, is_truthy},
    parser::Value,
};

pub fn lox_time(_index: usize, _args: &Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_secs_f64(),
    ))
}

pub fn lox_tostring(index: usize, args: &Vec<Value>) -> Result<Value, RuntimeError> {
    let value = args.first().unwrap().clone();

    match value {
        Value::String(s) => Ok(Value::String(s)),
        Value::Number(n) => Ok(Value::String(format!("{n}"))),
        Value::Bool(b) => Ok(Value::String(format!("{b}"))),
        Value::Func(_) | Value::Class(_) | Value::Instance(_, _) => Err(
            RuntimeError::CantConvertValue(value, "String".to_string(), index),
        ),
        Value::Nil => Ok(Value::String("nil".to_string())),
    }
}

pub fn lox_assert(index: usize, args: &Vec<Value>) -> Result<Value, RuntimeError> {
    let condition = args.get(0).unwrap().clone();
    let message = args.get(1).unwrap().clone();

    if is_truthy(&condition) {
        return Ok(Value::Nil);
    }

    Err(RuntimeError::AssertionFailed(message, index))
}

pub fn lox_panic(index: usize, args: &Vec<Value>) -> Result<Value, RuntimeError> {
    let message = args.get(0).unwrap().clone();
    Err(RuntimeError::Panic(message, index))
}
