use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    interpreter::{RuntimeError, is_truthy},
    parser::Value,
};

pub fn time(_index: usize) -> Result<Value, RuntimeError> {
    Ok(Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_secs_f64(),
    ))
}

pub fn to_string(index: usize, value: Value) -> Result<Value, RuntimeError> {
    match value {
        Value::String(s) => Ok(Value::String(s)),
        Value::Number(n) => Ok(Value::String(format!("{n}"))),
        Value::Bool(b) => Ok(Value::String(format!("{b}"))),
        Value::Func(_) => Err(RuntimeError::CantConvertValue(
            value,
            "String".to_string(),
            index,
        )),
        Value::Nil => Ok(Value::String("nil".to_string())),
    }
}

pub fn lox_assert(index: usize, condition: Value, message: Value) -> Result<Value, RuntimeError> {
    if is_truthy(&condition) {
        return Ok(Value::Nil);
    }

    Err(RuntimeError::AssertionFailed(message, index))
}

pub fn lox_panic(index: usize, message: Value) -> Result<Value, RuntimeError> {
    Err(RuntimeError::Panic(message, index))
}
