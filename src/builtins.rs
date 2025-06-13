use std::time::{SystemTime, UNIX_EPOCH};

use crate::parser::Value;

pub fn time() -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_secs_f64(),
    )
}
