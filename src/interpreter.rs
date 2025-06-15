use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    builtins::{lox_assert, lox_panic, lox_time, lox_tostring},
    errormsg::print_error_at,
    parser::{BinaryOp, Expr, Fn, LogicalOp, Stmt, UnaryOp, Value},
};

#[derive(Debug, Default)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Rc<Value>>,
    is_builtin_env: bool,
}

impl Env {
    pub fn lookup(&self, name: &String, distance: Option<usize>) -> Option<Rc<Value>> {
        match distance {
            Some(0) => self.vars.get(name).map(|v| v.clone()),
            Some(n) => self.lookup(name, Some(n.checked_sub(1).unwrap())),
            None => {
                // None means we only apply this to the toplevel
                if self.is_toplevel() {
                    self.lookup(name, Some(0))
                } else if let Some(parent) = &self.parent {
                    parent.borrow().lookup(name, None)
                } else {
                    None
                }
            }
        }
    }

    pub fn define(&mut self, name: &String, value: Rc<Value>) {
        self.vars.insert(name.clone(), value);
    }

    pub fn assign(
        &mut self,
        name: &String,
        value: Rc<Value>,
        index: &usize,
        dist: Option<usize>,
    ) -> Result<(), RuntimeError> {
        if self.is_builtin_env {
            return Err(RuntimeError::CantModifyBuiltins(index.clone()));
        }

        match dist {
            Some(0) => {
                if let Some(_) = self.vars.get(name) {
                    self.vars.insert(name.clone(), value);
                    return Ok(());
                }

                Err(RuntimeError::UnknownVariable(name.clone(), index.clone()))
            }
            Some(n) => self.assign(name, value, index, Some(n - 1)),
            None => {
                // None means we only apply this to the toplevel
                if self.is_toplevel() {
                    self.assign(name, value, index, Some(0))
                } else {
                    let parent = self.parent.clone().unwrap();
                    parent.borrow_mut().assign(name, value, index, None)
                }
            }
        }
    }

    fn is_toplevel(&self) -> bool {
        // top level scope is the env that is right below the builtin env
        match &self.parent {
            Some(env) => env.borrow().is_builtin_env,
            None => false, // the only env without parent is builtin which by definition isnt
                           // the toplevel one
        }
    }

    pub fn create_child(parent_env: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            parent: Some(parent_env.clone()),
            ..Default::default()
        }))
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    ControlFlow(ControlFlow, usize),
    BinaryOpUnaryTypeError(Value, BinaryOp, usize),
    BinaryOpTyperError(Value, BinaryOp, Value, usize),
    UnaryOpTypeError(Value, UnaryOp, usize),
    DivByZero(usize),
    UnknownVariable(String, usize),
    NotCallable(usize),
    FnInvalidNumberOfArguments(usize, usize, usize),
    CantModifyBuiltins(usize),
    CantConvertValue(Value, String, usize),
    AssertionFailed(Value, usize),
    Panic(Value, usize),
    InvalidPropertyRead(Value, String, usize),
    InvalidPropertyWrite(Value, String, usize),
    UnknownProperty(Value, String, usize),
}

#[derive(Debug)]
pub enum ControlFlow {
    Break,
    Continue,
    Return(Option<Value>),
}

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
    locals: HashMap<usize, usize>,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut builtins = Env::default();
        builtins.is_builtin_env = true;

        // define language builtins
        builtins.define(
            &String::from("time"),
            Rc::new(Value::Func(Fn::NativeFunc(lox_time, 0), None)),
        );
        builtins.define(
            &String::from("tostring"),
            Rc::new(Value::Func(Fn::NativeFunc(lox_tostring, 1), None)),
        );
        builtins.define(
            &String::from("panic"),
            Rc::new(Value::Func(Fn::NativeFunc(lox_panic, 1), None)),
        );
        builtins.define(
            &String::from("assert"),
            Rc::new(Value::Func(Fn::NativeFunc(lox_assert, 2), None)),
        );

        Self {
            env: Env::create_child(Rc::new(RefCell::new(builtins))),
            locals: HashMap::new(),
        }
    }
}

impl Interpreter {
    pub fn run(&mut self, stmts: &Vec<Stmt>) -> Result<Value, RuntimeError> {
        let mut value = Value::Nil;

        for stmt in stmts.iter() {
            value = self.evaluate_stmt(stmt, self.env.clone())?;
        }

        // clear locals after using
        self.locals.clear();

        Ok(value)
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Print(expr, _) => {
                let value = self.evaluate_expr(expr, env)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Stmt::Expr(expr) => self.evaluate_expr(expr, env),
            Stmt::Var(name, expr, _) => {
                let value = if let Some(expr) = expr {
                    Some(self.evaluate_expr(expr, env.clone())?)
                } else {
                    None
                };

                env.borrow_mut()
                    .define(name, Rc::new(value.unwrap_or(Value::Nil)));

                Ok(Value::Nil)
            }
            Stmt::Block(stmts) => {
                self.evaluate_block(stmts, env)?;
                Ok(Value::Nil)
            }
            Stmt::If(condition, then_branch, else_branch, _) => {
                let value = self.evaluate_expr(condition, env.clone())?;

                if is_truthy(&value) {
                    self.evaluate_stmt(then_branch, env)?;
                } else if let Some(else_branch) = else_branch {
                    self.evaluate_stmt(else_branch, env)?;
                }

                Ok(Value::Nil)
            }
            Stmt::While(condition, body, after, _) => {
                loop {
                    let env = env.clone();
                    let value = self.evaluate_expr(condition, env.clone())?;

                    if !is_truthy(&value) {
                        break;
                    }

                    match self.evaluate_stmt(body, env.clone()) {
                        Ok(_) => {}
                        Err(RuntimeError::ControlFlow(ControlFlow::Break, _)) => break,
                        Err(RuntimeError::ControlFlow(ControlFlow::Continue, _)) => {
                            if let Some(after) = after {
                                let _ = self.evaluate_stmt(after, env.clone())?;
                            }

                            continue;
                        }
                        Err(err) => return Err(err),
                    }

                    if let Some(after) = after {
                        let _ = self.evaluate_stmt(after, env.clone())?;
                    }
                }
                Ok(Value::Nil)
            }
            Stmt::Break(index) => Err(RuntimeError::ControlFlow(ControlFlow::Break, index.clone())),
            Stmt::Continue(index) => Err(RuntimeError::ControlFlow(
                ControlFlow::Continue,
                index.clone(),
            )),
            Stmt::Func(name, params, body, _) => {
                env.borrow_mut().define(
                    name,
                    Rc::new(Value::Func(
                        Fn::LoxFunc(
                            Some(name.clone()),
                            params.iter().map(|(name, _)| name.clone()).collect(),
                            body.clone(),
                            env.clone(),
                        ),
                        None,
                    )),
                );
                Ok(Value::Nil)
            }
            Stmt::Return(expr, index) => {
                let value = if let Some(expr) = expr {
                    Some(self.evaluate_expr(expr, env)?)
                } else {
                    None
                };

                Err(RuntimeError::ControlFlow(
                    ControlFlow::Return(value),
                    index.clone(),
                ))
            }
            Stmt::Class(name, methods, _) => {
                let methods_map = Rc::new(RefCell::new(HashMap::new()));
                let class = Rc::new(Value::Class(name.clone(), methods_map.clone()));

                for m in methods {
                    if let Stmt::Func(name, params, body, _) = m {
                        methods_map.borrow_mut().insert(
                            name.clone(),
                            Value::Func(
                                Fn::LoxMethod(
                                    name.clone(),
                                    params.iter().map(|(name, _)| name.clone()).collect(),
                                    body.clone(),
                                    env.clone(),
                                ),
                                None,
                            ),
                        );
                    }
                }

                env.borrow_mut().define(name, class);
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
            Expr::Variable(name, index) => {
                match env.borrow().lookup(name, self.expr_distance(expr)) {
                    Some(value) => Ok(value.as_ref().clone()),
                    None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
                }
            }
            Expr::Assign(name, expr, index) => {
                let dist = self.expr_distance(expr);
                let var = env.borrow().lookup(name, dist.clone());
                match var {
                    Some(_) => {
                        let value = self.evaluate_expr(expr, env.clone())?;
                        env.borrow_mut()
                            .assign(name, Rc::new(value.clone()), index, dist)?;
                        Ok(value)
                    }
                    None => Err(RuntimeError::UnknownVariable(name.clone(), index.clone())),
                }
            }
            Expr::Logical(lhs, op, rhs, _) => {
                let lhs = self.evaluate_expr(lhs, env.clone())?;

                match op {
                    LogicalOp::Or => {
                        if is_truthy(&lhs) {
                            return Ok(Value::Bool(true));
                        }
                    }
                    LogicalOp::And => {
                        if !is_truthy(&lhs) {
                            return Ok(Value::Bool(false));
                        }
                    }
                }

                if is_truthy(&self.evaluate_expr(rhs, env)?) {
                    Ok(Value::Bool(true))
                } else {
                    Ok(Value::Bool(false))
                }
            }
            Expr::Call(callee, arguments, _) => {
                let callee_val = self.evaluate_expr(callee, env.clone())?;

                let mut args = vec![];

                for arg in arguments.iter() {
                    args.push(self.evaluate_expr(arg, env.clone())?);
                }

                match callee_val.clone() {
                    Value::Func(fun, context) => {
                        if fun.arity() != args.len() {
                            return Err(RuntimeError::FnInvalidNumberOfArguments(
                                fun.arity(),
                                args.len(),
                                callee.token_index(),
                            ));
                        }

                        self.call_func(&fun, &args, context, callee.token_index())
                    }
                    Value::Class(_, methods) => {
                        let instance = Rc::new(Value::Instance(
                            Box::new(callee_val),
                            Rc::new(RefCell::new(HashMap::new())),
                        ));

                        let init_arity =
                            if let Some(Value::Func(Fn::LoxMethod(_, params, _, _), _)) =
                                methods.borrow().get("init")
                            {
                                params.len()
                            } else {
                                0
                            };

                        if init_arity != args.len() {
                            return Err(RuntimeError::FnInvalidNumberOfArguments(
                                init_arity,
                                args.len(),
                                callee.token_index(),
                            ));
                        }

                        if let Some(Value::Func(Fn::LoxMethod(_, _, _, _), _)) =
                            methods.borrow().get("init")
                        {
                            if let Some(Value::Func(fun, _)) = methods.borrow().get("init") {
                                let _ = self.call_func(
                                    fun,
                                    &args,
                                    Some(instance.clone()),
                                    callee.token_index(),
                                );
                            }
                        }

                        Ok(instance.as_ref().clone())
                    }
                    _ => Err(RuntimeError::NotCallable(callee.token_index())),
                }
            }
            Expr::Closure(params, body, _) => Ok(Value::Func(
                Fn::LoxFunc(
                    None,
                    params.iter().map(|(name, _)| name.clone()).collect(),
                    body.clone(),
                    env,
                ),
                None,
            )),
            Expr::Ternary(condition, then_expr, else_expr, _) => {
                let cond = self.evaluate_expr(&condition, env.clone())?;

                if is_truthy(&cond) {
                    return Ok(self.evaluate_expr(then_expr, env)?);
                }

                Ok(self.evaluate_expr(else_expr, env)?)
            }
            Expr::ReadProperty(lhs, name, index) => {
                let lhs = self.evaluate_expr(lhs, env)?;

                match lhs.clone() {
                    Value::Instance(class, params) => {
                        if let Some(value) = params.borrow().get(name) {
                            return Ok(value.clone());
                        }

                        if let Value::Class(_, methods) = class.as_ref() {
                            if let Some(Value::Func(method, _)) = methods.borrow().get(name) {
                                return Ok(Value::Func(method.clone(), Some(Rc::new(lhs))));
                            }
                        } else {
                            unreachable!();
                        }

                        Err(RuntimeError::UnknownProperty(
                            lhs,
                            name.clone(),
                            index.clone(),
                        ))
                    }
                    Value::String(str) => match name.as_str() {
                        "length" => Ok(Value::Number(str.len() as f64)),
                        "reversed" => Ok(Value::String(str.chars().rev().collect())),
                        "uppercased" => Ok(Value::String(str.to_uppercase())),
                        "lowercased" => Ok(Value::String(str.to_lowercase())),
                        _ => Err(RuntimeError::InvalidPropertyRead(
                            lhs,
                            name.clone(),
                            index.clone(),
                        )),
                    },
                    _ => Err(RuntimeError::InvalidPropertyRead(
                        lhs,
                        name.clone(),
                        index.clone(),
                    )),
                }
            }
            Expr::WriteProperty(lhs, name, rhs, index) => {
                let lhs = self.evaluate_expr(lhs, env.clone())?;
                let rhs = self.evaluate_expr(rhs, env)?;

                match lhs {
                    Value::Instance(_class, params) => {
                        params.borrow_mut().insert(name.clone(), rhs);
                        Ok(Value::Nil)
                    }
                    _ => Err(RuntimeError::InvalidPropertyWrite(
                        lhs,
                        name.clone(),
                        index.clone(),
                    )),
                }
            }
            Expr::This(index) => {
                let this = env
                    .borrow()
                    .lookup(&String::from("this"), self.expr_distance(expr));

                if let Some(this) = this {
                    return Ok(this.as_ref().clone());
                }

                Err(RuntimeError::UnknownVariable(
                    String::from("this"),
                    index.clone(),
                ))
            }
        }
    }

    fn call_func(
        &mut self,
        fun: &Fn,
        args: &Vec<Value>,
        context: Option<Rc<Value>>,
        index: usize,
    ) -> Result<Value, RuntimeError> {
        let res = match fun {
            Fn::LoxFunc(_, params, body, env) => {
                let env = Env::create_child(env.clone());

                for (i, param) in params.iter().enumerate() {
                    env.borrow_mut()
                        .define(param, Rc::new(args.get(i).unwrap().clone()));
                }

                match self.evaluate_block_with_env(&body, env) {
                    Ok(_) => Ok(Value::Nil),
                    Err(err) => Err(err),
                }
            }
            Fn::LoxMethod(name, params, body, env) => {
                let env = Env::create_child(env.clone());

                for (i, param) in params.iter().enumerate() {
                    env.borrow_mut()
                        .define(param, Rc::new(args.get(i).unwrap().clone()));
                }

                let context = context.unwrap();

                env.borrow_mut()
                    .define(&String::from("this"), context.clone());

                match (name.as_str(), self.evaluate_block_with_env(&body, env)) {
                    // a method named 'init' is always the constructor, which always returns 'this'
                    ("init", Ok(_)) => Ok(context.as_ref().clone()),
                    // also every return should return this
                    ("init", Err(RuntimeError::ControlFlow(ControlFlow::Return(_), _))) => {
                        Ok(context.as_ref().clone())
                    }
                    (_, Ok(_)) => Ok(Value::Nil),
                    (_, Err(err)) => Err(err),
                }
            }
            Fn::NativeFunc(fun, _) => fun(index, args),
        };

        match res {
            // intercept return errors and use that as return value
            Err(RuntimeError::ControlFlow(ControlFlow::Return(value), _)) => {
                Ok(value.unwrap_or(Value::Nil))
            }
            res => res,
        }
    }

    fn expr_distance(&self, expr: &Expr) -> Option<usize> {
        self.locals
            .get(&expr.token_index())
            .map(|dist| dist.clone())
    }

    fn evaluate_block(
        &mut self,
        stmts: &Vec<Stmt>,
        env: Rc<RefCell<Env>>,
    ) -> Result<(), RuntimeError> {
        let new_env = Env::create_child(env);
        self.evaluate_block_with_env(stmts, new_env)
    }

    fn evaluate_block_with_env(
        &mut self,
        stmts: &Vec<Stmt>,
        env: Rc<RefCell<Env>>,
    ) -> Result<(), RuntimeError> {
        for stmt in stmts.iter() {
            let _ = self.evaluate_stmt(stmt, env.clone())?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        // TODO: replace the token index with an token ID as this is kinda in the REPL
        self.locals.insert(expr.token_index(), depth);
    }
}

pub fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    }
}

fn f64_equals(a: &f64, b: &f64) -> bool {
    (a - b).abs() <= f64::EPSILON
}

pub fn print_runtime_error(source: &String, err: RuntimeError) {
    let (message, index) = match err {
        RuntimeError::BinaryOpUnaryTypeError(value, op, index) => (
            format!("operator '{op}' can't be used with '{:?}'", value),
            index,
        ),
        RuntimeError::BinaryOpTyperError(lhs, op, rhs, index) => (
            format!(
                "operator '{op}' for '{:?}' and '{:?}' is not allowed",
                lhs, rhs
            ),
            index,
        ),
        RuntimeError::UnaryOpTypeError(value, op, index) => (
            format!("unary operator '{op}' can not be used with {:?}", value),
            index,
        ),
        RuntimeError::DivByZero(index) => ("division by zero".to_string(), index),
        RuntimeError::UnknownVariable(name, index) => (format!("unknown variable '{name}'"), index),
        RuntimeError::ControlFlow(cf, index) => {
            (format!("illegal control flow statement '{:?}'", cf), index)
        }
        RuntimeError::NotCallable(index) => ("expression is not callable".to_string(), index),
        RuntimeError::FnInvalidNumberOfArguments(expected, got, index) => (
            format!(
                "invalid number of arguments passed to function, expected {expected} parameters but received {got} parameters instead"
            ),
            index,
        ),
        RuntimeError::CantModifyBuiltins(index) => ("can't modify builtins".to_string(), index),
        RuntimeError::CantConvertValue(value, to, index) => {
            (format!("can't convert {:?} to {to}", value), index)
        }
        RuntimeError::AssertionFailed(message, index) => {
            (format!("assertion failed: {message}"), index)
        }
        RuntimeError::Panic(message, index) => (format!("PANIC: {message}"), index),
        RuntimeError::InvalidPropertyRead(callee, name, index) => (
            format!("invalid property access '.{name}' on '{callee}'"),
            index,
        ),
        RuntimeError::InvalidPropertyWrite(callee, name, index) => (
            format!("illegal write attempt '.{name}' on '{callee}'"),
            index,
        ),
        RuntimeError::UnknownProperty(callee, name, index) => {
            (format!("'{callee}' has no property '.{name}'"), index)
        }
    };

    print_error_at(source, index, message.as_str());
}
