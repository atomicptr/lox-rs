use std::collections::HashMap;

use crate::{
    errormsg::print_error_at,
    interpreter::Interpreter,
    parser::{Expr, Stmt},
};

pub fn resolve(interpreter: &mut Interpreter, stmts: &Vec<Stmt>) -> Result<(), ResolverError> {
    let mut resolver = Resolver {
        interpreter,
        scopes: Vec::new(),
    };

    resolver.resolve(stmts)?;

    Ok(())
}

#[derive(Debug)]
pub enum ResolverError {
    CantReadLocalVarInItsOwnInitializer(usize),
    VariableAlreadyDeclared(String, usize),
    UnusedVar(String, usize),
}

#[derive(Debug)]
struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, VarState>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum VarState {
    Declared(usize),
    Defined(usize),
    Used,
}

impl Resolver<'_> {
    pub fn resolve(&mut self, stmts: &Vec<Stmt>) -> Result<(), ResolverError> {
        for stmt in stmts.iter() {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolverError> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.report_unused_vars()?;
                self.end_scope();

                Ok(())
            }
            Stmt::Var(name, expr, _) => {
                self.declare(name, stmt.stmt_index())?;

                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }

                self.define(name, stmt.stmt_index());

                Ok(())
            }
            Stmt::Func(name, params, body, _) => {
                self.declare(name, stmt.stmt_index())?;
                self.define(name, stmt.stmt_index());

                self.resolve_func(params, body)?;

                Ok(())
            }
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::If(condition, then_branch, else_branch, _) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;

                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }

                Ok(())
            }
            Stmt::Print(expr, _) => self.resolve_expr(expr),
            Stmt::Return(Some(expr), _) => self.resolve_expr(expr),
            Stmt::Return(None, _) => Ok(()),
            Stmt::While(condition, body, after, _) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;

                if let Some(after) = after {
                    self.resolve_stmt(after)?;
                }

                Ok(())
            }
            Stmt::Break(_) => Ok(()),
            Stmt::Continue(_) => Ok(()),
        }
    }

    fn resolve_func(
        &mut self,
        params: &Vec<(String, usize)>,
        body: &Vec<Stmt>,
    ) -> Result<(), ResolverError> {
        self.begin_scope();

        for (param, index) in params {
            self.declare(param, index.clone())?;
            self.define(param, index.clone());
        }

        self.resolve(body)?;
        self.report_unused_vars()?;

        self.end_scope();

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolverError> {
        match expr {
            Expr::Literal(_, _) => Ok(()),
            Expr::Variable(name, index) => {
                if let Some(scope) = self.scopes.first() {
                    if let Some(VarState::Declared(_)) = scope.get(name) {
                        return Err(ResolverError::CantReadLocalVarInItsOwnInitializer(
                            index.clone(),
                        ));
                    }
                }

                self.resolve_local(expr, name);

                Ok(())
            }
            Expr::Assignment(name, expr, _) => {
                self.resolve_expr(expr)?;
                self.resolve_local(expr, name);

                Ok(())
            }
            Expr::Binary(lhs, _, rhs, _) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)
            }
            Expr::Call(callee, args, _) => {
                self.resolve_expr(callee)?;

                for arg in args {
                    self.resolve_expr(arg)?;
                }

                Ok(())
            }
            Expr::Grouping(expr, _) => self.resolve_expr(expr),
            Expr::Logical(lhs, _, rhs, _) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)
            }
            Expr::Unary(_, expr, _) => self.resolve_expr(expr),
            Expr::Closure(params, body, _) => self.resolve_func(params, body),
            Expr::Ternary(cond, then_expr, else_expr, _) => {
                self.resolve_expr(cond)?;
                self.resolve_expr(then_expr)?;
                self.resolve_expr(else_expr)
            }
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &String) {
        for (i, scope) in self.scopes.iter_mut().rev().enumerate() {
            if let Some(_) = scope.get(name) {
                self.interpreter.resolve(expr, i);

                scope.insert(name.clone(), VarState::Used);
            }
        }
    }

    fn declare(&mut self, name: &String, index: usize) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.first_mut() {
            // already has the name
            if let Some(_) = scope.get(name) {
                return Err(ResolverError::VariableAlreadyDeclared(name.clone(), index));
            }

            scope.insert(name.clone(), VarState::Declared(index));
        }

        Ok(())
    }

    fn define(&mut self, name: &String, index: usize) {
        if let Some(scope) = self.scopes.first_mut() {
            scope.insert(name.clone(), VarState::Defined(index));
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes
            .pop()
            .expect("tried to pop scopes stack when it was empty");
    }

    fn report_unused_vars(&self) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.first() {
            for (name, state) in scope.iter() {
                if let VarState::Used = state {
                    continue;
                }

                let index = match state {
                    VarState::Declared(index) => index.clone(),
                    VarState::Defined(index) => index.clone(),
                    VarState::Used => unreachable!(),
                };

                return Err(ResolverError::UnusedVar(name.clone(), index));
            }
        }

        Ok(())
    }
}

pub fn print_resolver_error(source: &String, err: ResolverError) {
    let (message, index) = match err {
        ResolverError::CantReadLocalVarInItsOwnInitializer(index) => (
            "can't read local variable in its own initialzer".to_string(),
            index.clone(),
        ),
        ResolverError::VariableAlreadyDeclared(name, index) => (
            format!("there is already a variable named {name} declared in this scope"),
            index,
        ),
        ResolverError::UnusedVar(name, index) => (format!("unused variable '{name}'"), index),
    };

    print_error_at(source, index, message.as_str());
}
