use std::collections::HashMap;
use crate::ast::{BinaryOperator, Expr, Literal, Statement};
use crate::error::{Result, VeldError};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Function {
        params: Vec<(String, String)>,
        body: Vec<Statement>,
        return_type: Option<String>,
    },
    Return(Box<Value>),
    Void,
}

impl Value {
    // Helper method to unwrap return values
    fn unwrap_return(self) -> Value {
        match self {
            Value::Return(val) => *val,
            val => val,
        }
    }
}

#[derive(Debug, Default)]
struct Scope {
    values: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    fn set(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}

pub struct Interpreter {
    scopes: Vec<Scope>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<Value> {
        let mut last_value = Value::Void;
        for stmt in statements {
            last_value = self.execute_statement(stmt)?;
        }
        Ok(last_value)
    }

    fn execute_statement(&mut self, statement: Statement) -> Result<Value> {
        match statement {
            Statement::FunctionDeclaration { name, params, return_type, body } => {
                let function = Value::Function {
                    params,
                    body,
                    return_type,
                };
                self.current_scope_mut().set(name, function);
                Ok(Value::Void)
            }
            Statement::VariableDeclaration { name, value, .. } => {
                let value = self.evaluate_expression(*value)?;
                let value = value.unwrap_return(); // Unwrap any return value
                self.current_scope_mut().set(name, value.clone());
                Ok(value)
            }
            Statement::ExprStatement(expr) => {
                let value = self.evaluate_expression(expr)?;
                Ok(value.unwrap_return())
            }
        }
    }

    fn evaluate_expression(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Literal(lit) => Ok(match lit {
                Literal::Integer(n) => Value::Integer(n),
                Literal::Float(n) => Value::Float(n),
                Literal::String(s) => Value::String(s),
                Literal::Boolean(b) => Value::Boolean(b),
            }),
            Expr::Identifier(name) => {
                self.get_variable(&name)
                    .ok_or_else(|| VeldError::RuntimeError(format!("Undefined variable '{}'", name)))
            }
            Expr::BinaryOp { left, operator, right } => {
                let left_val = self.evaluate_expression(*left)?.unwrap_return();
                let right_val = self.evaluate_expression(*right)?.unwrap_return();
                self.evaluate_binary_op(left_val, operator, right_val)
            }
            Expr::FunctionCall { name, arguments } => {
                self.call_function(name, arguments)
            }
        }
    }
    fn evaluate_binary_op(&mut self, left: Value, operator: BinaryOperator, right: Value) -> Result<Value> {
        match (left, operator, right) {
            (Value::Integer(a), BinaryOperator::Add, Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Integer(a), BinaryOperator::Subtract, Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Integer(a), BinaryOperator::Multiply, Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Integer(a), BinaryOperator::Divide, Value::Integer(b)) => {
                if b == 0 {
                    Err(VeldError::RuntimeError("Division by zero".to_string()))
                } else {
                    Ok(Value::Integer(a / b))
                }
            }
            // Add more operations (Float operations, etc.)
            _ => Err(VeldError::RuntimeError("Invalid operation".to_string())),
        }
    }

    fn call_function(&mut self, name: String, arguments: Vec<Expr>) -> Result<Value> {
        let function = self.get_variable(&name)
            .ok_or_else(|| VeldError::RuntimeError(format!("Undefined function '{}'", name)))?;

        match function {
            Value::Function { params, body, .. } => {
                // Create new scope for function
                self.push_scope();

                // Evaluate and bind arguments
                if arguments.len() != params.len() {
                    return Err(VeldError::RuntimeError(format!(
                        "Expected {} arguments but got {}",
                        params.len(),
                        arguments.len()
                    )));
                }

                // Evaluate arguments and bind them to parameters
                for (i, arg) in arguments.into_iter().enumerate() {
                    let value = self.evaluate_expression(arg)?;
                    let value = value.unwrap_return();
                    self.current_scope_mut().set(params[i].0.clone(), value);
                }

                // Execute function body
                let mut result = Value::Void;
                for stmt in body {
                    result = self.execute_statement(stmt)?;
                    if matches!(result, Value::Return(_)) {
                        break;
                    }
                }

                // Remove function scope
                self.pop_scope();

                Ok(result.unwrap_return())
            }
            _ => Err(VeldError::RuntimeError(format!("'{}' is not a function", name))),
        }
    }
    // Scope management
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}