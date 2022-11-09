/*
 * A constant pool is an array of values. A LOAD instruction to load
 * a constant looks up the value by index in that array.
 */
use crate::function::*;
use std::fmt;
use std::ops;

#[derive(PartialEq, PartialOrd)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
    Str(String),
    Func(Function),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Boolean(b) => Value::Boolean(*b),
            Value::Number(n) => Value::Number(*n),
            Value::Nil => Value::Nil,
            Value::Str(s) => Value::Str(s.clone()),
            Value::Func(f) => Value::Func(f.clone()),
        }
    }
}

impl Value {
    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_))
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Boolean(false) | Value::Nil)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Number(val) => write!(f, "{}", val),
            Self::Boolean(val) => write!(f, "{}", val),
            Self::Str(s) => write!(f, "{}", s),
            Self::Nil => write!(f, "nil"),
            Self::Func(func) => write!(f, "{func}"),
        }
    }
}

impl ops::Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            _ => panic!("Invalid operation"),
        }
    }
}

impl ops::Sub for Value {
    type Output = Value;
    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => panic!("Invalid operation"),
        }
    }
}

impl ops::Mul for Value {
    type Output = Value;
    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => panic!("Invalid operation"),
        }
    }
}

impl ops::Div for Value {
    type Output = Value;
    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => panic!("Invalid operation"),
        }
    }
}

impl ops::Neg for Value {
    type Output = Value;
    fn neg(self) -> Value {
        match self {
            Value::Number(a) => Value::Number(-a),
            _ => panic!("Invalid operation"),
        }
    }
}

#[derive(Clone, Default)]
pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    // pub fn free(&mut self) {
    //     self.values = Vec::new();
    // }

    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn print(&self, idx: u8) {
        print!("{}", self.values[idx as usize]);
    }

    pub fn read_value(&self, index: usize) -> &Value {
        &self.values[index]
    }
}
