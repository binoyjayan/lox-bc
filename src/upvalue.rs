use std::rc::Rc;

use crate::value::*;

#[derive(Debug)]
pub struct Upvalue {
    location: Rc<Value>,
}

impl Upvalue {
    pub fn new(value: &Rc<Value>) -> Self {
        Self {
            location: value.clone(),
        }
    }
    pub fn get_value(&self) -> Rc<Value> {
        Rc::clone(&self.location)
    }
}
