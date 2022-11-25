use std::cell::RefCell;
use std::rc::Rc;

use crate::value::*;

#[derive(Debug)]
pub struct Upvalue {
    location: Rc<RefCell<Value>>,
}

impl Upvalue {
    pub fn new(value: &Rc<RefCell<Value>>) -> Self {
        Self {
            location: value.clone(),
        }
    }

    pub fn get_value(&self) -> Rc<RefCell<Value>> {
        Rc::clone(&self.location)
    }

    pub fn set_value(&self, value: &Rc<RefCell<Value>>) {
        *self.location.borrow_mut() = value.borrow().clone()
    }
}
