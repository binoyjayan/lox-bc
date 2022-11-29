use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::closure::*;
use crate::value::*;

#[derive(Debug)]
pub struct Class {
    name: String,
    methods: RefCell<HashMap<String, Rc<Closure>>>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: RefCell::new(HashMap::new()),
        }
    }
    pub fn add_method(&self, name: &str, method: Value) {
        if let Value::Closure(closure) = method {
            self.methods.borrow_mut().insert(name.to_string(), closure);
        }
    }

    pub fn get_method(&self, name: &str) -> Option<Rc<Closure>> {
        self.methods.borrow().get(name).cloned()
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)
    }
}
