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
    initializer: RefCell<Option<Rc<Closure>>>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: RefCell::new(HashMap::new()),
            initializer: RefCell::new(None),
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

    // To enable optimized invocation for initialize
    pub fn get_init_method(&self) -> Option<Rc<Closure>> {
        if self.initializer.borrow().is_none() {
            None
        } else {
            Some(self.initializer.borrow().as_ref().unwrap().clone())
        }
    }
    pub fn set_init_method(&self, closure: Rc<Closure>) {
        self.initializer.replace(Some(closure));
    }

    // Copy methods from the class passed as argument to self
    pub fn copy_methods(&self, superclass: &Self) {
        for (k, v) in superclass.methods.borrow().iter() {
            self.methods.borrow_mut().insert(k.clone(), Rc::clone(v));
        }
        self.initializer.replace(superclass.get_init_method());
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)
    }
}
