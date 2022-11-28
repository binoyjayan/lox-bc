use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::class::*;
use crate::value::*;

/*
 * Being a dynamic language lox freely adds fields to an object at runtime.
 * This is in contrast to statically typed languages that requires the compiler
 * to know how much memory to allocate for the classes and what offset in
 * memory they can be found. Not having this, lox has to perform a hash table
 * lookup to access a field.
 *
 */
#[derive(Debug)]
pub struct Instance {
    klass: Rc<Class>,
    fields: RefCell<HashMap<String, Value>>,
}

impl Instance {
    pub fn new(klass: Rc<Class>) -> Self {
        Self {
            klass,
            fields: RefCell::new(HashMap::new()),
        }
    }
    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.fields.borrow().get(name).cloned()
    }
    pub fn set_field(&self, name: String, value: Value) {
        self.fields.borrow_mut().insert(name, value);
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} instance", self.klass)
    }
}
