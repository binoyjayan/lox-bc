use std::fmt;
use std::rc::Rc;

use crate::chunk::*;
use crate::closure::*;
use crate::value::*;

#[derive(Debug, Default)]
pub struct Function {
    arity: usize,
    chunk: Rc<Chunk>,
    name: String,
    upvalue_count: usize,
}

impl PartialOrd for Function {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        panic!("Comparing Ord of two functions")
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", self.name)
        }
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            chunk: self.chunk.clone(),
            name: self.name.clone(),
            upvalue_count: self.upvalue_count,
        }
    }
}

impl Function {
    pub fn new<T: Into<String>>(
        arity: usize,
        chunk: &Rc<Chunk>,
        name: T,
        upvalue_count: usize,
    ) -> Self {
        Function {
            arity,
            chunk: Rc::clone(chunk),
            name: name.into(),
            upvalue_count,
        }
    }

    pub fn toplevel(chunk: &Rc<Chunk>) -> Self {
        Self::new(0, chunk, "", 0)
    }

    pub fn get_chunk(&self) -> Rc<Chunk> {
        Rc::clone(&self.chunk)
    }

    pub fn get_arity(&self) -> usize {
        self.arity
    }

    pub fn stack_name(&self) -> &str {
        if self.name.is_empty() {
            "<script>"
        } else {
            self.name.as_str()
        }
    }

    // #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn upvalue_count(&self) -> usize {
        self.upvalue_count
    }
}

/*
 * Method References look like this: instance.method(args)
 * However, it can also be two steps:
 * var closure = instance.method
 * closure(args)
 *
 * The obvious approach is to look up the method in the class’s method table
 * and return the Closure object associated with that name. When a method is
 * accessed, 'this' gets bound to the instance the method was accessed from.
 * When the user executes a method access, the closure for that method is
 * found and wrapped in a new 'BoundMethod' object that tracks the instance
 * that the method was accessed from. This bound object can be called later
 * like a function. When invoked, the VM will do some shenanigans to wire up
 * this to point to the receiver inside the method’s body.
 * It wraps the receiver and the method closure together. The receiver’s type
 * is Value even though methods can be called only on ObjInstances. Since the
 * VM doesn’t care what kind of receiver it has anyway, using 'Value' means
 * not having to keep converting the object back to a Value when it gets passed
 * to more general functions.
 */
#[derive(Debug)]
pub struct BoundMethod {
    receiver: Value,
    method: Rc<Closure>,
}

impl BoundMethod {
    pub fn new(receiver: &Value, method: &Rc<Closure>) -> Self {
        Self {
            receiver: receiver.clone(),
            method: Rc::clone(method),
        }
    }

    pub fn get_closure(&self) -> Rc<Closure> {
        Rc::clone(&self.method)
    }

    pub fn get_receiver(&self) -> Value {
        self.receiver.clone()
    }
}

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.method)
    }
}
