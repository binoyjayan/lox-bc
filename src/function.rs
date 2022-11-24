use std::fmt;
use std::rc::Rc;

use crate::chunk::*;

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
