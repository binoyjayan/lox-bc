use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::chunk::*;
use crate::function::*;
use crate::upvalue::*;
use crate::value::*;

/*
 * Aside from global variables, a function have no way to reference a variable
 * declared outside of its own body. To fix this, the entired lexical scope of
 * all surrounding functions need to be included when resolving a variable.
 *
 * The problem is harder in a byte-code based VM as it stores locals on stack.
 * A stack is used for local variables since locals have stack semantics, meaning
 * that the variables are discarded in the reverse order that they are created.
 * But, with closures, that is not entirely so.
 *
 * A function can create a closure by defining it as a function inside of it.
 * Refer the 'examples' directory for a closure created with make_closure().
 * It returns returns a reference to a function defined inside of it.
 * Since the closure escapes while holding on to the local variable, locals
 * surrounding the closure must outlive the function call where it was created.
 *
 * This problem can be solved by dynamically allocating memory for all local
 * variables. But this is not very efficient. Using a stack is a better approach.
 * Most local variables are 'not' captured by closures and have stack semantics.
 *
 * Since, some locals have very different lifetimes, there are two implementation
 * strategy in play here. Locals that aren't used in closures are kept in stack
 * as usual. When a local is captured by a closure, another solution is used
 * that lifts them onto the heap where they can live as long as they are needed.
 *
 * Refer to 'Closure conversions' and 'Lambda lifting' for details.
 *
 * The existing Function object represents the 'raw' compile-time state of a
 * function declaration. At runtime, when a function is executed, the function
 * object is wrapped with a new object of type "Closure". This has reference to
 * the underlying bare function along with runtime state for the variables that
 * the function 'closes' over. To make things simple, every function is wrapped
 * with a Closure object even if the function does not close over an capture
 * the surrounding variables.
 *
 */

#[derive(Debug)]
pub struct Closure {
    function: Rc<Function>,
    // captured variables go here
    upvalues: RefCell<Vec<Rc<Upvalue>>>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.function)
    }
}

impl Closure {
    pub fn new(function: Rc<Function>) -> Self {
        Self {
            function: Rc::clone(&function),
            upvalues: RefCell::new(Vec::new()),
        }
    }
    pub fn get_arity(&self) -> usize {
        self.function.get_arity()
    }

    pub fn get_chunk(&self) -> Rc<Chunk> {
        self.function.get_chunk()
    }

    pub fn stack_name(&self) -> &str {
        self.function.stack_name()
    }

    pub fn get_upvalue(&self, offset: usize) -> Rc<RefCell<Value>> {
        Rc::clone(&self.upvalues.borrow()[offset]).get_value()
    }

    pub fn push_upvalue(&self, value: &Rc<RefCell<Value>>) {
        self.upvalues
            .borrow_mut()
            .push(Rc::new(Upvalue::new(value)));
    }

    pub fn set_upvalue(&self, offset: usize, value: &Rc<RefCell<Value>>) {
        self.upvalues.borrow_mut()[offset].set_value(value)
    }
}
