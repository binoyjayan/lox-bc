use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::*;
use crate::class::*;
use crate::closure::*;
use crate::compiler::*;
use crate::error::*;
use crate::function::*;
use crate::instance::*;
use crate::native::*;
use crate::opcode::*;
use crate::value::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct VM {
    stack: Vec<Rc<RefCell<Value>>>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
}

// An instance of type 'CallFrame' is used for each function call
#[derive(Debug)]
struct CallFrame {
    closure: Rc<Closure>,
    ip: RefCell<usize>,
    slots: usize,
}

impl CallFrame {
    fn inc(&self, amount: usize) {
        *self.ip.borrow_mut() += amount;
    }

    fn dec(&self, amount: usize) {
        *self.ip.borrow_mut() -= amount;
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        };
        let native: Rc<dyn NativeFunction> = Rc::new(NativeClock {});
        vm.define_native("clock", &native);
        vm
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut compiler = Compiler::new();
        let function = compiler.compile(source)?;

        let closure = Rc::new(Closure::new(Rc::new(function)));
        self.push(Value::Closure(Rc::clone(&closure)));
        self.call(closure, 0);

        // TODO: Is this required?
        // self.run(); let _ = self.pop();
        self.run()
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn ip(&self) -> usize {
        *self.current_frame().ip.borrow()
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn get_upvalue(&self, offset: usize) -> Rc<RefCell<Value>> {
        self.current_frame().closure.get_upvalue(offset)
    }

    fn set_upvalue(&self, offset: usize, value: &Rc<RefCell<Value>>) {
        self.current_frame().closure.set_upvalue(offset, value);
    }

    fn capture_upvalue(&self, offset: usize) -> Rc<RefCell<Value>> {
        Rc::clone(&self.stack[offset])
    }

    fn chunk(&self) -> Rc<Chunk> {
        self.current_frame().closure.get_chunk()
    }

    fn run(&mut self) -> Result<(), InterpretResult> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                self.print_stack();
                self.chunk().disassemble_instruction(self.ip());
            }
            let instruction: Opcode = self.read_opcode();
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant().clone();
                    self.push(constant);
                }
                Opcode::Return => {
                    let result = self.pop()?;
                    let prev_frame = self.frames.pop().unwrap();
                    if self.frames.is_empty() {
                        let _ = self.pop();
                        // Exit interpreter
                        return Ok(());
                    }
                    self.stack.truncate(prev_frame.slots);
                    self.stack.push(result);
                }
                Opcode::Add => {
                    self.binary_op(|a, b| a + b)?;
                }
                Opcode::Subtract => {
                    self.binary_op(|a, b| a - b)?;
                }
                Opcode::Multiply => {
                    self.binary_op(|a, b| a * b)?;
                }
                Opcode::Divide => {
                    self.binary_op(|a, b| a / b)?;
                }
                Opcode::Negate => {
                    if !self.peek(0)?.borrow().is_number() {
                        return Err(self.error_runtime("Operand must be a number"));
                    }
                    let value = self.pop()?.borrow().clone();
                    self.push(-&value);
                }
                Opcode::Nil => self.push(Value::Nil),
                Opcode::False => self.push(Value::Boolean(false)),
                Opcode::True => self.push(Value::Boolean(true)),
                Opcode::Not => {
                    let value = self.pop()?.borrow().clone();
                    self.push(Value::Boolean(value.is_falsey()))
                }
                Opcode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Boolean(a == b));
                }
                Opcode::Greater => {
                    self.binary_op(|a, b| Value::Boolean(a > b))?;
                }
                Opcode::Less => {
                    self.binary_op(|a, b| Value::Boolean(a < b))?;
                }
                Opcode::Print => {
                    let value = self.pop()?.borrow().clone();
                    println!("{}", value);
                }
                Opcode::Pop => {
                    let _ = self.pop();
                }
                Opcode::DefineGlobal => {
                    let name = self.read_constant().clone();
                    if let Value::Str(s) = name {
                        let value = self.pop()?;
                        self.globals.insert(s, value.borrow().clone());
                    } else {
                        panic!("Unable to read global variable");
                    }
                }
                Opcode::GetGlobal => {
                    if let Value::Str(s) = self.read_constant().clone() {
                        if let Some(v) = self.globals.get(&s) {
                            let u = v.clone();
                            self.push(u);
                        } else {
                            return Err(self.error_runtime(format!("Undefined variable '{}'.", s)));
                        }
                    } else {
                        panic!("Unable to read constant from table.");
                    }
                }
                Opcode::SetGlobal => {
                    if let Value::Str(s) = self.read_constant().clone() {
                        let value = self.peek(0)?.borrow().clone();
                        if let Entry::Occupied(mut o) = self.globals.entry(s.clone()) {
                            *o.get_mut() = value;
                        } else {
                            return Err(self.error_runtime(format!("Undefined variable '{}'", s)));
                        }
                    } else {
                        panic!("Unable to read constant from table.");
                    }
                }
                Opcode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    // Push the local variable on stack so the latest instruction
                    // can operate on the operands on the top of the stack
                    let slot_offset = self.current_frame().slots;
                    self.stack.push(self.stack[slot + slot_offset].clone());
                }
                Opcode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    let slot_offset = self.current_frame().slots;
                    self.stack[slot + slot_offset] = self.peek(0)?.clone();
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0)?.borrow().is_falsey() {
                        self.current_frame().inc(offset);
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_short();
                    self.current_frame().inc(offset);
                }
                Opcode::Loop => {
                    let offset = self.read_short();
                    self.current_frame().dec(offset);
                }
                Opcode::Call => {
                    let arg_count = self.read_byte() as usize;
                    // arg_count helps 'call_value()' to find the function on the stack
                    if !self.call_value(arg_count) {
                        return Err(InterpretResult::RuntimeError);
                        // return Err(self.error_runtime("Failed to call function"));
                    }
                }
                Opcode::Closure => self.closure_op(),
                // operand to the instruction is the index into the current function's
                // upvalue array. So, lookup the corresponding upvalue using the index
                Opcode::GetUpvalue => {
                    let slot = self.read_byte() as usize;
                    self.stack.push(Rc::clone(&self.get_upvalue(slot)))
                }
                Opcode::SetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek(0)?;
                    self.set_upvalue(slot, value);
                }
                Opcode::CloseUpvalue => {
                    let _ = self.pop();
                }
                Opcode::Class => {
                    let constant = self.read_constant().clone();
                    let class_string = if let Value::Str(s) = constant {
                        s
                    } else {
                        panic!("Unable to read constant from table.");
                    };
                    self.push(Value::Class(Rc::new(Class::new(class_string))));
                }
                Opcode::GetProperty => {
                    self.get_property_op()?;
                }
                Opcode::SetProperty => {
                    self.set_property_op()?;
                }
                Opcode::Method => {
                    let constant = self.read_constant();
                    let method_name = if let Value::Str(s) = constant {
                        s
                    } else {
                        panic!("Failed to get method name from table");
                    };
                    self.define_method(&method_name)?;
                }
                Opcode::Invoke => {
                    self.invoke_instruction()?;
                }
                Opcode::Inherit => {
                    self.inheritance()?;
                }
                Opcode::GetSuper => {
                    self.get_super_op()?;
                }
                Opcode::SuperInvoke => {
                    self.invoke_super()?;
                }
            }
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(Rc::new(RefCell::new(value)))
    }

    fn pop(&mut self) -> Result<Rc<RefCell<Value>>, InterpretResult> {
        match self.stack.pop() {
            Some(value) => Ok(value),
            None => {
                // Err(self.error_runtime("Stack underflow"))
                panic!("Stack underflow");
            }
        }
    }

    fn peek(&self, distance: usize) -> Result<&Rc<RefCell<Value>>, InterpretResult> {
        if distance >= self.stack.len() {
            panic!("Stack underflow");
        } else {
            Ok(&self.stack[self.stack.len() - distance - 1])
        }
    }

    /*
     * Function to get fields or methods. Fields and methods are accessed using
     * the same "dot property syntax. The compiler parses the right expressions
     * and emits OP_GET_PROPERTY instructions for them. When a property access
     * instruction executes, the instance is on top of the stack. The
     * instruction’s job is to find a field or method with the given name and
     * replace the top of the stack with the accessed property. Insert the code
     * to look up a field on the receiver instance. Fields take priority over
     * and shadow methods, so look for a field first. If the instance does not
     * have a field with the given property name, then the name may refer to a
     * method. Use the instance’s class and pass it to a new bind_method()
     * helper. If that function finds a method, it places the method on the
     * stack and returns true. Otherwise, it returns false to indicate a method
     * with that name couldn’t be found. Since the name also wasn’t a field,
     * that means there is a runtime error, which aborts the interpreter.
     */
    fn get_property_op(&mut self) -> Result<(), InterpretResult> {
        let instance = if let Value::Instance(inst) = self.peek(0)?.borrow().clone() {
            Some(inst)
        } else {
            None
        };
        if instance.is_none() {
            return Err(self.error_runtime("Only instances have properties."));
        }
        let instance = instance.unwrap();
        let constant = self.read_constant();
        let field_name = if let Value::Str(s) = constant {
            s
        } else {
            panic!("Failed to get field name from table");
        };
        if let Some(value) = instance.get_field(&field_name) {
            let _ = self.pop();
            self.push(value);
        } else if !self.bind_method(instance.get_class(), &field_name)? {
            return Err(InterpretResult::RuntimeError);
        }
        Ok(())
    }

    fn set_property_op(&mut self) -> Result<(), InterpretResult> {
        let instance = if let Value::Instance(inst) = self.peek(1)?.borrow().clone() {
            Some(inst)
        } else {
            None
        };
        if instance.is_none() {
            return Err(self.error_runtime("Only instances have fields."));
        }
        let instance = instance.unwrap();
        let constant = self.read_constant();
        let field_name = if let Value::Str(s) = constant {
            s
        } else {
            panic!("Failed to get class name from table");
        };

        let value = self.pop()?;
        instance.set_field(field_name, value.borrow().clone());
        // discard instance
        let _ = self.pop();
        self.stack.push(value);

        Ok(())
    }

    /*
     * The method closure is on top of the stack, above the class it will be
     * bound to. Read those two stack slots and store the closure in the
     * class’s method table. Then pop the closure since it is done with.
     */
    fn define_method(&mut self, name: &str) -> Result<(), InterpretResult> {
        let method = self.peek(0)?.borrow().clone();
        let klass = if let Value::Class(c) = self.peek(1)?.borrow().clone() {
            c
        } else {
            panic!("Failed to get class name from stack");
        };
        if name == "init" {
            if let Value::Closure(closure) = method {
                klass.set_init_method(closure);
            } else {
                panic!("method should have been a closure.");
            }
        } else {
            klass.add_method(name, method);
        }
        let _ = self.pop();
        Ok(())
    }

    /*
     * First we look for a method with the given name in the class’s method
     * table. If it is not found, report a runtime error and bail out.
     * Otherwise, take the method and wrap it in a new BoundMethod object.
     * Grab the receiver from its home on top of the stack. Finally, pop
     * the instance and replace the top of the stack with the bound method.
     */
    fn bind_method(&mut self, klass: Rc<Class>, name: &str) -> Result<bool, InterpretResult> {
        if let Some(method) = klass.get_method(name) {
            let value = self.peek(0)?.borrow().clone();
            let bound = Rc::new(BoundMethod::new(&value, &method));
            let _ = self.pop();
            self.push(Value::BoundMethod(bound));
            Ok(true)
        } else {
            Err(self.error_runtime(format!("Undefined property '{}'.", name)))
        }
    }

    /*
     * Look up the method name from the first operand and then read the
     * argument count operand. Then hand off to invoke() to do the heavy
     * lifting. That function returns true if the invocation succeeds.
     * As usual, a false return means a runtime error occurred. We check for
     * that here and abort the interpreter if disaster has struck.
     */
    fn invoke_instruction(&mut self) -> Result<(), InterpretResult> {
        let constant = self.read_constant();
        let method_name = if let Value::Str(s) = constant {
            s
        } else {
            panic!("Failed to get method name from table");
        };
        let arg_count = self.read_byte() as usize;
        if !self.invoke(&method_name, arg_count)? {
            return Err(InterpretResult::RuntimeError);
        }
        Ok(())
    }

    fn invoke_super(&mut self) -> Result<(), InterpretResult> {
        let method_name = self.read_string();
        let arg_count = self.read_byte() as usize;
        let superclass = self.pop()?.borrow().clone();
        let superclass = if let Value::Class(klass) = superclass {
            klass
        } else {
            return Err(InterpretResult::RuntimeError);
        };

        if !self.invoke_from_class(&superclass, &method_name, arg_count) {
            return Err(InterpretResult::RuntimeError);
        }
        Ok(())
    }

    /*
     * First grab the receiver off the stack. The arguments passed to the
     * method are above it on the stack, so peek that many slots down. Then,
     * it’s a simple matter to cast the object to an instance and invoke
     * the method on it. That does assume the object is an instance. As with
     * OP_GET_PROPERTY instructions, also handle the case where a user
     * incorrectly tries to call a method on a value of the wrong type.
     *
     * Before looking up a method on the instance’s class, look for a field
     * with the same name. If we find a field, then store it on the stack
     * in place of the receiver, under the argument list. This is how
     * OP_GET_PROPERTY behaves since the latter instruction executes before a
     * subsequent parenthesized list of arguments has been evaluated. Then try
     * to call that field’s value like the callable that it hopefully is. The
     * call_value() helper will check the value’s type and call it as
     * appropriate or report a runtime error if the field’s value isn’t a
     * callable type like a closure.
     */
    fn invoke(&mut self, name: &str, arg_count: usize) -> Result<bool, InterpretResult> {
        let receiver = self.peek(arg_count)?.borrow().clone();
        if let Value::Instance(instance) = receiver {
            if let Some(value) = instance.get_field(name) {
                let stack_top = self.stack.len();
                self.stack[stack_top - arg_count - 1] = Rc::new(RefCell::new(value));
                Ok(self.call_value(arg_count))
            } else {
                Ok(self.invoke_from_class(&instance.get_class(), name, arg_count))
            }
        } else {
            let _ = self.error_runtime("Only instances have methods.");
            Ok(false)
        }
    }

    /*
     * This function combines the logic of how the VM implements OP_GET_PROPERTY
     * and OP_CALL instructions, in that order. First look up the method by
     * name in the class’s method table. If we don’t find one, we report that
     * runtime error and exit. Otherwise, take the method’s closure and push a
     * call to it onto the CallFrame stack. There is no need to allocate and
     * initialize an BoundMethod object. In fact, there is not need to juggle
     * anything on the stack. The receiver and method arguments are already
     * where they need to be.
     *
     * This is a key reason why we use stack slot zero to store the receiver.
     * It’s how the caller already organizes the stack for a method call. An
     * efficient calling convention is an important part of a bytecode VM’s
     * performance story.
     */
    fn invoke_from_class(&mut self, klass: &Rc<Class>, name: &str, arg_count: usize) -> bool {
        if let Some(closure) = klass.get_method(name) {
            self.call(closure, arg_count)
        } else {
            let _ = self.error_runtime(format!("Undefined property '{}'", name));
            false
        }
    }

    /*
     * Fill the upvalue array over in the interpreter when it creates
     * a closure. This is where all of the operands after OP_CLOSURE is walked
     * through to see what kind of upvalue each slot captures. Iterate over
     * each upvalue the closure expects. For each one, read a pair of operand
     * bytes. If the upvalue closes over a local variable in the enclosing
     * function, let capture_upvalue() do the work. Otherwise, capture
     * an upvalue from the surrounding function. An OP_CLOSURE instruction is
     * emitted at the end of a function declaration. While executing that
     * declaration, the current function is the surrounding one. That means the
     * current function’s closure is stored in the CallFrame at the top of the
     * callstack. So, to grab an upvalue from the enclosing function, read it
     * from the frame local variable, which caches a reference to that CallFrame.
     */
    fn closure_op(&mut self) {
        let constant = self.read_constant();
        if let Value::Func(function) = constant {
            let upvalue_count = function.upvalue_count();
            let closure = Closure::new(function);
            for _ in 0..upvalue_count {
                let is_local = self.read_byte() != 0;
                let index = self.read_byte() as usize;
                let captured = if is_local {
                    let offset = self.current_frame().slots + index;
                    self.capture_upvalue(offset)
                } else {
                    self.get_upvalue(index)
                };
                closure.push_upvalue(&captured);
            }
            self.push(Value::Closure(Rc::new(closure)));
        } else {
            panic!("Failed to find closure")
        }
    }

    /* Initialize callframe on the stack. Store a pointer to the function
     * being called and point the frame's ip to the beginning of the
     * function's bytecode. Finally, setup the slots pointer to give the
     * frame its window into the stack. The '-1' is for the stack 'slot 0'
     * which the compiler sets aside for when methods are added later.
     */
    fn call(&mut self, closure: Rc<Closure>, arg_count: usize) -> bool {
        let arity = closure.get_arity();

        if arity != arg_count {
            let _ = self.error_runtime(format!(
                "Expected {} arguments but got {}.",
                arity, arg_count
            ));
            return false;
        }

        if self.frames.len() == 256 {
            let _ = self.error_runtime("Stack overflow.");
            return false;
        }
        self.frames.push(CallFrame {
            closure: Rc::clone(&closure),
            ip: RefCell::new(0),
            slots: self.stack.len() - arg_count - 1,
        });
        true
    }

    fn inheritance(&mut self) -> Result<(), InterpretResult> {
        let superclass = self.peek(1)?.borrow().clone();
        let superclass = if let Value::Class(klass) = superclass {
            klass
        } else {
            return Err(self.error_runtime("Superclass must be a class."));
        };
        let subclass = if let Value::Class(klass) = self.peek(0)?.borrow().clone() {
            klass
        } else {
            panic!("No subclass found of stack.");
        };

        subclass.copy_methods(&superclass);

        let _ = self.pop();
        Ok(())
    }

    /*
     * As with properties, read the method name from the constant table.
     * Then pass that to the bind_method() which looks up the method in the
     * given class’s method table and creates an BoundMethod object to bundle
     * the resulting closure to the current instance. The key difference is
     * which class is passed to bind_method(). With a normal property access,
     * use the Instances’s own class, which provides the dynamic dispatch.
     * For a super call, use the statically resolved superclass of the
     * containing class, that the compiler has made available on top of the
     * stack. Pop that superclass and pass it to the bind_method(), which
     * skips over any overriding methods in any of the subclasses between that
     * superclass and the instance’s own class. It also correctly includes any
     * methods inherited by the superclass from any of its superclasses.
     * The rest of the behavior is the same. Popping the superclass leaves the
     * instance at the top of the stack. When bind_method() succeeds, it pops
     * the instance and pushes the new bound method. Otherwise, it reports a
     * runtime error and returns false.
     */
    fn get_super_op(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        let popped_value = self.pop()?.borrow().clone();
        let superclass = if let Value::Class(klass) = popped_value {
            klass
        } else {
            panic!("no superclass");
        };
        self.bind_method(superclass, &name)?;
        Ok(())
    }

    /*
     * Users can call methods, classes and closures.
     * When methods are declared on classes and accessed on instances, they get
     * bound to methods on stack. Pull the raw closure back out of the BoundMethod
     * object and use the existing call() helper to begin an invocation of that
     * closure by pushing a CallFrame for it onto the call stack.
     *
     * The callee can be found at the offset 'arg_count' from the top
     * of the stack.
     */
    fn call_value(&mut self, arg_count: usize) -> bool {
        let callee = self.peek(arg_count).unwrap().borrow().clone();
        let result = match callee {
            Value::BoundMethod(method) => {
                let stack_top = self.stack.len();
                self.stack[stack_top - arg_count - 1] =
                    Rc::new(RefCell::new(method.get_receiver()));
                return self.call(method.get_closure(), arg_count);
            }
            Value::Class(klass) => {
                let stack_top = self.stack.len();
                let init = klass.get_init_method();
                let klass = Rc::new(RefCell::new(Value::Instance(Rc::new(Instance::new(klass)))));
                self.stack[stack_top - arg_count - 1] = klass;
                if let Some(initializer) = init {
                    self.call(initializer, arg_count)
                } else if arg_count != 0 {
                    let _ =
                        self.error_runtime(format!("Expected 0 arguments but got {}.", arg_count));
                    false
                } else {
                    true
                }
            }
            Value::Closure(c) => {
                return self.call(c, arg_count);
            }
            Value::Native(f) => {
                let stack_top = self.stack.len();
                let result = f.call(arg_count, &self.stack[stack_top - arg_count..stack_top]);
                self.stack.truncate(stack_top - arg_count - 1);
                self.push(result);
                true
            }
            _ => false,
        };
        if !result {
            let _ = self.error_runtime("Can only call functions and classes.");
        }
        result
    }

    fn read_byte(&mut self) -> u8 {
        let val = self.chunk().read_byte(self.ip());
        self.current_frame().inc(1);
        val
    }

    fn read_short(&mut self) -> usize {
        self.current_frame().inc(2);
        self.chunk().get_jump_offset(self.ip() - 2)
    }

    fn read_opcode(&mut self) -> Opcode {
        self.read_byte().into()
    }

    fn read_constant(&mut self) -> Value {
        let index = self.chunk().read_byte(self.ip()) as usize;
        self.current_frame().inc(1);
        self.chunk().get_constant(index)
    }

    // Helper to read string from constant table
    fn read_string(&mut self) -> String {
        let constant = self.read_constant();
        if let Value::Str(s) = constant {
            s
        } else {
            panic!("Unable to get field name from table");
        }
    }

    #[allow(dead_code)]
    fn print_stack(&self) {
        print!("          ");
        for slot in &self.stack {
            print!("[ ");
            print!("{}", slot.borrow());
            print!(" ]");
        }
        println!();
    }

    fn binary_op(&mut self, op: fn(a: &Value, b: &Value) -> Value) -> Result<(), InterpretResult> {
        if self.peek(0)?.borrow().is_string() && self.peek(1)?.borrow().is_string() {
            // pop b before a
            let b = self.pop()?;
            let a = self.pop()?;
            self.push(Value::Str(format!("{}{}", a.borrow(), b.borrow())));
            Ok(())
        } else if self.peek(0)?.borrow().is_number() && self.peek(1)?.borrow().is_number() {
            // pop b before a
            let b = self.pop()?;
            let a = self.pop()?;
            self.push(op(&a.borrow(), &b.borrow()));
            Ok(())
        } else {
            Err(self.error_runtime("Operands must be two numbers or two strings."))
        }
    }

    /*
     * Look into the chunk's debug lines array to find the executing instruction's
     * current line number in source code. 'ip' always points to the next instruction.
     * So, the current instr
     */
    fn error_runtime<T: Into<String>>(&mut self, err_str: T) -> InterpretResult {
        eprintln!("{}", err_str.into());
        for frame in self.frames.iter().rev() {
            let instruction = *frame.ip.borrow() - 1;
            let line = frame.closure.get_chunk().get_line(instruction);
            eprintln!("[line {}] in {}", line, frame.closure.stack_name());
        }
        self.reset_stack();
        InterpretResult::RuntimeError
    }

    fn define_native<T: Into<String>>(&mut self, name: T, function: &Rc<dyn NativeFunction>) {
        self.globals
            .insert(name.into(), Value::Native(Rc::clone(function)));
    }
}
