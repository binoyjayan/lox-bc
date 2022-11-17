use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::*;
use crate::compiler::*;
use crate::error::*;
use crate::native::*;
use crate::value::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
}

// An instance of type 'CallFrame' is used for each function call
#[derive(Debug)]
struct CallFrame {
    function: usize, // index into vm.stack
    ip: RefCell<usize>,
    // slots: Vec<usize>,
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

        self.stack.push(Value::Func(Rc::new(function)));
        self.call(0);

        // TODO: Is this required (and self.run() before pop())?
        // let _ = self.pop();
        self.run()
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn ip(&self) -> usize {
        *self.frames.last().unwrap().ip.borrow()
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn chunk(&self) -> Rc<Chunk> {
        let position = self.current_frame().function;
        if let Value::Func(func) = &self.stack[position] {
            func.get_chunk()
        } else {
            panic!("No chunk in function")
        }
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
                    self.stack.push(constant);
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
                    if let Value::Number(value) = self.peek(0)? {
                        let _ = self.pop();
                        self.stack.push(Value::Number(-value));
                    } else {
                        return Err(self.error_runtime("Operand must be a number"));
                    }
                }
                Opcode::Nil => self.stack.push(Value::Nil),
                Opcode::False => self.stack.push(Value::Boolean(false)),
                Opcode::True => self.stack.push(Value::Boolean(true)),
                Opcode::Not => {
                    let value = self.pop()?;
                    self.stack.push(Value::Boolean(value.is_falsey()))
                }
                Opcode::Equal => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.stack.push(Value::Boolean(a == b));
                }
                Opcode::Greater => {
                    self.binary_op(|a, b| Value::Boolean(a > b))?;
                }
                Opcode::Less => {
                    self.binary_op(|a, b| Value::Boolean(a < b))?;
                }
                Opcode::Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                }
                Opcode::Pop => {
                    let _ = self.pop();
                }
                Opcode::DefineGlobal => {
                    let name = self.read_constant().clone();
                    if let Value::Str(s) = name {
                        let value = self.pop()?;
                        self.globals.insert(s, value);
                    } else {
                        panic!("Unable to read global variable");
                    }
                }
                Opcode::GetGlobal => {
                    if let Value::Str(s) = self.read_constant().clone() {
                        if let Some(v) = self.globals.get(&s) {
                            self.stack.push(v.clone());
                        } else {
                            return Err(self.error_runtime(format!("Undefined variable '{}'.", s)));
                        }
                    } else {
                        panic!("Unable to read constant from table.");
                    }
                }
                Opcode::SetGlobal => {
                    if let Value::Str(s) = self.read_constant().clone() {
                        let value = self.peek(0)?.clone();
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
                    if self.peek(0)?.is_falsey() {
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
            }
        }
    }

    fn pop(&mut self) -> Result<Value, InterpretResult> {
        match self.stack.pop() {
            Some(value) => Ok(value),
            None => {
                // Err(self.error_runtime("Stack underflow"))
                panic!("Stack underflow");
            }
        }
    }

    fn peek(&mut self, distance: usize) -> Result<Value, InterpretResult> {
        if distance >= self.stack.len() {
            // Err(self.error_runtime("Stack underflow"))
            panic!("Stack underflow");
        } else {
            Ok(self.stack[self.stack.len() - distance - 1].clone())
        }
    }

    /* Initialize callframe on the stack. Store a pointer to the function
     * being called and point the frame's ip to the beginning of the
     * function's bytecode. Finally, setup the slots pointer to give the
     * frame its window into the stack. The '-1' is for the stack 'slot 0'
     * which the compiler sets aside for when methods are added later.
     */
    fn call(&mut self, arg_count: usize) -> bool {
        let arity = if let Value::Func(callee) = self.peek(arg_count).unwrap() {
            callee.get_arity()
        } else {
            // shouldn't happen
            panic!("Can only call functions and classes.");
        };

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
            function: self.stack.len() - arg_count - 1,
            ip: RefCell::new(0),
            slots: self.stack.len() - arg_count - 1,
        });

        true
    }

    /*
     * The callee can be found at the offset 'arg_count' from the top
     * of the stack.
     */
    fn call_value(&mut self, arg_count: usize) -> bool {
        let callee = self.peek(arg_count).unwrap();
        let result = match callee {
            Value::Func(_f) => {
                return self.call(arg_count);
            }
            Value::Native(f) => {
                let stack_top = self.stack.len();
                let result = f.call(arg_count, &self.stack[stack_top - arg_count..stack_top]);
                self.stack.truncate(stack_top + arg_count + 1);
                self.stack.push(result);
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

    #[allow(dead_code)]
    fn print_stack(&self) {
        print!("          ");
        for slot in &self.stack {
            print!("[ ");
            print!("{}", slot);
            print!(" ]");
        }
        println!();
    }

    fn binary_op(&mut self, op: fn(a: Value, b: Value) -> Value) -> Result<(), InterpretResult> {
        if self.peek(0)?.is_string() && self.peek(1)?.is_string() {
            // pop b before a
            let b = self.pop()?;
            let a = self.pop()?;
            self.stack.push(Value::Str(format!("{}{}", a, b)));
            Ok(())
        } else if self.peek(0)?.is_number() && self.peek(1)?.is_number() {
            // pop b before a
            let b = self.pop()?;
            let a = self.pop()?;
            self.stack.push(op(a, b));
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
            let function = &self.stack[frame.function];
            let instruction = *frame.ip.borrow() - 1;
            if let Value::Func(func) = function {
                let line = func.as_ref().get_chunk().get_line(instruction);
                eprintln!("[line {}] in {}", line, func.stack_name());
            } else {
                panic!("Failed to get stacktrace");
            }
        }
        self.reset_stack();
        InterpretResult::RuntimeError
    }

    fn define_native<T: Into<String>>(&mut self, name: T, function: &Rc<dyn NativeFunction>) {
        self.globals
            .insert(name.into(), Value::Native(Rc::clone(function)));
    }
}
