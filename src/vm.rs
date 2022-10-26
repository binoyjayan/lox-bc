use std::rc::Rc;

use crate::chunk::*;
use crate::compiler::*;
use crate::error::*;
use crate::value::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    chunk: Rc<Chunk>,
    globals: HashMap<String, Value>,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
            chunk: Rc::new(Chunk::new()),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(&mut chunk);
        compiler.compile(source)?;
        self.ip = 0;
        self.chunk = Rc::new(chunk);
        self.run()
        // self.chunk.free();
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn run(&mut self) -> Result<(), InterpretResult> {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                self.print_stack();
                self.chunk.disassemble_instruction(self.ip);
            }
            let instruction: Opcode = self.read_opcode();
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant().clone();
                    self.stack.push(constant);
                }
                Opcode::Return => {
                    // Exit interpreter
                    return Ok(());
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
                    println!("{}\n", value);
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
                    self.stack.push(self.stack[slot].clone());
                }
                Opcode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    self.stack[slot] = self.peek(0)?.clone();
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0)?.is_falsey() {
                        self.ip += offset;
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_short();
                    self.ip += offset;
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

    fn read_byte(&mut self) -> u8 {
        let val = self.chunk.read_byte(self.ip);
        self.ip += 1;
        val
    }

    fn read_short(&mut self) -> usize {
        self.ip += 2;
        self.chunk.get_jump_offset(self.ip - 2)
    }

    fn read_opcode(&mut self) -> Opcode {
        self.read_byte().into()
    }

    fn read_constant(&mut self) -> &Value {
        let index = self.chunk.read_byte(self.ip) as usize;
        self.ip += 1;
        self.chunk.get_constant(index)
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
    fn error_runtime<T: ToString>(&mut self, err_str: T) -> InterpretResult {
        let line = self.chunk.get_line(self.ip - 1);
        eprintln!("{}", err_str.to_string());
        eprintln!("[line {}] in script", line);
        self.reset_stack();
        InterpretResult::RuntimeError
    }
}
