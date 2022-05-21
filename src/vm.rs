use crate::chunk::*;
use crate::compiler::*;
use crate::value::*;

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &String) -> InterpretResult {
        let mut compiler = Compiler::new();
        compiler.compile(source);
        InterpretResult::Ok
        // self.ip = 0;
        // self.run(chunk)
    }

    fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                self.print_stack();
                chunk.disassemble_instruction(self.ip);
            }
            let instruction: Opcode = self.read_opcode(chunk);
            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant(chunk);
                    self.stack.push(constant);
                    // InterpretResult::Ok?
                }
                Opcode::Add => {
                    self.binary_op(|a, b| a + b);
                }
                Opcode::Subtract => {
                    self.binary_op(|a, b| a - b);
                }
                Opcode::Multiply => {
                    self.binary_op(|a, b| a * b);
                }
                Opcode::Divide => {
                    self.binary_op(|a, b| a / b);
                }
                Opcode::Negate => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(-value);
                    } else {
                        // Stack overflow
                        return InterpretResult::RuntimeError;
                    }
                }
                Opcode::Return => {
                    return if let Some(value) = self.stack.pop() {
                        println!("{}", value);
                        InterpretResult::Ok
                    } else {
                        // Stack overflow
                        InterpretResult::RuntimeError
                    };
                }
            }
        }
    }

    fn read_opcode(&mut self, chunk: &Chunk) -> Opcode {
        let val: Opcode = chunk.read_byte(self.ip).into();
        self.ip += 1;
        val
    }

    fn read_constant(&mut self, chunk: &Chunk) -> Value {
        let index = chunk.read_byte(self.ip) as usize;
        self.ip += 1;
        chunk.get_constant(index)
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

    fn binary_op(&mut self, op: fn(a: Value, b: Value) -> Value) -> InterpretResult {
        // pop b before a
        if let (Some(b), Some(a)) = (self.stack.pop(), self.stack.pop()) {
            self.stack.push(op(a, b));
            InterpretResult::Ok
        } else {
            // Stack overflow
            InterpretResult::RuntimeError
        }
    }
}
