use crate::chunk::*;
use crate::compiler::*;
use crate::error::*;
use crate::value::*;

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
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
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretResult> {
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(&mut chunk);
        compiler.compile(source)?;
        self.ip = 0;
        let result = self.run(&chunk);
        chunk.free();
        result
    }

    pub fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn run(&mut self, chunk: &Chunk) -> Result<(), InterpretResult> {
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
                        return self.error_runtime(chunk, "Operand must be a number");
                    }
                }
                Opcode::Return => {
                    println!("{}", self.pop()?);
                    return Ok(());
                }
            }
        }
    }

    fn pop(&mut self) -> Result<Value, InterpretResult> {
        // TODO: Report stack underflow
        self.stack.pop().ok_or(InterpretResult::RuntimeError)
    }

    fn peek(&mut self, distance: usize) -> Result<Value, InterpretResult> {
        // TODO: Report stack underflow
        Ok(self.stack[self.stack.len() - distance - 1])
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

    fn binary_op(&mut self, op: fn(a: Value, b: Value) -> Value) -> Result<(), InterpretResult> {
        if !self.peek(0)?.is_number() || !self.peek(1)?.is_number() {
            //return self.error_runtime()
            return Err(InterpretResult::RuntimeError);
        }
        // pop b before a
        let b = self.pop()?;
        let a = self.pop()?;
        self.stack.push(op(a, b));
        Ok(())
    }

    /*
     * Look into the chunk's debug lines array to find the executing instruction's
     * current line number in source code. 'ip' always points to the next instruction.
     * So, the current instr
     */
    fn error_runtime<T: ToString>(&mut self, chunk: &Chunk, err_str: T) -> Result<(), InterpretResult> {
        let line = chunk.get_line(self.ip - 1);
        eprintln!("{}", err_str.to_string());
        eprintln!("[line {}] in script", line);
        self.reset_stack();
        Err(InterpretResult::RuntimeError)
    }
}
