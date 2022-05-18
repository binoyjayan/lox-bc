use crate::chunk::*;
use crate::value::*;

pub struct VM {
    ip: usize,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new() -> Self {
        Self { ip: 0 }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> InterpretResult {
        self.ip = 0;
        self.run(chunk)
    }

    fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        loop {
            let instruction: Opcode = self.read_opcode(chunk);
            match instruction {
                Opcode::OpReturn => return InterpretResult::Ok,
                Opcode::OpConstant => {
                    let constant = self.read_constant(chunk);
                    println!("{}", constant);
                }
                _ => todo!(),
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
}
