use crate::value::*;

pub enum Opcode {
    OpConstant,
    OpReturn,
}

pub struct Chunk {
    code: Vec<u8>,
    constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.code.push(byte)
    }

    pub fn write_opcode(&mut self, code: Opcode) {
        self.code.push(code.into())
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value) as u8
    }

    pub fn free(&mut self) {
        self.code = Vec::new();
        self.constants.free();
    }

    pub fn disassemble_chunk<T: ToString>(&self, name: T) {
        // Display header to know which chunk is being disassembled
        println!("== {} ==", name.to_string());
        let mut offset = 0;

        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        let instruction: Opcode = self.code[offset].into();
        match instruction {
            Opcode::OpConstant => self.constant_instruction("OP_CONSTANT", offset),
            Opcode::OpReturn => self.simple_instruction("OP_RETURN", offset),
        }
    }

    // Print name of opcode followed by looking up the constant using
    // the index to the constant pool and printing the constant.
    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:-16}{:4} '", name, constant);
        self.constants.print(constant);
        println!("'");
        offset + 2
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }
}

impl From<u8> for Opcode {
    fn from(code: u8) -> Self {
        match code {
            0 => Opcode::OpConstant,
            1 => Opcode::OpReturn,
            _ => unimplemented!("Invalid opcode {}", code),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
