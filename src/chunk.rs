use crate::value::*;

pub enum Opcode {
    OpConstant,
    OpReturn,
}

pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn write_opcode(&mut self, code: Opcode, line: usize) {
        self.code.push(code.into());
        self.lines.push(line);
    }

    pub fn read_byte(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value) as u8
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants.read_value(index)
    }

    pub fn free(&mut self) {
        self.code = Vec::new();
        self.lines = Vec::new();
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
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            // The instruction belongs to the same line in source file
            print!("{:>4} ", "|"); // right justify
        } else {
            print!("{:4} ", self.lines[offset]);
        }
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
