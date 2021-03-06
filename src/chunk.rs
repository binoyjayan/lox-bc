use crate::value::*;
use std::convert::TryFrom;

pub enum Opcode {
    Constant,
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
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

    pub fn read_byte(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    pub fn add_constant(&mut self, value: Value) -> Option<u8> {
        let idx = self.constants.write(value);
        u8::try_from(idx).ok()
    }

    pub fn get_constant(&self, index: usize) -> &Value {
        self.constants.read_value(index)
    }

    pub fn get_line(&self, ip: usize) -> usize {
        self.lines[ip]
    }

    // pub fn free(&mut self) {
    //     self.code = Vec::new();
    //     self.lines = Vec::new();
    //     self.constants.free();
    // }

    pub fn disassemble_chunk<T: ToString>(&self, name: T) {
        // Display header to know which chunk is being disassembled
        println!("== {} ==", name.to_string());
        let mut offset = 0;

        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            // The instruction belongs to the same line in source file
            print!("{:>4} ", "|"); // right justify
        } else {
            print!("{:4} ", self.lines[offset]);
        }
        let instruction: Opcode = self.code[offset].into();
        match instruction {
            Opcode::Constant => self.constant_instruction("OP_CONSTANT", offset),
            Opcode::Return => self.simple_instruction("OP_RETURN", offset),
            Opcode::Add => self.simple_instruction("OP_AD", offset),
            Opcode::Subtract => self.simple_instruction("OP_SUBTRACT", offset),
            Opcode::Multiply => self.simple_instruction("OP_MULTIPLY", offset),
            Opcode::Divide => self.simple_instruction("OP_DIVIDE", offset),
            Opcode::Negate => self.simple_instruction("OP_NEGATE", offset),
            Opcode::Nil => self.simple_instruction("OP_NIL", offset),
            Opcode::True => self.simple_instruction("OP_TRUE", offset),
            Opcode::False => self.simple_instruction("OP_FALSE", offset),
            Opcode::Not => self.simple_instruction("OP_NOT", offset),
            Opcode::Equal => self.simple_instruction("OP_EQ", offset),
            Opcode::Greater => self.simple_instruction("OP_GT", offset),
            Opcode::Less => self.simple_instruction("OP_LT", offset),
            Opcode::Print => self.simple_instruction("OP_PRINT", offset),
            Opcode::Pop => self.simple_instruction("OP_POP", offset),
            Opcode::DefineGlobal => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            Opcode::GetGlobal => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
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
            0 => Opcode::Constant,
            1 => Opcode::Return,
            2 => Opcode::Add,
            3 => Opcode::Subtract,
            4 => Opcode::Multiply,
            5 => Opcode::Divide,
            6 => Opcode::Negate,
            7 => Opcode::Nil,
            8 => Opcode::True,
            9 => Opcode::False,
            10 => Opcode::Not,
            11 => Opcode::Equal,
            12 => Opcode::Greater,
            13 => Opcode::Less,
            14 => Opcode::Print,
            15 => Opcode::Pop,
            16 => Opcode::DefineGlobal,
            17 => Opcode::GetGlobal,
            _ => unimplemented!("Invalid opcode {}", code),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
