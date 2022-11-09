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
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
}

#[derive(Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: ValueArray,
}

pub enum ChunkType {
    Script,
    Function,
}

#[derive(PartialEq, Eq)]
#[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
pub enum JumpStyle {
    Forwards,
    Backwards,
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

    pub fn write_at(&mut self, offset: usize, byte: u8) {
        self.code[offset] = byte;
    }

    pub fn read_byte(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    pub fn add_constant(&mut self, value: Value) -> Option<u8> {
        let idx = self.constants.write(value);
        u8::try_from(idx).ok()
    }

    pub fn get_constant(&self, index: usize) -> Value {
        self.constants.read_value(index).clone()
    }

    pub fn get_line(&self, ip: usize) -> usize {
        self.lines[ip]
    }

    pub fn count(&self) -> usize {
        self.lines.len()
    }

    pub fn get_jump_offset(&self, offset: usize) -> usize {
        (((self.code[offset] as u16) << 8) | self.code[offset + 1] as u16) as usize
    }

    // pub fn free(&mut self) {
    //     self.code = Vec::new();
    //     self.lines = Vec::new();
    //     self.constants.free();
    // }
    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn disassemble_chunk<T: ToString>(&self, name: T) {
        // Display header to know which chunk is being disassembled
        println!("== {} ==", name.to_string());
        let mut offset = 0;

        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        use JumpStyle::*;
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
            Opcode::GetGlobal => self.constant_instruction("OP_GET_GLOBAL", offset),
            Opcode::SetGlobal => self.constant_instruction("OP_SET_GLOBAL", offset),
            Opcode::GetLocal => self.byte_instruction("OP_GET_LOCAL", offset),
            Opcode::SetLocal => self.byte_instruction("OP_SET_LOCAL", offset),
            Opcode::JumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", Forwards, offset),
            Opcode::Jump => self.jump_instruction("OP_JUMP", Forwards, offset),
            Opcode::Loop => self.jump_instruction("OP_LOOP", Backwards, offset),
        }
    }

    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:-16} {:4}", name, slot);
        offset + 2
    }

    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    pub fn jump_instruction(&self, name: &str, jump_style: JumpStyle, offset: usize) -> usize {
        let jump = self.get_jump_offset(offset + 1);
        // let jump = (((self.code[offset + 1] as u16) << 8) | self.code[offset + 2] as u16) as usize;
        let jump_to = if jump_style == JumpStyle::Forwards {
            offset + 3 + jump
        } else {
            offset + 3 - jump
        };
        println!("{:-16} {:4} -> {}", name, offset, jump_to);
        offset + 3
    }

    // Print name of opcode followed by looking up the constant using
    // the index to the constant pool and printing the constant.
    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:-16} {:4} '", name, constant);
        self.constants.print(constant);
        println!("'");
        offset + 2
    }

    #[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
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
            18 => Opcode::SetGlobal,
            19 => Opcode::GetLocal,
            20 => Opcode::SetLocal,
            21 => Opcode::JumpIfFalse,
            22 => Opcode::Jump,
            23 => Opcode::Loop,
            _ => unimplemented!("Invalid opcode {}", code),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
