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
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
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
            24 => Opcode::Call,
            25 => Opcode::Closure,
            26 => Opcode::GetUpvalue,
            27 => Opcode::SetUpvalue,
            _ => unimplemented!("Invalid opcode {}", code),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(code: Opcode) -> Self {
        code as u8
    }
}
