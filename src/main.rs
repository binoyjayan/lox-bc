mod chunk;
mod value;
mod vm;

use chunk::*;
use vm::*;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();

    // Define the first constant
    let constant = chunk.add_constant(1.2);
    chunk.write_byte(constant, 123);
    chunk.write_opcode(Opcode::Constant, 123);

    // Define a second constant
    let constant = chunk.add_constant(3.4);
    chunk.write_opcode(Opcode::Constant, 123);
    chunk.write_byte(constant, 123);
    // Add the two constants
    chunk.write_opcode(Opcode::Add, 123);

    // Define a third constant
    let constant = chunk.add_constant(5.6);
    chunk.write_opcode(Opcode::Constant, 123);
    chunk.write_byte(constant, 123);
    chunk.write_opcode(Opcode::Divide, 123);

    // Negate the value
    chunk.write_opcode(Opcode::Negate, 123);
    chunk.write_opcode(Opcode::Return, 123);
    chunk.disassemble_chunk("test chunk");
    vm.interpret(&chunk);
    chunk.free()
}
