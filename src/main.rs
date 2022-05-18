mod chunk;
mod value;
mod vm;

use chunk::*;
use vm::*;

fn main() {
    let mut vm = VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_byte(constant, 123);
    chunk.write_opcode(Opcode::OpConstant, 123);
    chunk.write_opcode(Opcode::OpReturn, 123);
    chunk.disassemble_chunk("test chunk");

    vm.interpret(&chunk);
    chunk.free()
}
