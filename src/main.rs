mod chunk;
mod value;

use chunk::*;

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write_byte(constant);
    chunk.write_opcode(Opcode::OpConstant);
    chunk.write_opcode(Opcode::OpReturn);
    chunk.disassemble_chunk("test chunk");
    chunk.free()
}
