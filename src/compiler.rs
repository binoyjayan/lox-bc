use crate::error::*;
use crate::scanner::*;
use crate::token::*;
use crate::vm::*;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, source: &String) -> Result<(), InterpretResult> {
        let mut line = 0;
        let mut scanner = Scanner::new(source);

        loop {
            let token = scanner.scan_token();
            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("    | ");
            }
            println!("{:10?} '{}'", token.ttype, token.lexeme);
            if token.ttype == TokenType::Eof {
                break;
            }
        }
        Ok(())
    }
}
