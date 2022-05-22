mod chunk;
mod compiler;
mod error;
mod scanner;
mod token;
mod value;
mod vm;

use chunk::*;
use error::*;
use vm::*;

use std::env;
use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::process;

fn main() {
    let mut vm = VM::new();
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]).expect("Failed to run file"),
        _ => {
            println!("Usage: {} <script>", &args[0]);
            process::exit(64);
        }
    }
}

pub fn repl(vm: &mut VM) {
    let stdin = io::stdin();
    print!(">> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        if let Ok(line) = line {
            vm.interpret(&line);
        }
        print!(">> ");
        io::stdout().flush().unwrap();
    }
    println!("\nExiting...");
}

fn run_file(vm: &mut VM, path: &str) -> io::Result<()> {
    let buf = fs::read_to_string(path)?;
    match vm.interpret(&buf.to_string()) {
        Ok(_) => Ok(()),
        Err(InterpretResult::CompileError) => std::process::exit(65),
        Err(InterpretResult::RuntimeError) => std::process::exit(70),
    }
}
