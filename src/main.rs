extern crate core;

mod chunk;
mod compiler;
mod error;
mod function;
mod native;
mod precedence;
mod scanner;
mod token;
mod value;
mod vm;

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
            let _ = vm.interpret(&line);
        }
        print!(">> ");
        io::stdout().flush().unwrap();
    }
    println!("\nExiting...");
}

fn run_file(vm: &mut VM, path: &str) -> io::Result<()> {
    let buf = fs::read_to_string(path)?;
    match vm.interpret(&buf) {
        Ok(_) => Ok(()),
        Err(InterpretResult::CompileError) => std::process::exit(65),
        Err(InterpretResult::RuntimeError) => std::process::exit(70),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(file_path: &str, expected: bool) {
        let mut vm = VM::new();
        let buf = fs::read_to_string(file_path);
        if buf.is_err() {
            assert!(false, "Failed to read file");
        }
        let buf = buf.unwrap();
        let result = match vm.interpret(&buf) {
            Ok(_) => true,
            _ => false,
        };
        assert!(result == expected);
    }
    #[test]
    fn var_declaration() {
        run_test("./examples/breakfast.lox", true);
    }

    #[test]
    fn for_loop() {
        run_test("./examples/for-loop.lox", true);
    }

    #[test]
    fn function1_for() {
        run_test("./examples/function1-for.lox", true);
    }

    #[test]
    fn function2_broken() {
        run_test("./examples/function2-broken.lox", false);
    }

    #[test]
    fn function3_fibonacci() {
        run_test("./examples/function3-fib.lox", true);
    }
}
