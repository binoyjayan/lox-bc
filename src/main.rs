extern crate core;

mod chunk;
mod class;
mod closure;
mod compiler;
mod error;
mod function;
mod instance;
mod native;
mod opcode;
mod precedence;
mod scanner;
mod token;
mod upvalue;
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
        run_test("./examples/function-for.lox", true);
    }

    #[test]
    fn function_broken() {
        run_test("./examples/function-broken.lox", false);
    }

    #[test]
    fn function_fibonacci() {
        run_test("./examples/function-fibonacci.lox", true);
    }

    #[test]
    fn function_native() {
        run_test("./examples/function-native.lox", true);
    }

    #[test]
    fn closure_simple() {
        run_test("./examples/closure-simple.lox", true);
    }

    #[test]
    fn closure_bake() {
        run_test("./examples/closure-bake.lox", true);
    }

    #[test]
    fn closure_outer() {
        run_test("./examples/closure-outer.lox", true);
    }

    #[test]
    fn closure_closed_upvalues() {
        run_test("./examples/closure-closed-upvalues.lox", true);
    }

    #[test]
    fn closure_assigned() {
        run_test("./examples/closure-assigned.lox", true);
    }

    #[test]
    fn closure_inner_outer() {
        run_test("./examples/closure-inner-outer.lox", true);
    }

    #[test]
    fn closure_close_over_variable() {
        run_test("./examples/closure-close-over-variable.lox", true);
    }

    #[test]
    fn closure_devious() {
        run_test("./examples/closure-devious.lox", true);
    }

    #[test]
    fn class_simple() {
        run_test("./examples/class-simple.lox", true);
    }

    #[test]
    fn class_this() {
        run_test("./examples/class-this.lox", true);
    }

    #[test]
    fn class_this_misused() {
        run_test("./examples/class-this-misused.lox", false);
    }

    #[test]
    fn class_initializer() {
        run_test("./examples/class-initializer.lox", true);
    }

    #[test]
    fn class_invoke_methods() {
        run_test("./examples/class-invoke-methods.lox", true);
    }

    #[test]
    fn class_invoke_fields() {
        run_test("./examples/class-invoke-fields.lox", true);
    }

    #[test]
    fn class_superclass_inheritance() {
        run_test("./examples/class-inheritance-simple.lox", true);
    }

    #[test]
    fn class_superclass_inheritance_super() {
        run_test("./examples/class-inheritance-super.lox", true);
    }
}
