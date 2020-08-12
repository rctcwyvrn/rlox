mod chunk;
mod compiler;
mod debug;
mod gc;
mod native;
mod prec;
mod resolver;
mod scanner;
mod value;
mod vm;

use crate::compiler::Compiler;
use crate::resolver::Resolver;
use crate::vm::{ExecutionMode, InterpretResult, VM};

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() >= 2 {
        let debug = (args.len() == 3) && args[2].eq("--debug");
        let result = run_file(args.get(1).unwrap(), debug);
        std::process::exit(match result {
            InterpretResult::InterpretOK => 0,
            InterpretResult::InterpretCompileError => 65,
            InterpretResult::InterpretRuntimeError => 70,
        })
    } else {
        println!("Usage: rlox path [--debug]");
    }
}

fn run_file(filename: &String, debug: bool) -> InterpretResult {
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(why) => panic!("Failed to open {}: {}", path_display, why),
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => return interpret(&s, debug),
        Err(why) => panic!("Failed to read {}: {}", path_display, why),
    };
}

fn interpret(source: &String, debug: bool) -> InterpretResult {
    let mut resolver = Resolver::new();
    let compiler = Compiler::new(source, &mut resolver);
    let result = compiler.compile(debug);
    if let None = result {
        return InterpretResult::InterpretCompileError;
    }

    let vm = if debug {
        VM::new(ExecutionMode::Trace, result.unwrap())
    } else {
        VM::new(ExecutionMode::Default, result.unwrap())
    };
    vm.run()
}
