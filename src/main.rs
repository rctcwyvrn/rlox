mod chunk;
mod debug;
mod value;
mod vm;
mod compiler;
mod scanner;

use crate::vm::{VM, ExecutionMode, InterpretResult};
use crate::chunk::{Chunk};
use crate::compiler::compile;

use std::env;
use std::io;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        let result = run_file(args.get(1).unwrap());
        println!("> {:?}",result);
    } else {
        println!("Usage: rlox [path]");
    }
}

fn repl() {
    let mut input = String::new();
    loop {
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                interpret(&input);
            }
            Err(error) => println!("Failed to get stdin: {}", error)
        }
        input.clear();
    }
}

fn run_file(filename: &String) -> InterpretResult{
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(why) => panic!("Failed to open {}: {}", path_display, why),
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => return interpret(&s),
        Err(why) => panic!("Failed to read {}: {}", path_display, why),
    };
}

fn interpret(source: &String) -> InterpretResult {
    let vm = VM::init_vm(ExecutionMode::Trace);
    //let vm = VM::init_vm(test_chunk, ExecutionMode::Default);

    let mut chunk = Chunk::init_chunk();
    let result = compile(source, &mut chunk);
    if !result {
        return InterpretResult::InterpretCompileError;
    }

    vm.run(&chunk)
}