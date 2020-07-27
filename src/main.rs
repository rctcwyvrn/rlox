mod chunk;
mod debug;
mod value;
mod vm;
mod compiler;
mod scanner;
mod prec;
mod native;
mod resolver;

use crate::vm::{VM, ExecutionMode, InterpretResult};
use crate::compiler::{Compiler, DEBUG};
use crate::resolver::{Resolver};

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
        eprintln!("> {:?}", result);
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

fn run_file(filename: &String) -> InterpretResult {
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
    // Debug thing: Dump tokens
    // let mut temp_scanner = crate::scanner::Scanner::new(source);
    // let mut x = temp_scanner.scan_token();
    // while !(x.token_type == crate::scanner::TokenType::TokenEOF) {
    //     println!("{:?}",x);
    //     x = temp_scanner.scan_token();
    // }

    let mut resolver = Resolver::new();
    let compiler = Compiler::new(source, &mut resolver);
    let result = compiler.compile();
    if let None = result {
        return InterpretResult::InterpretCompileError;
    }

    let vm = if DEBUG { VM::new(ExecutionMode::Trace, result.unwrap()) } else { VM::new(ExecutionMode::Default, result.unwrap()) };
    vm.run()
}