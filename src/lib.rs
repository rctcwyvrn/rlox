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
use crate::vm::{ExecutionMode, VM};

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    InterpretOK,
    InterpretCompileError,
    InterpretRuntimeError,
}

pub fn interpret(source: &String, debug: bool, quiet: bool) -> InterpretResult {
    let compiler = Compiler::new(source, quiet);
    let result = compiler.compile(debug);
    if let None = result {
        return InterpretResult::InterpretCompileError;
    }

    let result = result.unwrap();
    let vm = if debug {
        VM::new(ExecutionMode::Trace, result, quiet)
    } else {
        VM::new(ExecutionMode::Default, result, quiet)
    };
    vm.run()
}
