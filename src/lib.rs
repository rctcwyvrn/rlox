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
use crate::vm::{ExecutionMode, VM};

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    InterpretOK,
    InterpretCompileError,
    InterpretRuntimeError,
}

pub fn interpret(source: &String, debug: bool) -> InterpretResult {
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
