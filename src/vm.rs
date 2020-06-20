use crate::chunk::{Chunk, OpCode};
use crate::debug::*;
use crate::value::Value;

#[derive(Debug)]
pub enum InterpretResult {
    InterpretOK,
    InterpretCompileError,
    InterpretRuntimeError
}

#[derive(Debug)]
pub enum ExecutionMode {
    Default,
    Trace
}

pub struct VM {
    mode: ExecutionMode,
    stack: Vec<Value>
}

impl VM {
    pub fn init_vm(mode: ExecutionMode) -> VM {
        VM { mode, stack: Vec::new() }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap() // add runtime failure handling in here later
    }

    pub fn run(mut self, chunk: &Chunk) -> InterpretResult {
        // throw this bad boi in here
        #[macro_export]
        macro_rules! op_binary {
            ($oper:tt) => {
                if let Value::Double(b) = self.pop() {
                    if let Value::Double(a) = self.pop() {
                        self.stack.push(Value::Double(a $oper b))
                    }
                }
            }
        }

        println!("== Starting execution | Mode: {:?} ==", self.mode);
        for instr in chunk.code.iter() {
            if let ExecutionMode::Trace = self.mode {
                // DEBUG TRACE EXECUTION
                println!("---");
                print!("Next instr: ");
                disassemble_instruction(&instr, &chunk);
                println!("Stack: ");
                for value in &self.stack {
                    println!("[ {:?} ]", value);
                }
                println!("---");
            }

            match instr.op_code {
                OpCode::OpReturn => { 
                    println!("temp return thing: {:?}", self.stack.pop().unwrap());
                    return InterpretResult::InterpretOK
                },
                OpCode::OpConstant(index) => self.stack.push(chunk.get_constant(index)),
                OpCode::OpNegate => { 
                    if let Value::Double(x) = self.pop() { self.stack.push(Value::Double(x * -1.0)); }
                },
                OpCode::OpAdd => op_binary!(+),
                OpCode::OpSubtract => op_binary!(-),
                OpCode::OpMultiply => op_binary!(*),
                OpCode::OpDivide => op_binary!(/),
            }
        }

        return InterpretResult::InterpretRuntimeError;
    }
}