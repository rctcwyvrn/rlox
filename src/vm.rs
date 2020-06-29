use crate::chunk::{Chunk, OpCode, Instr};
use crate::debug::*;
use crate::value::{Value, is_falsey, values_equal};

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

pub struct VM<'a> {
    mode: ExecutionMode,
    chunk: &'a Chunk,
    stack: Vec<Value>,
    ip: usize,
}

impl VM<'_> {
    pub fn init_vm(mode: ExecutionMode, chunk: &Chunk) -> VM {
        VM { mode, chunk, stack: Vec::new(), ip: 0}
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap() // add runtime failure handling in here later
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn debug_trace(&self, instr: &Instr) {
        // DEBUG TRACE EXECUTION
        println!("---");
        print!("Next instr: ");
        disassemble_instruction(instr, self.chunk);
        println!("Stack: ");
        for value in &self.stack {
            println!("[ {:?} ]", value);
        }
        println!("---\n");
    }

    fn runtime_error(&self, msg: &str) {
        let instr = self.ip - 1;
        let line = self.chunk.code.get(instr).unwrap().line_num;
        eprintln!("{}", msg);
        eprintln!("\t[line {}] in script\n", line);
    }

    pub fn run(mut self) -> InterpretResult {
        // throw this bad boi in here
        macro_rules! op_binary {
            ($val_type: path, $oper: tt) => {
                {
                    //if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                    if let (Value::Double(a), Value::Double(b)) = (self.pop(), self.pop()) {
                        self.push($val_type(a $oper b))
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
            }
        }

        println!("== Starting execution | Mode: {:?} ==", self.mode);
        while self.ip < self.chunk.code.len() {
            let instr = self.chunk.code.get(self.ip).unwrap();
            self.ip = self.ip + 1;

            if let ExecutionMode::Trace = self.mode {
                self.debug_trace(&instr);
            }

            match instr.op_code {
                OpCode::OpReturn => { 
                    println!("temp return thing: {:?}", self.pop());
                    return InterpretResult::InterpretOK
                },

                OpCode::OpConstant(index) => self.push(self.chunk.get_constant(index)),
                OpCode::OpTrue          => self.push(Value::Bool(true)),
                OpCode::OpFalse         => self.push(Value::Bool(false)),
                OpCode::OpNil           => self.push(Value::Nil),
                
                OpCode::OpAdd           => {
                    let t = (self.pop(), self.pop());
                    if let (Value::LoxString(a), Value::LoxString(b)) = t {
                        self.push(Value::LoxString(format!("{}{}",b,a)))
                    } else if let (Value::Double(a), Value::Double(b)) = t {
                        self.push(Value::Double(a + b))
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return InterpretResult::InterpretRuntimeError;
                    }
                },
                OpCode::OpSubtract      => op_binary!(Value::Double,-),
                OpCode::OpMultiply      => op_binary!(Value::Double,*),
                OpCode::OpDivide        => op_binary!(Value::Double,/),
                OpCode::OpGreater       => op_binary!(Value::Bool, >),
                OpCode::OpLess          => op_binary!(Value::Bool, <),
                OpCode::OpEqual => {
                    let t = (self.pop(), self.pop());
                    self.push(Value::Bool(values_equal(t)));
                },

                OpCode::OpNot => {
                    let val = Value::Bool(is_falsey(self.pop()));
                    self.push(val);
                },
                OpCode::OpNegate => {
                    let value = self.pop().as_num(); 
                    match value {
                        Some(x) => self.push(Value::Double(x * -1.0)),
                        None => {
                            self.runtime_error("Attempted to negate a non-number value");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                },
            }
        }

        return InterpretResult::InterpretRuntimeError;
    }
}