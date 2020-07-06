use crate::chunk::{Chunk, OpCode, Instr};
use crate::debug::*;
use crate::value::{Value, is_falsey, values_equal};

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
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
    globals: HashMap<String, Value>,
    ip: usize,
}

impl VM<'_> {
    pub fn init_vm(mode: ExecutionMode, chunk: &Chunk) -> VM {
        VM { mode, chunk, stack: Vec::new(), globals: HashMap::new(), ip: 0}
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
        println!("---");
        print!("> Next instr (#{}): ", self.ip - 1);
        disassemble_instruction(instr, self.chunk, self.ip - 1);
        println!("> Stack: ");
        for value in &self.stack {
            println!(">> [ {:?} ]", value);
        }
        println!("> Globals: ");
        for (name, val) in self.globals.iter() {
            println!(">> {} => {:?}", name, val);
        }
        println!("---\n");
    }

    fn debug_print_constants(&self) {
        println!("---");
        println!("> Constants");
        for val in &self.chunk.constants {
            println!(">> [ {:?} ]", val);
        }
        println!("---\n");
    }

    fn runtime_error(&self, msg: &str) {
        let instr = self.ip - 1;
        let line = self.chunk.code.get(instr).unwrap().line_num;
        eprintln!("{}", msg);
        eprintln!("\t[line {}] in script\n", line);
    }

    fn get_variable_name(&self, index: usize) -> String {
        let name_val = self.chunk.get_constant(index);
        if let Value::LoxString(var_name) = name_val {
            return var_name;
        } else {
            panic!("VM panic: Found a non LoxString value for a variable name");
        }
    }

    pub fn run(mut self) -> InterpretResult {
        // throw this bad boi in here
        macro_rules! op_binary {
            ($val_type: path, $oper: tt) => {
                {
                    //if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                    if let (Value::Double(a), Value::Double(b)) = (self.pop(), self.pop()) {
                        self.push($val_type(b $oper a))
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
            }
        }

        if let ExecutionMode::Trace = self.mode {
            self.debug_print_constants();
            println!("== Starting execution | Mode: {:?} ==", self.mode);
        }

        while self.ip < self.chunk.code.len() {
            let instr = self.chunk.code.get(self.ip).unwrap();
            self.ip = self.ip + 1;

            if let ExecutionMode::Trace = self.mode {
                self.debug_trace(&instr);
            }

            match instr.op_code {
                OpCode::OpReturn => { return InterpretResult::InterpretOK }, // soon
                OpCode::OpPop => {
                    self.pop();
                },
                OpCode::OpDefineGlobal(index) => {
                    let var_name = self.get_variable_name(index);
                    let var_val = self.pop();
                    self.globals.insert(var_name, var_val);
                },
                OpCode::OpGetGlobal(index) => {
                    let var_name = self.get_variable_name(index);
                    let var_val = self.globals.get(&var_name);
                    match var_val {
                        Some(x) => {
                            let new = x.clone();
                            self.push(new)
                        },
                        None => {
                            self.runtime_error(&format!("Undefined variable '{}'", var_name)[..]);
                            return InterpretResult::InterpretRuntimeError;
                        },
                    }
                }
                OpCode::OpSetGlobal(index) => {
                    let var_name = self.get_variable_name(index);

                    // We don't want assignment to pop the value since this is an expression
                    // this will almost always be in a expression statement, which will pop the value
                    let var_val = self.peek().clone();
                    if !self.globals.contains_key(&var_name) {
                        self.runtime_error(&format!("Undefined variable '{}'", var_name)[..]);
                        return InterpretResult::InterpretRuntimeError
                    } else {
                        self.globals.insert(var_name, var_val);
                    }
                }
                OpCode::OpGetLocal(index) => self.push(self.stack[index].clone()),    // Note: We gotta clone these values around the stack because our operators pop off the top and we also don't want to modify the variable value
                OpCode::OpSetLocal(index) => self.stack[index] = self.peek().clone(), // Same idea as OpSetGlobal, don't pop value since it's an expression

                OpCode::OpJump(offset) => self.ip += offset, // Kinda really gross
                OpCode::OpJumpIfFalse(offset) => {
                    if is_falsey(self.peek()) { // Does not pop the value off the top of the stack because we need them for logical operators
                        self.ip += offset;
                    }
                },
                OpCode::OpLoop(neg_offset) => self.ip -= neg_offset,

                OpCode::OpConstant(index) => self.push(self.chunk.get_constant(index)),
                OpCode::OpTrue            => self.push(Value::Bool(true)),
                OpCode::OpFalse           => self.push(Value::Bool(false)),
                OpCode::OpNil             => self.push(Value::Nil),
                
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
                OpCode::OpSubtract      => op_binary!(Value::Double, -),
                OpCode::OpMultiply      => op_binary!(Value::Double, *),
                OpCode::OpDivide        => op_binary!(Value::Double, /),
                OpCode::OpGreater       => op_binary!(Value::Bool, >),
                OpCode::OpLess          => op_binary!(Value::Bool, <),
                OpCode::OpEqual => {
                    let t = (self.pop(), self.pop());
                    self.push(Value::Bool(values_equal(t)));
                },

                OpCode::OpNot => {
                    let val = Value::Bool(is_falsey(&self.pop()));
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

                OpCode::OpPrint => {
                    println!("{}",self.pop().to_string());
                }
            }
        }

        return InterpretResult::InterpretRuntimeError;
    }
}