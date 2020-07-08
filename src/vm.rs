use crate::chunk::{OpCode, Instr, FunctionChunk};
use crate::debug::*;
use crate::value::{Value, is_falsey, values_equal};
use crate::native::*;

use std::collections::HashMap;

const FRAMES_MAX: usize = 64;

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

// Is it good rust to split these into two very coupled but seperate structs or is there a way to keep them together while not angering the borrow checker?
// Mutable VM state: stack, frames, ip within the frames, globals <-- this struct should be built in run and mutated in there
// Immutable VM values: mode, function chunks (code + constants) <-- this struct should be passed from the compiler as an immutable reference

#[derive(Debug, Clone, Copy)]
struct CallFrame {
    function: usize, // Index into the VM.functions Vec for which function is being called
    ip: usize,
    frame_start: usize,
}

struct VMState {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
}

impl VMState {
    fn push(&mut self, val: Value) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(x) => x,
            None => panic!("VM panic! Attempted to pop a value when the value stack was empty")
        }
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn peek_at(&self, dist: usize) -> &Value {
        self.stack.get(self.stack.len() - dist - 1).unwrap()
    }

    fn frame(&self) -> &CallFrame {
        match self.frames.last() {
            Some(x) => x,
            None => panic!("VM panic! Ran out of call frames?"),
        }
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        match self.frames.last_mut() {
            Some(x) => x,
            None => panic!("VM panic! Ran out of call frames?"),
        }
    }

    /// Returns the index into the stack where the current call frame's relative stack starts
    fn frame_start(&self) -> usize {
        self.frame().frame_start
    }
    
    fn increment_ip(&mut self) {
        self.frame_mut().ip+=1;
    }

    fn jump(&mut self, offset: usize) {
        self.frame_mut().ip += offset;
    }

    fn jump_back(&mut self, neg_offset: usize) {
        self.frame_mut().ip -= neg_offset;
    }

    /// Checks if the targetted Value is a LoxFunction, passes it to call() to continue attempting the call
    /// 
    /// Returns a String containing an error message or None
    fn call_value(&mut self, arg_count: usize, function_defs: &Vec<FunctionChunk>) -> Option<String> {
        let callee = self.peek_at(arg_count);
        if let Value::LoxFunction(fn_index) = callee {
            let fn_index = *fn_index; // Borrow checker dance
            self.call(fn_index, arg_count, function_defs)
        } else if let Value::NativeFunction(native_fn) = callee {
            let native_fn = native_fn.clone();
            self.call_native(&native_fn, arg_count);
            None
        } else {
            Some(String::from("Can only call functions and classses"))
        }
    }

    /// Attempts to call a function with the values on the stack, with the given # of arguments
    fn call(&mut self, fn_index: usize, arg_count: usize, function_defs: &Vec<FunctionChunk>) -> Option<String> {
        let target_fn = function_defs.get(fn_index).unwrap();
        if arg_count != target_fn.arity {
            return Some(format!("Expected {} arguments but got {} instead", target_fn.arity, arg_count));
        }

        if self.frames.len() == FRAMES_MAX {
            return Some(String::from("Stack overflow"));
        }

        let frame = CallFrame {
            function: fn_index,
            ip: 0,
            frame_start: self.stack.len() - arg_count - 1
        };
        self.frames.push(frame);
        return None;
    }

    /// Attempts to call a native (rust) function
    fn call_native(&mut self, native_fn: &NativeFn, arg_count: usize) {
        let mut args: Vec<Value> = Vec::new();
        for _ in 0..arg_count {
            println!("popping! {:?}", self.stack);
            args.push(self.pop());
        }
        let result = native_fn(arg_count, args);
        self.push(result);
    }

    fn define_native(&mut self, name: String, native_fn: NativeFn) {
        self.push(Value::LoxString(name.clone()));           // For garbage collection later?
        self.push(Value::NativeFunction(native_fn)); // ^
        self.globals.insert(name, Value::NativeFunction(native_fn));
        self.pop();
        self.pop();
    }

    fn define_std_lib(&mut self) {
        self.define_native(String::from("test_native"), test_native);
    }

    fn debug_trace(&self) {
        println!("> Stack: ");
        for value in &self.stack {
            println!(">> [ {:?} ]", value);
        }
        println!("> Globals: ");
        for (name, val) in self.globals.iter() {
            println!(">> {} => {:?}", name, val);
        }
    }

    /// Initializes the VMState with:
    /// 
    /// - A CallFrame for function #0
    /// - Defined global variables for the native functions
    /// - A Value::LoxFunction for function #0 pushed onto the stack
    fn new() -> VMState {
        let first_fn = CallFrame {
            function: 0,
            ip: 0,
            frame_start: 0,
        };

        let mut frames = Vec::new();
        frames.push(first_fn);

        let first_val = Value::LoxFunction(0);
        let mut stack = Vec::new();
        stack.push(first_val);

        let mut state = VMState {
            stack,
            frames,
            globals: HashMap::new(),
        };

        state.define_std_lib();
        return state;
    }
}

pub struct VM {
    mode: ExecutionMode,
    pub functions: Vec<FunctionChunk>, // Stores all function definitions
}

impl VM {
    pub fn new<'a>(mode: ExecutionMode, functions: Vec<FunctionChunk>) -> VM {
        VM { mode, functions }
    }

    fn get_chunk(&self, state: &VMState) -> &FunctionChunk {
        match self.functions.get(state.frame().function) {
            Some(x) => x,
            None => panic!("VM panic! Invalid CallFrame function index"),
        }
    }

    fn debug_trace(&self, instr: &Instr, state: &VMState) {
        println!("---");
        print!("> Next instr (#{}): ", state.frame().ip - 1);
        disassemble_instruction(instr, &self.get_chunk(state).chunk, state.frame().ip - 1);
        state.debug_trace();
        println!("---\n");
    }

    fn debug_print_constants(&self) {
        println!("---");
        println!("> Constants");
        for fn_chunk in self.functions.iter(){
            let name = if let Some(fn_name) = &fn_chunk.name {
                fn_name.clone()
            } else {
                String::from("script")
            };
            println!(">>> {}", name);
            for val in fn_chunk.chunk.constants.iter() {
                println!(">> [ {:?} ]", val);
            }
        }
        println!("---\n");
    }

    fn runtime_error(&self, msg: &str, state: &VMState) {
        eprintln!("{}", msg);
        //eprintln!("\t[line {}] in script\n", line);

        for call_frame in state.frames.iter().rev() {
            let function = self.functions.get(call_frame.function).unwrap();
            eprint!("[line {}] in ", function.chunk.code.get(call_frame.ip).unwrap().line_num);
            match &function.name {
                Some(name) => eprintln!("{}", name),
                None => eprintln!("script"),
            }
        }
    }

    fn get_variable_name(&self, index: usize, state: &VMState) -> String {
        let name_val = self.get_chunk(state).chunk.get_constant(index); // FIXME: should each FunctionChunk have a constants field or should it be shared? Does this work for globals? (I think it does because each identifier makes a new LoxString on the relevant chunk's constants vec)
        if let Value::LoxString(var_name) = name_val {
            return var_name;
        } else {
            panic!("VM panic: Found a non LoxString value for a variable name");
        }
    }

    fn get_instr(&self, state: &VMState) -> Option<&Instr> {
        let frame = state.frame();
        let fun = self.functions.get(frame.function)?;
        let instr = fun.chunk.code.get(frame.ip)?;
        Some(instr)
    }

    pub fn run(&self) -> InterpretResult {
        if let ExecutionMode::Trace = self.mode {
            println!("== Starting execution | Mode: {:?} ==", self.mode);
            self.debug_print_constants();
        }

        let mut state = VMState::new();

        // Move this into a match arm that matches all the binary ops, and then matches on the individual opcodes?
        macro_rules! op_binary {
            ($val_type: path, $oper: tt) => {
                {
                    //if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                    if let (Value::Double(a), Value::Double(b)) = (state.pop(), state.pop()) {
                        state.push($val_type(b $oper a))
                    } else {
                        self.runtime_error("Operands must be numbers", &state);
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
            }
        }


        loop {
            let instr = self.get_instr(&state);
            if let None = instr {
                panic!("VM panic! Unable to get next instruction :c");
            }

            let instr = instr.unwrap();
            state.increment_ip();

            if let ExecutionMode::Trace = self.mode {
                self.debug_trace(&instr, &state);
            }

            match instr.op_code {
                OpCode::OpReturn => {
                        let result = state.pop(); // Save the result (the value on the top of the stack)
                        for _ in 0..(state.stack.len() - state.frame().frame_start) { // Clean up the call frame part of that stack
                            state.pop();
                        }

                        state.frames.pop();
                        if state.frames.is_empty() {
                            return InterpretResult::InterpretOK;
                        } else {
                            state.push(result); // Push the result back
                        }
                    },
                OpCode::OpPop => {
                    state.pop();
                },
                OpCode::OpDefineGlobal(index) => {
                    let var_name = self.get_variable_name(index, &state);
                    let var_val = state.pop();
                    state.globals.insert(var_name, var_val);
                },
                OpCode::OpGetGlobal(index) => {
                    let var_name = self.get_variable_name(index, &state);
                    let var_val = state.globals.get(&var_name);
                    match var_val {
                        Some(x) => {
                            let new = x.clone();
                            state.push(new)
                        },
                        None => {
                            self.runtime_error(&format!("Undefined variable '{}'", var_name)[..], &state);
                            return InterpretResult::InterpretRuntimeError;
                        },
                    }
                }
                OpCode::OpSetGlobal(index) => {
                    let var_name = self.get_variable_name(index, &state);

                    // We don't want assignment to pop the value since this is an expression
                    // this will almost always be in a expression statement, which will pop the value
                    let var_val = state.peek().clone();
                    if !state.globals.contains_key(&var_name) {
                        self.runtime_error(&format!("Undefined variable '{}'", var_name)[..], &state);
                        return InterpretResult::InterpretRuntimeError
                    } else {
                        state.globals.insert(var_name, var_val);
                    }
                }
                OpCode::OpGetLocal(index) => state.push(state.stack[state.frame_start() + index].clone()),    // Note: We gotta clone these values around the stack because our operators pop off the top and we also don't want to modify the variable value
                OpCode::OpSetLocal(index) => {
                    let dest = state.frame_start() + index;
                    state.stack[dest] = state.peek().clone(); // Same idea as OpSetGlobal, don't pop value since it's an expression
                },

                OpCode::OpJump(offset) => state.jump(offset),
                OpCode::OpJumpIfFalse(offset) => {
                    if is_falsey(state.peek()) { // Does not pop the value off the top of the stack because we need them for logical operators
                        state.jump(offset);
                    }
                },
                OpCode::OpLoop(neg_offset) => state.jump_back(neg_offset),

                OpCode::OpCall(arity) => {
                    let result = state.call_value(arity, &self.functions);
                    if let Some(msg) = result { 
                        self.runtime_error(&msg[..], &state);
                        return InterpretResult::InterpretRuntimeError; 
                    }
                },

                OpCode::OpConstant(index) => state.push(self.get_chunk(&state).chunk.get_constant(index)), // FIXME
                OpCode::OpTrue            => state.push(Value::Bool(true)),
                OpCode::OpFalse           => state.push(Value::Bool(false)),
                OpCode::OpNil             => state.push(Value::Nil),

                OpCode::OpAdd           => {
                    let t = (state.pop(), state.pop());
                    if let (Value::LoxString(a), Value::LoxString(b)) = t {
                        state.push(Value::LoxString(format!("{}{}",b,a)))
                    } else if let (Value::Double(a), Value::Double(b)) = t {
                        state.push(Value::Double(a + b))
                    } else {
                        self.runtime_error("Operands must be numbers", &state);
                        return InterpretResult::InterpretRuntimeError;
                    }
                },
                OpCode::OpSubtract      => op_binary!(Value::Double, -),
                OpCode::OpMultiply      => op_binary!(Value::Double, *),
                OpCode::OpDivide        => op_binary!(Value::Double, /),
                OpCode::OpGreater       => op_binary!(Value::Bool, >),
                OpCode::OpLess          => op_binary!(Value::Bool, <),
                OpCode::OpEqual => {
                    let t = (state.pop(), state.pop());
                    state.push(Value::Bool(values_equal(t)));
                },

                OpCode::OpNot => {
                    let val = Value::Bool(is_falsey(&state.pop()));
                    state.push(val);
                },
                OpCode::OpNegate => {
                    let value = state.pop().as_num(); 
                    match value {
                        Some(x) => state.push(Value::Double(x * -1.0)),
                        None => {
                            self.runtime_error("Attempted to negate a non-number value", &state);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                },

                OpCode::OpPrint => {
                    println!("{}",state.pop().to_string(&self));
                }
            }
        }
    }
}