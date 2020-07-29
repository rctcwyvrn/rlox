use crate::chunk::{OpCode, Instr, FunctionChunk};
use crate::debug::*;
use crate::value::{Value, HeapObj, HeapObjVal, HeapObjType,  ObjClosure, ObjInstance, ObjPointer, is_falsey, values_equal};
use crate::native::*;
use crate::resolver::UpValue;

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

#[derive(Debug, Clone, Copy)]
pub enum DerefError {
    NotPointer,
    WrongType,
}

// Is it good rust to split these into two very coupled but seperate structs or is there a way to keep them together while not angering the borrow checker?
// Mutable VM state: stack, frames, ip within the frames, globals <-- this struct should be built in run and mutated in there
// Immutable VM values: mode, function chunks (code + constants) <-- this struct should be passed from the compiler as an immutable reference

#[derive(Debug, Clone)]
struct CallFrame {
    name: String,
    function: usize, // Index into the VM.functions Vec for which function is being called
    ip: usize,
    frame_start: usize,
}

pub struct VMState {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    pub instances: Vec<HeapObj>,
    // Not implemented due to it destryoing my code => multiple upvalues pointing to the same original value in a function will NOT affect each other. This is a small enough edge case that I'm willing to just let it go
    // upvalues: Vec<Value>, 
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

    // Note to future self: peek_mut SHOULD NEVER BE IMPLEMENTED! 
    // Values on the stack are always implicit copy/cloned, any persistent values must be ObjInstances on the instances vec with LoxPointers instead

    fn peek(&self) -> &Value {
        self.peek_at(0)
    }

    fn peek_at(&self, dist: usize) -> &Value {
        self.stack.get(self.stack.len() - dist - 1).unwrap()
    }

    /// FIXME: Replace this with a walking instantiation once garbage collection is added!!
    fn alloc(&mut self, val: HeapObj) -> Value {
        let index = self.instances.len();
        self.instances.push(val);
        Value::LoxPointer(ObjPointer { obj: index })
    }

    // Fixme: Figure out how to not copy paste this code for mut and immut
    pub fn deref(&self, pointer: ObjPointer) -> &HeapObj {
        match self.instances.get(pointer.obj) {
            Some(x) => x,
            None => panic!("VM panic! Invalid pointer"),
        }
    }

    fn deref_mut(&mut self, pointer: ObjPointer) -> &mut HeapObj {
        match self.instances.get_mut(pointer.obj) {
            Some(x) => x,
            None => panic!("VM panic! Invalid pointer"),
        }
    }

    /// Attempts to
    /// 1. Take the given Value as a LoxPointer
    /// 2. Deref it into a HeapObj
    /// 3. Match the obj_types
    fn deref_into(&self, pointer_val: &Value, obj_type: HeapObjType) -> Result<&HeapObjVal, DerefError> {
        if let Value::LoxPointer(pointer) = pointer_val {
            let obj = self.deref(*pointer);
            if obj.obj_type == obj_type {
                Ok(&obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
    }

    fn deref_into_mut(&mut self, pointer_val: &Value, obj_type: HeapObjType) -> Result<&mut HeapObjVal, DerefError> {
        if let Value::LoxPointer(pointer) = pointer_val {
            let obj = self.deref_mut(*pointer);
            if obj.obj_type == obj_type {
                Ok(&mut obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
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

    fn current_closure(&self) -> &ObjClosure {
        let pointer_val = self.stack.get(self.frame().frame_start).unwrap();
        match self.deref_into(pointer_val, HeapObjType::LoxClosure) {
            Ok(closure_obj) => closure_obj.as_closure(),
            Err(x) => panic!("VM panic! Unable to get current closure? {:?}", x),
        }
    }

    fn current_closure_mut(&mut self) -> &mut ObjClosure {
        let pointer_val = self.stack.get(self.frame().frame_start).unwrap().clone();
        match self.deref_into_mut(&pointer_val, HeapObjType::LoxClosure) {
            Ok(closure_obj) => closure_obj.as_closure_mut(),
            Err(x) => panic!("VM panic! Unable to get current closure? {:?}", x),
        }
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

    /// Fixme: mother of god there has to be a better way to do this than just having unwraps everywhere...
    fn capture_upvalue(&self, upvalue: &UpValue) -> Value {
        if upvalue.is_local {
            // Just copy the value from the current stack frame (which is the parents)
            self.stack.get(self.frame().frame_start + upvalue.index).unwrap().clone()
        } else {
            // Look at the current frame's closure's upvalue vec and copy it from there
            let parent_closure = self.current_closure();
            parent_closure.values.get(upvalue.index).unwrap().clone()
        }
    }

    fn push_upvalue(&mut self, index: usize) {
        let closure = self.current_closure();
        let val = closure.values.get(index).unwrap().clone();
        self.push(val);
    }

    fn set_upvalue(&mut self, index: usize) {
        let val = self.peek().clone();
        let closure = self.current_closure_mut();
        closure.values[index] = val;
    }

    /// Checks if the targetted Value is a LoxFunction, passes it to call() to continue attempting the call.
    /// Calls call_native() if the Value is a NativeFunction
    /// 
    /// Returns a String containing an error message or None
    fn call_value(&mut self, arg_count: usize, function_defs: &Vec<FunctionChunk>) -> Option<String> {
        let callee = self.peek_at(arg_count);
        if let Value::LoxPointer(_) = callee {
            match self.deref_into(callee, HeapObjType::LoxClosure) {
                Ok(closure) => {
                    let closure = closure.as_closure();
                    let fn_index = closure.function;
                    self.call(fn_index, arg_count, function_defs)
                },
                Err(_) => Some(String::from("Can only call functions and classses")),
            }
        } else if let Value::LoxFunction(_fn_index) = callee {
            panic!("VM panic! Tried to call a LoxFunction that was not wrapped in a LoxClosure? How did this happen?");
        } else if let Value::NativeFunction(native_fn) = callee {
            let native_fn = native_fn.clone();
            self.call_native(&native_fn, arg_count);
            None
        } else if let Value::LoxClass(class) = callee {
            //let class = class.class;

            // No constructors for now, just put on the instance
            if arg_count != 0 { panic!("constructor arguments not implemented yet")};

            let instance_obj = ObjInstance::new(*class);
            self.pop(); // Pop the LoxClass

            let ptr = self.alloc(HeapObj::new_instance(instance_obj));
            self.push(ptr);

            // The problem: 
            // Once gc is added the instances vec is gonna shrink at random times, meaning the LoxPointers will get shifted around
            // This means that we need to replace the values in the vec with placeholder values
            // So the "malloc" for self.instances will need to walk along and place values in properly
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
            name: target_fn.name.clone().unwrap_or(String::from("main")),
            function: fn_index,
            ip: 0,
            frame_start: self.stack.len() - arg_count - 1,
        };
        self.frames.push(frame);
        return None;
    }

    /// Attempts to call a native (rust) function
    fn call_native(&mut self, native_fn: &NativeFn, arg_count: usize) {
        let mut args: Vec<Value> = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop());
        }
        self.pop(); // Pop off the Value::NativeFunction
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

    /// Just adds clutter for now, re-add when there is a native lib to actually use
    fn define_std_lib(&mut self) {
        self.define_native(String::from("test_native"), test_native);
    }

    /// Initializes the VMState with:
    /// 
    /// - A CallFrame for function #0
    /// - Defined global variables for the native functions
    /// - A Value::LoxFunction for function #0 pushed onto the stack
    fn new() -> VMState {
        let first_fn = CallFrame {
            name: String::from("main"),
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
            instances: Vec::new(),
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

    fn runtime_error(&self, msg: &str, state: &VMState) {
        eprintln!("{}", msg);
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
            eprintln!("== Starting execution | Mode: {:?} ==", self.mode);
            debug_print_constants(&self);
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
                debug_trace(&self, &instr, &state);
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
                            self.runtime_error(format!("Undefined variable '{}'", var_name).as_str(), &state);
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
                        self.runtime_error(format!("Undefined variable '{}'", var_name).as_str(), &state);
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

                OpCode::OpGetProperty(index) => {
                    let name = self.get_variable_name(index, &state);
                    let pointer_val = state.peek();

                    match state.deref_into(pointer_val, HeapObjType::LoxInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance();
                            if instance.fields.contains_key(&name) {
                                let value = instance.fields.get(&name).unwrap().clone();
                                state.pop(); // Remove the instance
                                state.push(value); // Replace with the value
                            } else {
                                self.runtime_error(format!("Undefined property {} found in {:?}", name, instance).as_str(), &state);
                                return InterpretResult::InterpretRuntimeError;
                            }
                        },
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", pointer_val.to_string(&self, &state));
                            self.runtime_error(msg.as_str(), &state);
                            return InterpretResult::InterpretRuntimeError;
                        }, 
                    }
                },
                OpCode::OpSetProperty(index) => { // Fixme: this is nearly identical to OpGetProperty, is there any way to combine them nicely?
                    let name = self.get_variable_name(index, &state);
                    let val = state.pop();
                    let pointer_val = state.peek().clone();

                    match state.deref_into_mut(&pointer_val, HeapObjType::LoxInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance_mut();
                            instance.fields.insert(name, val.clone());
                        },
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", pointer_val.to_string(&self, &state));
                            self.runtime_error(msg.as_str(), &state);
                            return InterpretResult::InterpretRuntimeError;
                        }, 
                    }

                    // We return on an error, so we can clean up the stack now
                    state.pop(); // Instance
                    state.push(val); // Return the value to the stack
                },

                OpCode::OpGetUpvalue(index) => { state.push_upvalue(index); },
                OpCode::OpSetUpvalue(index) => { state.set_upvalue(index); },

                OpCode::OpClosure => {
                    if let Value::LoxFunction(function) = state.pop() {
                        let mut closure = ObjClosure::new(function); // Capture values into the closure here

                        let fn_chunk = self.functions.get(function).unwrap();
                        for upvalue in fn_chunk.upvalues.as_ref().unwrap().iter() {
                            closure.values.push(state.capture_upvalue(upvalue))
                        }
                        let ptr = state.alloc(HeapObj::new_closure(closure));
                        state.push(ptr);
                    } else {
                        panic!("VM panic! Attempted to wrap a non-function value in a closure");
                    }
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

                OpCode::OpClass(_index) => state.push(Value::LoxClass(0)),

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
                        self.runtime_error("Operands must be numbers or strings", &state);
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
                    println!("{}",state.pop().to_string(&self, &state));
                }
            }
        }
    }
}

fn debug_state_trace(state: &VMState) {
    eprintln!("> Frame: {:?}", state.frame());
    eprintln!("> Stack: ");
    for value in state.stack.iter() {
        eprintln!(">> [ {:?} ]", value);
    }
    eprintln!("> Globals: ");
    for (name, val) in state.globals.iter() {
        eprintln!(">> {} => {:?}", name, val);
    }
    debug_instances(state);
}

fn debug_instances(state: &VMState) {
    eprintln!("> Instances: ");
    for (i,instance) in state.instances.iter().enumerate() {
        eprintln!(">> [{}] {:?}", i, instance)
    }
}

fn debug_trace(vm: &VM, instr: &Instr, state: &VMState) {
    eprintln!("---");
    eprint!("> Next instr (#{}): ", state.frame().ip - 1);
    disassemble_instruction(instr, &vm.get_chunk(state).chunk, state.frame().ip - 1);
    debug_state_trace(state);
    eprintln!("---\n");
}

fn debug_print_constants(vm: &VM) {
    eprintln!("---");
    eprintln!("> Constants");
    for fn_chunk in vm.functions.iter(){
        let name = if let Some(fn_name) = &fn_chunk.name {
            fn_name.clone()
        } else {
            String::from("script")
        };
        eprintln!(">>> {}", name);
        for val in fn_chunk.chunk.constants.iter() {
            eprintln!(">> [ {:?} ]", val);
        }
    }
    eprintln!("---\n");
}