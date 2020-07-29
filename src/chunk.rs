use crate::value::Value;
use crate::resolver::{UpValue};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn,
    OpPop,
    
    OpDefineGlobal(usize), // Index of the LoxString name for this variable name in the constants vec
    OpGetGlobal(usize), // ^
    OpSetGlobal(usize), // ^
    
    OpGetLocal(usize), // Index on the stack
    OpSetLocal(usize), // ^

    OpGetProperty(usize), // Index of the LoxString variable in the constants vec corresponding with the property name
    OpSetProperty(usize), // ^
                          // Optimization todo: Fix this LoxString storing in the constants vec. Either only save one, or get rid of it entierly by resolving the globals/instance slot at compilation

    OpGetUpvalue(usize), // upvalue index for a closure
    OpSetUpvalue(usize), // ^
    OpClosure, // Wraps the top value of the stack (must be a LoxFunction) in a LoxClosure, capturing the appropriate UpValues at the same time 

    OpJump(usize), // Jump ip offset
    OpJumpIfFalse(usize),
    OpLoop(usize), // Jump backwards by offset

    OpCall(usize), // Arity

    OpClass(usize), // Index in the constants vec for the class name => temp: replace this with the index into a vec of functionchunk like objects

    OpConstant(usize), // Index of the constant we want to retrieve
    OpNil,
    OpTrue,
    OpFalse,

    OpNegate,
    OpNot,

    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpEqual,
    OpGreater,
    OpLess,

    OpPrint,
}

#[derive(Debug, Clone, Copy)]
pub struct Instr {
    pub op_code: OpCode,
    pub line_num: usize
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub constants: Vec<Value>
}

impl Chunk {
    pub fn write_instruction(&mut self, instruction: Instr) {
        self.code.push(instruction);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn get_constant(&self, index: usize) -> Value {
        let val = self.constants.get(index);
        match val {
            Some(x) => x.clone(),
            None => panic!("VM panic: Constant with given index not found") // add runtime failure in here later
        }
    }

    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Script
}

/// Compile time representation of a function, ie its code, name, resolved closure information
#[derive(Debug)]
pub struct FunctionChunk {
    pub chunk: Chunk,
    pub name: Option<String>, // None for the top level script
    pub arity: usize,
    pub fn_type: FunctionType,
    pub upvalues: Option<Vec<UpValue>>, // None while the function is being defined/for the top level script, must be set to Some after the definition is complete
}

impl FunctionChunk {
    pub fn new(name: Option<String>, arity: usize, fn_type: FunctionType) -> FunctionChunk {
        FunctionChunk {
            chunk: Chunk::new(),
            name,
            arity,
            fn_type,
            upvalues: None,
        }
    }

    pub fn set_upvalues(&mut self, upvalues: Vec<UpValue>) {
        self.upvalues = Some(upvalues);
    }
}
