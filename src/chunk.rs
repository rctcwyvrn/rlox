use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn,
    OpPop,
    
    OpDefineGlobal(usize), // Index of the LoxString variable name in the constants vec
    OpGetGlobal(usize), // ^
    OpSetGlobal(usize), // ^
    OpGetLocal(usize), // Index on the stack
    OpSetLocal(usize), // ^
    OpGetUpvalue(usize), // upvalue index for a closure
    OpSetUpvalue(usize), // ^

    OpJump(usize), // Jump ip offset
    OpJumpIfFalse(usize),
    OpLoop(usize), // Jump backwards by offset

    OpClosure, // Wraps the top value of the stack (must be a LoxFunction) in a LoxClosure
    OpCall(usize), // Arity

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

    pub upvalue_count: usize,
}

impl FunctionChunk {
    pub fn new(name: Option<String>, arity: usize, fn_type: FunctionType) -> FunctionChunk {
        FunctionChunk {
            chunk: Chunk::new(),
            name,
            arity,
            fn_type,
            upvalue_count: 0,
        }
    }
}
