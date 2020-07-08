use crate::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OpReturn,
    OpPop,
    
    OpDefineGlobal(usize), // Index of the LoxString variable name in the constants vec
    OpGetGlobal(usize), // ^
    OpSetGlobal(usize), // ^

    OpGetLocal(usize), // Index on the stack
    OpSetLocal(usize), // ^

    OpJump(usize), // Jump ip offset
    OpJumpIfFalse(usize),
    OpLoop(usize), // Jump backwards by offset
    
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

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub struct FunctionChunk {
    pub chunk: Chunk,
    pub name: Option<String>,
    pub arity: usize,
    pub fn_type: FunctionType,
}

impl FunctionChunk {
    pub fn new(name: Option<String>, arity: usize, fn_type: FunctionType) -> FunctionChunk {
        FunctionChunk {
            chunk: Chunk::new(),
            name,
            arity,
            fn_type,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Script
}