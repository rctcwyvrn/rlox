use crate::resolver::UpValue;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    OpReturn,
    OpPop,

    OpDefineGlobal(usize), // Index of the LoxString name for this variable name in the constants vec
    OpGetGlobal(usize),    // ^
    OpSetGlobal(usize),    // ^
    OpGetSuper(usize),     //  ^
    OpCallGlobal(usize,usize), // A combination of OpCall and OpGetGlobal
    OpGetLocal(usize), // Index on the stack
    OpSetLocal(usize), // ^

    OpInvoke(usize, usize), // Combines a GetProperty and a Call. Contains the exact same information. First usize is the index for the property name, second is for the arity
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

    OpClass(usize), // Index into the classes vec for the ClassChunk object

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Instr {
    pub op_code: OpCode,
    pub line_num: usize,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<Instr>,
}

impl Chunk {
    pub fn write_instruction(&mut self, instruction: Instr) {
        self.code.push(instruction);
    }

    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

/// Compile time representation of a function, ie its code, name, resolved closure information
#[derive(Debug)]
pub struct FunctionChunk {
    pub chunk: Chunk,
    pub name: Option<String>, // None for the top level script
    pub arity: usize,
    pub fn_type: FunctionType,
    pub upvalues: Option<Vec<UpValue>>, // None while the function is being defined and for functions without upvalues. If the function does have upvalues, this field must be set and must be binded with an OpClosure
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

/// Compile time repr of a class
#[derive(Debug)]
pub struct ClassChunk {
    pub name: String,
    pub methods: HashMap<String, usize>,
    pub superclass: Option<usize>,
}

impl ClassChunk {
    pub fn new(name: String) -> ClassChunk {
        ClassChunk {
            name,
            methods: HashMap::new(),
            superclass: None,
        }
    }
}
