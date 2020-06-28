use crate::value::Value;

#[derive(Debug)]
pub enum OpCode {
    OpReturn,
    OpNegate,
    OpConstant(usize), // Index of the constant we want to retrieve
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
}

#[derive(Debug)]
pub struct Instr {
    pub op_code: OpCode,
    pub line_num: usize
}

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
            None => Value::Double(69.69) // add runtime failure in here later
        }
    }

    pub fn init_chunk() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new()
        }
    }
}