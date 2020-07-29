use crate::chunk::{Chunk, Instr, OpCode};
use crate::value::Value;

pub const DEBUG: bool = false;

pub fn disassemble_chunk(chunk: &Chunk, name: &Option<String>) {
    match name {
        Some(name) => eprintln!("== <fn {}> ==============", name),
        None => eprintln!("== <script> =============="),
    }
    eprintln!("Constants");
    for (i, val) in chunk.constants.iter().enumerate() {
        eprintln!("{}\t{:?}", i, val);
    }
    eprintln!("---");
    eprintln!("byte\tline\tOpCode");
    let mut last_line_num = 0;
    for (i,instr) in chunk.code.iter().enumerate() {
        let line_marker = if last_line_num == instr.line_num { "|".to_string() } else { instr.line_num.to_string() };
        last_line_num = instr.line_num;
        eprint!("{}\t{}", i, line_marker);
        disassemble_instruction(instr, &chunk, i)
    }

    eprintln!("======================\n");
}

pub fn disassemble_instruction(instr: &Instr, chunk: &Chunk, instr_offset: usize) {
    match instr.op_code {
        OpCode::OpConstant(index) => eprintln!("\t{:?} => {:?}", instr.op_code, chunk.constants.get(index).unwrap()), 
        OpCode::OpDefineGlobal(index) | 
            OpCode::OpSetGlobal(index) |
            OpCode::OpGetGlobal(index) |
            OpCode::OpClass(index) => {
                if let Value::LoxString(name) = chunk.constants.get(index).unwrap() {
                    eprintln!("\t{:?} => name: {:?}", instr.op_code, name)
                }
            },
        OpCode::OpJump(jump_offset) |
            OpCode::OpJumpIfFalse(jump_offset) => eprintln!("\t{:?} | jump -> {}", instr.op_code, instr_offset + jump_offset + 1),
        OpCode::OpLoop(neg_offset) => eprintln!("\t{:?} | loop back -> {}", instr.op_code, instr_offset - neg_offset + 1),
        _ => eprintln!("\t{:?}", instr.op_code)
    }
}