use crate::chunk::{Chunk, Instr, OpCode};
use crate::value::Value;

pub fn disassemble_chunk(chunk: &Chunk, name: &Option<String>) {
    match name {
        Some(name) => println!("== <fn {}> ==============", name),
        None => println!("== <script> =============="),
    }
    println!("Constants");
    for (i, val) in chunk.constants.iter().enumerate() {
        println!("{}\t{:?}", i, val);
    }
    println!("---");
    println!("byte\tline\tOpCode");
    let mut last_line_num = 0;
    for (i,instr) in chunk.code.iter().enumerate() {
        let line_marker = if last_line_num == instr.line_num { "|".to_string() } else { instr.line_num.to_string() };
        last_line_num = instr.line_num;
        print!("{}\t{}", i, line_marker);
        disassemble_instruction(instr, &chunk, i)
    }

    println!("======================\n");
}

pub fn disassemble_instruction(instr: &Instr, chunk: &Chunk, instr_offset: usize) {
    match instr.op_code {
        OpCode::OpConstant(index) => println!("\t{:?} => {:?}", instr.op_code, chunk.constants.get(index).unwrap()), 
        OpCode::OpDefineGlobal(index) | 
            OpCode::OpSetGlobal(index) |
            OpCode::OpGetGlobal(index) => {
                if let Value::LoxString(name) = chunk.constants.get(index).unwrap() {
                    println!("\t{:?} => name: {:?}", instr.op_code, name)
                }
            },
        OpCode::OpJump(jump_offset) |
            OpCode::OpJumpIfFalse(jump_offset) => println!("\t{:?} | jump -> {}", instr.op_code, instr_offset + jump_offset + 1),
        OpCode::OpLoop(neg_offset) => println!("\t{:?} | loop back -> {}", instr.op_code, instr_offset - neg_offset + 1),
        _ => println!("\t{:?}", instr.op_code)
    }
}