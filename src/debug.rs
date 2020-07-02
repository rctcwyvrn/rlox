use crate::chunk::{Chunk, Instr, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==============", name);
    println!("byte\tline\tOpCode");
    let mut last_line_num = 0;

    for (i,instr) in chunk.code.iter().enumerate() {
        let line_marker = if last_line_num == instr.line_num { "|".to_string() } else { instr.line_num.to_string() };
        last_line_num = instr.line_num;
        print!("{}\t{}", i, line_marker);
        disassemble_instruction(instr, &chunk)
    }

    println!("======================\n");
}

pub fn disassemble_instruction(instr: &Instr, chunk: &Chunk) {
    match instr.op_code {
        OpCode::OpConstant(index) | 
            OpCode::OpDefineGlobal(index) | 
            OpCode::OpSetGlobal(index) |
            OpCode::OpGetGlobal(index) => print!("\t{:?} => {:?}\n", instr.op_code, chunk.constants.get(index).unwrap()),
        _ => print!("\t{:?}\n", instr.op_code)
    }
}