use crate::chunk::{Chunk, ClassChunk, FunctionChunk, Instr, OpCode};
use crate::value::Value;

pub fn disassemble_class_chunk(
    class_chunk: &ClassChunk,
    function_defs: &Vec<FunctionChunk>,
    class_defs: &Vec<ClassChunk>,
    constants: &Vec<Value>,
    identifiers: &Vec<String>,
) {
    match class_chunk.superclass {
        Some(i) => eprintln!(
            "== <class {} | subclass of {}> ===============",
            &class_chunk.name, &class_defs[i].name
        ),
        None => eprintln!("== <class {}> ===============", &class_chunk.name),
    }
    for (name, fn_index) in class_chunk.methods.iter() {
        eprintln!(
            "== <method {} | #{}> ============",
            identifiers.get(*name).unwrap(),
            fn_index
        );
        disassemble_chunk(&function_defs[*fn_index].chunk, constants, identifiers);
    }
}

pub fn disassemble_fn_chunk(
    index: usize,
    fn_chunk: &FunctionChunk,
    constants: &Vec<Value>,
    identifiers: &Vec<String>,
) {
    match &fn_chunk.name {
        Some(name) => eprintln!("== <fn {} | #{}> ==============", name, index),
        None => eprintln!("== <script> =============="),
    }
    disassemble_chunk(&fn_chunk.chunk, constants, identifiers);
}

fn disassemble_chunk(chunk: &Chunk, constants: &Vec<Value>, identifiers: &Vec<String>) {
    eprintln!("---");
    eprintln!("byte\tline\tOpCode");
    let mut last_line_num = 0;
    for (i, instr) in chunk.code.iter().enumerate() {
        let line_marker = if last_line_num == instr.line_num {
            "|".to_string()
        } else {
            instr.line_num.to_string()
        };
        last_line_num = instr.line_num;
        eprint!("{}\t{}", i, line_marker);
        disassemble_instruction(instr, i, constants, identifiers)
    }

    eprintln!("======================\n");
}

pub fn disassemble_instruction(
    instr: &Instr,
    instr_offset: usize,
    constants: &Vec<Value>,
    identifiers: &Vec<String>,
) {
    match instr.op_code {
        OpCode::OpConstant(index) => eprintln!(
            "\t{:?} => {:?}",
            instr.op_code,
            constants.get(index).unwrap()
        ),
        OpCode::OpDefineGlobal(index)
        | OpCode::OpGetSuper(index)
        | OpCode::OpSetGlobal(index)
        | OpCode::OpGetGlobal(index)
        | OpCode::OpCallGlobal(index, _)
        | OpCode::OpGetProperty(index)
        | OpCode::OpSetProperty(index) => eprintln!(
            "\t{:?} => name: {:?}",
            instr.op_code,
            identifiers.get(index).unwrap()
        ),
        OpCode::OpJump(jump_offset) | OpCode::OpJumpIfFalse(jump_offset) => eprintln!(
            "\t{:?} | jump -> {}",
            instr.op_code,
            instr_offset + jump_offset
        ),
        OpCode::OpLoop(neg_offset) => eprintln!(
            "\t{:?} | loop back -> {}",
            instr.op_code,
            instr_offset - neg_offset
        ),
        _ => eprintln!("\t{:?}", instr.op_code),
    }
}
