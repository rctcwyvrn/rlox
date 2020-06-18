mod chunk;
mod debug;
mod value;
mod vm;

use crate::chunk::{Instr, OpCode, Chunk};
use crate::debug::disassemble_chunk;
use crate::value::Value;
use crate::vm::{VM, ExecutionMode};

fn main() {
    let mut test_chunk = Chunk::init_chunk();
    let test_return = Instr {
        op_code: OpCode::OpReturn,
        line_num: 12
    };

    let index = test_chunk.add_constant(Value::Double(5.0));
    let test_constant_1 = Instr {
        op_code: OpCode::OpConstant(index),
        line_num: 12
    };

    let index = test_chunk.add_constant(Value::Double(20.0));
    let test_constant_2 = Instr {
        op_code: OpCode::OpConstant(index),
        line_num: 12
    };

    let test_negate = Instr {
        op_code: OpCode::OpNegate,
        line_num: 12
    };

    let test_bin_op = Instr {
        //op_code: OpCode::OpMultiply,
        op_code: OpCode::OpDivide,
        //op_code: OpCode::OpSubtract,
        //op_code: OpCode::OpAdd,
        line_num: 12
    };

    test_chunk.write_instruction(test_constant_1);
    test_chunk.write_instruction(test_constant_2);
    test_chunk.write_instruction(test_negate);
    test_chunk.write_instruction(test_bin_op);
    test_chunk.write_instruction(test_return);

    disassemble_chunk(&test_chunk, "test program");

    let vm = VM::init_vm(ExecutionMode::Trace);
    //let vm = VM::init_vm(test_chunk, ExecutionMode::Default);

    vm.run(test_chunk);
}
