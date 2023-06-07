use super::{InstructionData, ToInstructionData};

use crate::assembly::argument::Argument;
use crate::assembly::error::AssemblerError;

const NUM_DATA_VALS: usize = 4;

#[derive(Clone, Copy)]
pub struct RegisterInstruction {
    opcode: u8,
    num_regs: usize,
}

impl RegisterInstruction {
    pub fn new(opcode: u8, num_regs: usize) -> Self {
        assert!(num_regs < NUM_DATA_VALS);
        Self { opcode, num_regs }
    }
}

impl ToInstructionData for RegisterInstruction {
    fn to_instruction_data(&self, args: &[Argument]) -> Result<InstructionData, AssemblerError> {
        if args.len() != self.num_regs {
            return Err(AssemblerError::ArgumentCount { expected: self.num_regs, actual: args.len() });
        }

        assert!(!args.is_empty());
        assert!(args.len() < NUM_DATA_VALS);

        let mut nybbles: [u8; NUM_DATA_VALS] = [0; NUM_DATA_VALS];

        for i in 0..NUM_DATA_VALS {
            match i.cmp(&args.len()) {
                std::cmp::Ordering::Equal => nybbles[i] = self.opcode,
                std::cmp::Ordering::Less => {
                    nybbles[i] = match args[i].to_register_val() {
                        Ok(v) => v.get_index() as u8,
                        Err(s) => return Err(AssemblerError::ArgumentError(s)),
                    }
                }
                _ => (),
            }
        }

        Ok(InstructionData {
            opcode: nybbles[3],
            arg0: nybbles[2],
            arg1: nybbles[1],
            arg2: nybbles[0],
        })
    }
}
