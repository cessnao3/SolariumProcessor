use super::{InstructionData, ToInstructionData};

use crate::assembly::argument::Argument;

const NUM_DATA_VALS: usize = 4;

#[derive(Clone, Copy)]
pub struct RegisterInstruction
{
    opcode: u8,
    num_regs: usize
}

impl RegisterInstruction
{
    pub fn new(opcode: u8, num_regs: usize) -> RegisterInstruction
    {
        assert!(num_regs < NUM_DATA_VALS);
        return RegisterInstruction
        {
            opcode,
            num_regs
        };
    }
}

impl ToInstructionData for RegisterInstruction
{
    fn to_instruction_data(&self, args: &Vec<Argument>) -> Result<InstructionData, String>
    {
        if args.len() != self.num_regs
        {
            return Err(format!(
                "instruction expected {0:} argument{1:}, got {2:}",
                self.num_regs,
                if args.len() > 0 { "s" } else { "" },
                args.len()));
        }

        assert!(args.len() > 0);
        assert!(args.len() < NUM_DATA_VALS);

        let mut nybbles: [u8; NUM_DATA_VALS] = [0; NUM_DATA_VALS];

        for i in 0..NUM_DATA_VALS
        {
            if i == args.len()
            {
                nybbles[i] = self.opcode;
            }
            else if i < args.len()
            {
                nybbles[i] = match args[i].to_register_val()
                {
                    Ok(v) => v,
                    Err(s) => return Err(s)
                };
            }
        }

        return Ok(InstructionData
        {
            opcode: nybbles[3],
            arg0: nybbles[2],
            arg1: nybbles[1],
            arg2: nybbles[0]
        });
    }
}
