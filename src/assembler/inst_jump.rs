use std::collections::HashMap;

/// Provides basic information for the different types of jump instructions
#[derive(Clone)]
pub struct InstructionJump
{
    pub name: String,
    pub opcode: u8,
    pub num_operands: usize
}

impl InstructionJump
{
    pub fn get_instructions() -> HashMap<String, InstructionJump>
    {
        let instructions = vec![
            InstructionJump
            {
                name: "jmp".to_string(),
                opcode: 0x20,
                num_operands: 0
            },
            InstructionJump
            {
                name: "jne".to_string(),
                opcode: 0x21,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jeq".to_string(),
                opcode: 0x22,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jn".to_string(),
                opcode: 0x23,
                num_operands: 1
            },
            InstructionJump
            {
                name: "jp".to_string(),
                opcode: 0x24,
                num_operands: 1
            },
            InstructionJump
            {
                name: "jge".to_string(),
                opcode: 0x25,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jg".to_string(),
                opcode: 0x26,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jle".to_string(),
                opcode: 0x27,
                num_operands: 2
            },
            InstructionJump
            {
                name: "jl".to_string(),
                opcode: 0x28,
                num_operands: 2
            }
        ];

        let mut inst_map: HashMap<String, InstructionJump> = HashMap::new();

        for inst in instructions.iter()
        {
            if inst_map.contains_key(&inst.name)
            {
                panic!("double instruction found for {0:}", inst.name);
            }

            inst_map.insert(
                inst.name.to_string(),
                inst.clone());
        }

        return inst_map;
    }

    pub fn expected_args(&self) -> usize
    {
        // Provide the number of operands, plus one for the destination location
        return self.num_operands + 1;
    }
}
