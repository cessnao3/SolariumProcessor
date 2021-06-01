pub struct InstructionJump
{
    pub opcode: u8,
    pub num_operands: usize,
    pub second_is_zero: bool
}

impl InstructionJump
{
    pub fn get_by_name(name: &str) -> Result<InstructionJump, String>
    {
        // Determine parameter values
        let opcode: u8;
        let num_operands: usize;
        let mut second_is_zero: bool = false;

        // Change based on input name
        if name == "jmp"
        {
            opcode = 0x20;
            num_operands = 0;
        }
        else if name == "jne"
        {
            opcode = 0x21;
            num_operands = 2;
        }
        else if name == "jnz"
        {
            opcode = 0x21;
            num_operands = 1;
            second_is_zero = true;
        }
        else if name == "jeq"
        {
            opcode = 0x22;
            num_operands = 2;
        }
        else if name == "jez"
        {
            opcode = 0x22;
            num_operands = 1;
            second_is_zero = true;
        }
        else if name == "jn"
        {
            opcode = 0x23;
            num_operands = 1;
        }
        else if name == "jp"
        {
            opcode = 0x24;
            num_operands = 1;
        }
        else if name == "jge"
        {
            opcode = 0x25;
            num_operands = 2;
        }
        else if name == "jg"
        {
            opcode = 0x26;
            num_operands = 2;
        }
        else if name == "jle"
        {
            opcode = 0x27;
            num_operands = 2;
        }
        else if name == "jl"
        {
            opcode = 0x28;
            num_operands = 2;
        }
        else
        {
            return Err(format!("unknown jump instruction for {0:}", name));
        }

        // Return the result
        return Ok(InstructionJump
        {
            opcode,
            num_operands,
            second_is_zero
        });
    }

    pub fn expected_words(&self) -> usize
    {
        return 1 + self.num_operands + 1;
    }
}
