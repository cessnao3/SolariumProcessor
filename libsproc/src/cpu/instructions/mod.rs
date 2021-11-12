pub struct InstructionGroup
{
    pub opcode: u8,
    pub arg0: u8,
    pub arg1: u8,
    pub arg2: u8
}

impl InstructionGroup
{
    pub fn new(instruction: u16) -> InstructionGroup
    {
        // Extract the different argument types
        let opcode = ((instruction & 0xF000) >> 12) as u8;
        let arg0 = ((instruction & 0x0F00) >> 8) as u8;
        let arg1 = ((instruction & 0x00F0) >> 4) as u8;
        let arg2 = ((instruction & 0x000F) >> 0) as u8;

        assert!(opcode & 0xF == opcode);
        assert!(arg0 & 0xF == arg0);
        assert!(arg1 & 0xF == arg1);
        assert!(arg2 & 0xF == arg2);

        return Self
        {
            opcode,
            arg0,
            arg1,
            arg2
        };
    }
}
