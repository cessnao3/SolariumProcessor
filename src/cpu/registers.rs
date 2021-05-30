use crate::memory::MemoryWord;

const NUM_REGISTERS: usize = 16;

pub enum Register
{
    ProgramCounter,
    StackPointer,
    StatusFlag,
    Zero,
    GP4,
    GP5,
    GP6,
    GP7,
    GP8,
    GP9,
    GP10,
    GP11,
    GP12,
    GP13,
    GP14,
    GP15
}

impl Register
{
    pub fn to_index(&self) -> usize
    {
        return match self
        {
            Register::ProgramCounter => 0,
            Register::StackPointer => 1,
            Register::StatusFlag => 2,
            Register::Zero => 3,
            Register::GP4 => 4,
            Register::GP5 => 5,
            Register::GP6 => 6,
            Register::GP7 => 7,
            Register::GP8 => 8,
            Register::GP9 => 9,
            Register::GP10 => 10,
            Register::GP11 => 11,
            Register::GP12 => 12,
            Register::GP13 => 13,
            Register::GP14 => 14,
            Register::GP15 => 15
        };
    }
}

pub struct RegisterManager
{
    registers: [MemoryWord; NUM_REGISTERS]
}

impl RegisterManager
{
    pub fn new() -> RegisterManager
    {
        return RegisterManager
        {
            registers: [0; NUM_REGISTERS]
        };
    }

    pub fn reset(&mut self)
    {
        for v in self.registers.iter_mut()
        {
            *v = 0;
        }
    }

    pub fn get(&self, register: Register) -> MemoryWord
    {
        return self.registers[register.to_index()];
    }

    pub fn set(&mut self, register: Register, value: MemoryWord)
    {
        self.registers[register.to_index()] = match register
        {
            Register::Zero => 0,
            _ => value
        };
    }
}
