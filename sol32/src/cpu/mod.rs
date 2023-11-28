use crate::memory::{MemoryMap, MemoryError};

pub enum ProcessorError {
    Memory(MemoryError),
    UnsupportedInterrupt(usize),
    UnknownRegister(usize),
    UnknownOperation(Instruction),
    StackUnderflow,
}

impl From<MemoryError> for ProcessorError {
    fn from(value: MemoryError) -> Self {
        Self::Memory(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    ProgramCounter,
    Status,
    StackPointer,
    Overflow,
    Return,
    ArgumentBase,
    GeneralPurpose(usize),
}

struct RegisterManager {
    registers: [u32; Self::REGISTER_COUNT],
}

impl RegisterManager {
    pub const REGISTER_COUNT: usize = 32;

    pub fn get(&self, reg: Register) -> Result<u32, ProcessorError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            Ok(self.registers[ind])
        } else {
            Err(ProcessorError::UnknownRegister(ind))
        }
    }

    pub fn set(&mut self, reg: Register, val: u32) -> Result<(), ProcessorError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            self.registers[ind] = val;
            Ok(())
        } else {
            Err(ProcessorError::UnknownRegister(ind))
        }
    }

    pub fn reset(&mut self) {
        self.registers.fill(0);
    }
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self {
            registers: [0; Self::REGISTER_COUNT],
        }
    }
}

impl Register {
    pub fn get_index(&self) -> usize {
        match self {
            Self::ProgramCounter => 0,
            Self::Status => 1,
            Self::StackPointer => 2,
            Self::Overflow => 3,
            Self::Return => 4,
            Self::ArgumentBase => 5,
            Self::GeneralPurpose(num) => *num,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResetType {
    Hard,
    Soft,
}

pub enum DataType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    F32,
}

pub struct DataTypeError(u8);

impl TryFrom<u8> for DataType {
    type Error = DataTypeError;

    fn try_from(val: u8) -> Result<Self, Self::Error> {
        Ok(match val {
            0 => Self::U8,
            1 => Self::I8,
            2 => Self::U16,
            3 => Self::I16,
            4 => Self::U32,
            5 => Self::I32,
            6 => Self::F32,
            _ => return Err(DataTypeError(val)),
        })
    }
}

impl From<DataType> for u8 {
    fn from(value: DataType) -> Self {
        match value {
            DataType::U8 => 0,
            DataType::I8 => 1,
            DataType::U16 => 2,
            DataType::I16 => 3,
            DataType::U32 => 4,
            DataType::I32 => 5,
            DataType::F32 => 6,
        }
    }
}

// Bit Formatting:
// | 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10  9  8 |  7  6  5 |  4  3  2  1  0 |
// | Opcode                  | Arg 0                     | Arg 1                     | Arg 2                     |
// | Opcode                  | DType 0  | Arg 0 (Reg)    | DType 1  | Arg 1 (Reg)    | DType 2  | Arg 2 (Reg)    |
// | Opcode                  | ImmTodo  | Immediate Value                                                        |
//   ^ TODO - Split load into 2 instructions to get full immediate value (signed, unsigned)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    data: [u8; 4],
}

impl Instruction {
    const NUM_IMM_BITS: u32 = {
        u8::BITS * 3
    };

    const IMM_SIGN_BIT: u32 = {
        1 << (Self::NUM_IMM_BITS - 1)
    };

    const IMM_NEG_MASK: u32 = {
        let mut val = 0;
        let mut i = Self::NUM_IMM_BITS;
        while i < u32::BITS {
            val |= i << i;
            i += 1;
        }
        val
    };

    pub fn opcode(&self) -> u8 {
        self.data[0]
    }

    fn reg_from_arg(arg: u8) -> Register {
        Register::GeneralPurpose((arg & 0x1F) as usize)
    }

    fn dt_from_arg(arg: u8) -> Result<DataType, DataTypeError>
    {
        let dt = (arg >> 5) & 7;
        DataType::try_from(dt)
    }

    pub fn arg0(&self) -> u8
    {
        self.data[1]
    }

    pub fn arg0_data_type(&self) -> Result<DataType, DataTypeError>
    {
        Self::dt_from_arg(self.arg0())
    }

    pub fn arg0_register(&self) -> Register
    {
        Self::reg_from_arg(self.arg0())
    }

    pub fn arg1(&self) -> u8
    {
        self.data[2]
    }

    pub fn arg1_data_type(&self) -> Result<DataType, DataTypeError>
    {
        Self::dt_from_arg(self.arg1())
    }

    pub fn arg1_register(&self) -> Register
    {
        Self::reg_from_arg(self.arg1())
    }

    pub fn arg2(&self) -> u8
    {
        self.data[3]
    }

    pub fn arg2_data_type(&self) -> Result<DataType, DataTypeError>
    {
        Self::dt_from_arg(self.arg2())
    }

    pub fn arg2_register(&self) -> Register
    {
        Self::reg_from_arg(self.arg2())
    }

    pub fn imm_unsigned(&self) -> u32
    {
        ((self.arg0() as u32) << 16) | ((self.arg1() as u32) << 8) | (self.arg2() as u32)
    }

    pub fn imm_signed(&self) -> i32
    {
        let mut val = self.imm_unsigned();
        if (val & Self::IMM_SIGN_BIT) != 0
        {
            val |= Self::IMM_NEG_MASK;
        }
        val as i32
    }
}

pub struct Processor {
    memory: MemoryMap,
    registers: RegisterManager,
}

impl Processor {
    pub const HARD_RESET_VECTOR: usize = 0;
    pub const SOFT_RESET_VECTOR: usize = 1;

    pub fn reset(&mut self, reset_type: ResetType) -> Result<(), ProcessorError> {
        if ResetType::Hard == reset_type {
            self.memory.reset();
        }

        let reset_vec = match reset_type {
            ResetType::Hard => Self::HARD_RESET_VECTOR,
            ResetType::Soft => Self::SOFT_RESET_VECTOR,
        };

        self.registers.reset();
        self.registers.set(Register::ProgramCounter, self.memory.get_u32(self.get_reset_vector(reset_vec)?)?)
    }

    pub fn trigger_interrupt(&mut self, interrupt_num: usize) -> Result<(), ProcessorError> {
        Err(ProcessorError::UnsupportedInterrupt(interrupt_num))
    }

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        let mut inst_jump = 1;

        let pc = self.registers.get(Register::ProgramCounter)?;

        let inst = Instruction {
            data: [
                self.memory.get_u8(pc)?.get(),
                self.memory.get_u8(pc + 1)?.get(),
                self.memory.get_u8(pc + 2)?.get(),
                self.memory.get_u8(pc + 3)?.get()]
        };

        const OP_BASE_CPU: u8 = 0;
        const OP_TYPE_CPU_NOOP: u8 = 0;
        const OP_TYPE_CPU_RESET: u8 = 1;
        const OP_TYPE_CPU_INTERRUPT: u8 = 2;
        const OP_TYPE_CPU_INTERRUPT_REGISTER: u8 = 3;
        const OP_TYPE_CPU_INTERRUPT_RETURN: u8 = 4;
        const OP_TYPE_CPU_CALL: u8 = 5;
        const OP_TYPE_CPU_RETURN: u8 = 6;
        const OP_TYPE_CPU_PUSH: u8 = 7;
        const OP_TYPE_CPU_POP: u8 = 8;
        const OP_TYPE_CPU_POP_REG: u8 = 9;
        const OP_TYPE_CPU_JUMP: u8 = 10;
        const OP_TYPE_CPU_JUMP_REL: u8 = 11;
        const OP_TYPE_CPU_JUMP_REL_IMM: u8 = 12;
        const OP_TYPE_CPU_BOOL: u8 = 13;
        const OP_TYPE_CPU_HALT: u8 = 15;

        // TODO - Add, Sub, Mul, Div, Shift, ArithShift, Mod, Eq, NEq, Lt, Gt, Lteq, Gteq, Conv, i32->f, u32->f, f->i32), load, save

        // TYPES: f32, i32, u32, i16, u16, i8, u8

        let opcode_base = (inst.opcode() >> 4) & 0xF;
        let opcode_type = inst.opcode() & 0xF;

        match opcode_base {
            OP_BASE_CPU => match opcode_type {
                OP_TYPE_CPU_NOOP => (),
                OP_TYPE_CPU_RESET => self.reset(ResetType::Soft)?,
                OP_TYPE_CPU_INTERRUPT => self.trigger_interrupt(inst.arg0() as usize)?,
                OP_TYPE_CPU_INTERRUPT_REGISTER => self.trigger_interrupt(self.registers.get(inst.arg0_register())? as usize)?,
                OP_TYPE_CPU_CALL => {
                    for i in 0..RegisterManager::REGISTER_COUNT {
                        self.stack_push(self.registers.get(Register::GeneralPurpose(i))?)?;
                    }
                    self.registers.set(Register::ProgramCounter, self.registers.get(inst.arg0_register())?)?;
                }
                OP_TYPE_CPU_RETURN | OP_TYPE_CPU_INTERRUPT_RETURN => {
                    for i in (0..RegisterManager::REGISTER_COUNT).rev() {
                        let val = self.stack_pop()?;
                        if i != Register::Return.get_index() || opcode_type == OP_TYPE_CPU_INTERRUPT_RETURN {
                            self.registers.set(Register::GeneralPurpose(i), val)?;
                        }
                    }
                }
                OP_TYPE_CPU_PUSH => {
                    let val = self.registers.get(inst.arg0_register())?;
                    self.stack_push(val)?;
                }
                OP_TYPE_CPU_POP => {
                    for _ in 0..inst.arg0() {
                        self.stack_pop()?;
                    }
                }
                OP_TYPE_CPU_POP_REG => {
                    let val = self.stack_pop()?;
                    self.registers.set(inst.arg0_register(), val)?;
                },
                OP_TYPE_CPU_JUMP => {
                    self.registers.set(Register::ProgramCounter, self.registers.get(inst.arg0_register())?)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL => {
                    self.registers.set(Register::ProgramCounter, (pc as i32 + self.registers.get(inst.arg0_register())? as i32) as u32)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL_IMM => {
                    self.registers.set(Register::ProgramCounter, (pc as i32 + inst.imm_signed()) as u32)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_BOOL => {
                    let val = self.registers.get(inst.arg1_register())?;
                    self.registers.set(inst.arg0_register(), if val != 0 { 1 } else { 0 })?;
                }
                OP_TYPE_CPU_HALT => {
                    inst_jump = 0;
                }
                _ => return Err(ProcessorError::UnknownOperation(inst)),
            }
            _ => return Err(ProcessorError::UnknownOperation(inst)),
        }

        self.registers.set(Register::ProgramCounter, inst_jump * 4)?;

        Ok(())
    }

    fn stack_push(&mut self, val: u32) -> Result<(), ProcessorError> {
        let sp_curr = self.registers.get(Register::StackPointer)?;
        self.memory.set_u32(sp_curr, val)?;
        self.registers.set(Register::StackPointer, sp_curr + 4)?;
        Ok(())
    }

    fn stack_pop(&mut self) -> Result<u32, ProcessorError> {
        let sp_curr = self.registers.get(Register::StackPointer)?;

        if sp_curr < 4 {
            return Err(ProcessorError::StackUnderflow);
        }

        let sp_curr = sp_curr - 4;
        self.registers.set(Register::StackPointer, sp_curr)?;

        Ok(self.memory.get_u32(sp_curr)?)
    }

    pub fn get_reset_vector(&self, index: usize) -> Result<u32, ProcessorError> {
        if index < 64 {
            Ok(index as u32 * 4)
        } else {
            Err(ProcessorError::UnsupportedInterrupt(index))
        }
    }
}
