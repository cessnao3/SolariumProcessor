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
pub enum RegisterMap {
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

    pub fn get(&self, reg: RegisterMap) -> Result<u32, ProcessorError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            Ok(self.registers[ind])
        } else {
            Err(ProcessorError::UnknownRegister(ind))
        }
    }

    pub fn set(&mut self, reg: RegisterMap, val: u32) -> Result<(), ProcessorError> {
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

impl RegisterMap {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    opcode: u8,
    arg0: u8,
    arg1: u8,
    arg2: u8,
}

impl Instruction {
    fn get_op_base(&self) -> u8 {
        (self.opcode >> 4) & 0xF
    }

    fn get_op_type(&self) -> u8 {
        self.opcode & 0xF
    }

    fn reg_a0(&self) -> RegisterMap {
        RegisterMap::GeneralPurpose(self.arg0 as usize)
    }

    fn reg_a1(&self) -> RegisterMap {
        RegisterMap::GeneralPurpose(self.arg0 as usize)
    }

    fn reg_a2(&self) -> RegisterMap {
        RegisterMap::GeneralPurpose(self.arg0 as usize)
    }

    fn arg_imm_signed(&self) -> i32 {
        let mut val = self.arg_imm_unsigned() as i32;
        if (self.arg0 & 0x80) != 0 {
            val |= 0xFF << 24;
        }
        val
    }

    fn arg_imm_unsigned(&self) -> u32 {
        ((self.arg0 as u32) << 16) | ((self.arg1 as u32) << 8) | (self.arg2 as u32)
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
        self.registers.set(RegisterMap::ProgramCounter, self.memory.get_u32(self.get_reset_vector(reset_vec)?)?)
    }

    pub fn trigger_interrupt(&mut self, interrupt_num: usize) -> Result<(), ProcessorError> {
        Err(ProcessorError::UnsupportedInterrupt(interrupt_num))
    }

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        let mut inst_jump = 1;

        let pc = self.registers.get(RegisterMap::ProgramCounter)?;

        let inst = Instruction {
            opcode: self.memory.get_u8(pc)?.get(),
            arg0: self.memory.get_u8(pc + 1)?.get(),
            arg1: self.memory.get_u8(pc + 2)?.get(),
            arg2: self.memory.get_u8(pc + 3)?.get(),
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

        // TODO - Add, Sub, Mul, Div, Shift, ArithShift, Mod, Eq, NEq, Lt, Gt, Lteq, Gteq, Conv, i32->f, u32->f, f->i32)

        // TYPES: f32, i32, u32, i16, u16, i8, u8

        match inst.get_op_base() {
            OP_BASE_CPU => match inst.get_op_type() {
                OP_TYPE_CPU_NOOP => (),
                OP_TYPE_CPU_RESET => self.reset(ResetType::Soft)?,
                OP_TYPE_CPU_INTERRUPT => self.trigger_interrupt(inst.arg0 as usize)?,
                OP_TYPE_CPU_INTERRUPT_REGISTER => self.trigger_interrupt(self.registers.get(inst.reg_a0())? as usize)?,
                OP_TYPE_CPU_CALL => {
                    for i in 0..RegisterManager::REGISTER_COUNT {
                        self.stack_push(self.registers.get(RegisterMap::GeneralPurpose(i))?)?;
                    }
                    self.registers.set(RegisterMap::ProgramCounter, self.registers.get(inst.reg_a0())?)?;
                }
                OP_TYPE_CPU_RETURN | OP_TYPE_CPU_INTERRUPT_RETURN => {
                    for i in (0..RegisterManager::REGISTER_COUNT).rev() {
                        let val = self.stack_pop()?;
                        if i != RegisterMap::Return.get_index() || inst.get_op_type() == OP_TYPE_CPU_INTERRUPT_RETURN {
                            self.registers.set(RegisterMap::GeneralPurpose(i), val)?;
                        }
                    }
                }
                OP_TYPE_CPU_PUSH => {
                    let val = self.registers.get(inst.reg_a0())?;
                    self.stack_push(val)?;
                }
                OP_TYPE_CPU_POP => {
                    for _ in 0..inst.arg0 {
                        self.stack_pop()?;
                    }
                }
                OP_TYPE_CPU_POP_REG => {
                    let val = self.stack_pop()?;
                    self.registers.set(inst.reg_a0(), val)?;
                },
                OP_TYPE_CPU_JUMP => {
                    self.registers.set(RegisterMap::ProgramCounter, self.registers.get(inst.reg_a0())?)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL => {
                    self.registers.set(RegisterMap::ProgramCounter, (pc as i32 + self.registers.get(inst.reg_a0())? as i32) as u32)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL_IMM => {
                    self.registers.set(RegisterMap::ProgramCounter, (pc as i32 + inst.arg_imm_signed()) as u32)?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_BOOL => {
                    let val = self.registers.get(inst.reg_a1())?;
                    self.registers.set(inst.reg_a0(), if val != 0 { 1 } else { 0 })?;
                }
                OP_TYPE_CPU_HALT => {
                    inst_jump = 0;
                }
                _ => return Err(ProcessorError::UnknownOperation(inst)),
            }
            _ => return Err(ProcessorError::UnknownOperation(inst)),
        }

        self.registers.set(RegisterMap::ProgramCounter, inst_jump * 4)?;

        Ok(())
    }

    fn stack_push(&mut self, val: u32) -> Result<(), ProcessorError> {
        let sp_curr = self.registers.get(RegisterMap::StackPointer)?;
        self.memory.set_u32(sp_curr, val)?;
        self.registers.set(RegisterMap::StackPointer, sp_curr + 4)?;
        Ok(())
    }

    fn stack_pop(&mut self) -> Result<u32, ProcessorError> {
        let sp_curr = self.registers.get(RegisterMap::StackPointer)?;

        if sp_curr < 4 {
            return Err(ProcessorError::StackUnderflow);
        }

        let sp_curr = sp_curr - 4;
        self.registers.set(RegisterMap::StackPointer, sp_curr)?;

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
