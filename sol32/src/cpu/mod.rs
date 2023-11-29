mod instruction;
mod operations;
mod register;

use crate::cpu::instruction::DataType;
use crate::memory::{MemoryError, MemoryMap};

use self::instruction::{Instruction, DataTypeError};
use self::operations::{ArithmeticOperations, BinaryOperations, FloatOperations, IntegerU8Operations, IntegerU16Operations, IntegerU32Operations, IntegerI8Operations, IntegerI16Operations, IntegerI32Operations, OperationError};
use self::register::{Register, RegisterManager, RegisterError};

pub enum ProcessorError {
    Memory(MemoryError),
    UnsupportedInterrupt(usize),
    Register(RegisterError),
    UnknownInstruction(Instruction),
    Operation(OperationError),
    StackUnderflow,
    DataType(DataTypeError),
}

impl From<MemoryError> for ProcessorError {
    fn from(value: MemoryError) -> Self {
        Self::Memory(value)
    }
}

impl From<RegisterError> for ProcessorError {
    fn from(value: RegisterError) -> Self {
        Self::Register(value)
    }
}

impl From<DataTypeError> for ProcessorError {
    fn from(value: DataTypeError) -> Self {
        Self::DataType(value)
    }
}

impl From<OperationError> for ProcessorError {
    fn from(value: OperationError) -> Self {
        Self::Operation(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResetType {
    Hard,
    Soft,
}

pub struct Processor {
    memory: MemoryMap,
    registers: RegisterManager,
    op_f32: FloatOperations,
    op_u8: IntegerU8Operations,
    op_u16: IntegerU16Operations,
    op_u32: IntegerU32Operations,
    op_i8: IntegerI8Operations,
    op_i16: IntegerI16Operations,
    op_i32: IntegerI32Operations,
}

impl Processor {
    pub const HARD_RESET_VECTOR: usize = 0;
    pub const SOFT_RESET_VECTOR: usize = 1;

    pub fn new() -> Self {
        Self {
            memory: MemoryMap::default(),
            registers: RegisterManager::default(),
            op_f32: FloatOperations,
            op_u8: IntegerU8Operations,
            op_u16: IntegerU16Operations,
            op_u32: IntegerU32Operations,
            op_i8: IntegerI8Operations,
            op_i16: IntegerI16Operations,
            op_i32: IntegerI32Operations,
        }
    }

    pub fn reset(&mut self, reset_type: ResetType) -> Result<(), ProcessorError> {
        if ResetType::Hard == reset_type {
            self.memory.reset();
        }

        let reset_vec = match reset_type {
            ResetType::Hard => Self::HARD_RESET_VECTOR,
            ResetType::Soft => Self::SOFT_RESET_VECTOR,
        };

        self.registers.reset();
        self.registers.set(
            Register::ProgramCounter,
            self.memory.get_u32(self.get_reset_vector(reset_vec)?)?,
        )?;

        Ok(())
    }

    pub fn trigger_interrupt(&mut self, interrupt_num: usize) -> Result<(), ProcessorError> {
        Err(ProcessorError::UnsupportedInterrupt(interrupt_num))
    }

    fn get_arith_operation(&self, dt: DataType) -> Result<&dyn ArithmeticOperations, ProcessorError> {
        Ok(match dt {
            DataType::U8 => &self.op_u8,
            DataType::U16 => &self.op_u16,
            DataType::U32 => &self.op_u32,
            DataType::I8 => &self.op_i8,
            DataType::I16 => &self.op_i16,
            DataType::I32 => &self.op_i32,
            DataType::F32 => &self.op_f32,
        })
    }

    fn get_bitwise_operation(&self, dt: DataType) -> Result<&dyn BinaryOperations, ProcessorError> {
        Ok(match dt {
            DataType::U8 => &self.op_u8,
            DataType::U16 => &self.op_u16,
            DataType::U32 => &self.op_u32,
            DataType::I8 => &self.op_i8,
            DataType::I16 => &self.op_i16,
            DataType::I32 => &self.op_i32,
            _ => return Err(ProcessorError::Operation(OperationError::UnuspportedOperation)),
        })
    }

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        let mut inst_jump = 1;

        let pc = self.registers.get(Register::ProgramCounter)?;

        let inst = Instruction::new([
            self.memory.get_u8(pc)?.get(),
            self.memory.get_u8(pc + 1)?.get(),
            self.memory.get_u8(pc + 2)?.get(),
            self.memory.get_u8(pc + 3)?.get(),
        ]);

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

        const OP_MATH: u8 = 10;
        const OP_MATH_ADD: u8 = 0;
        const OP_MATH_SUB: u8 = 1;
        const OP_MATH_MUL: u8 = 2;
        const OP_MATH_DIV: u8 = 3;
        const OP_MATH_REM: u8 = 4;

        const OP_BITS: u8 = 11;
        const OP_BITS_AND: u8 = 0;
        const OP_BITS_OR: u8 = 1;
        const OP_BITS_XOR: u8 = 2;
        const OP_BITS_SHL: u8 = 3;
        const OP_BITS_SHR: u8 = 4;

        // TODO - Load
        // TODO - LoadN
        // TODO - Save

        // TODO - Add, Sub, Mul, Div, Shift, ArithShift, Mod, Eq, NEq, Lt, Gt, Lteq, Gteq, Conv, i32->f, u32->f, f->i32), load, save

        let opcode_base = (inst.opcode() >> 4) & 0xF;
        let opcode_type = inst.opcode() & 0xF;

        match opcode_base {
            OP_BASE_CPU => match opcode_type {
                OP_TYPE_CPU_NOOP => (),
                OP_TYPE_CPU_RESET => self.reset(ResetType::Soft)?,
                OP_TYPE_CPU_INTERRUPT => self.trigger_interrupt(inst.arg0() as usize)?,
                OP_TYPE_CPU_INTERRUPT_REGISTER => {
                    self.trigger_interrupt(self.registers.get(inst.arg0_register())? as usize)?
                }
                OP_TYPE_CPU_CALL => {
                    for i in 0..RegisterManager::REGISTER_COUNT {
                        self.stack_push(self.registers.get(Register::GeneralPurpose(i))?)?;
                    }
                    self.registers.set(
                        Register::ProgramCounter,
                        self.registers.get(inst.arg0_register())?,
                    )?;
                }
                OP_TYPE_CPU_RETURN | OP_TYPE_CPU_INTERRUPT_RETURN => {
                    for i in (0..RegisterManager::REGISTER_COUNT).rev() {
                        let val = self.stack_pop()?;
                        if i != Register::Return.get_index()
                            || opcode_type == OP_TYPE_CPU_INTERRUPT_RETURN
                        {
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
                }
                OP_TYPE_CPU_JUMP => {
                    self.registers.set(
                        Register::ProgramCounter,
                        self.registers.get(inst.arg0_register())?,
                    )?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL => {
                    self.registers.set(
                        Register::ProgramCounter,
                        (pc as i32 + self.registers.get(inst.arg0_register())? as i32) as u32,
                    )?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_JUMP_REL_IMM => {
                    self.registers.set(
                        Register::ProgramCounter,
                        (pc as i32 + inst.imm_signed()) as u32,
                    )?;
                    inst_jump = 0;
                }
                OP_TYPE_CPU_BOOL => {
                    let val = self.registers.get(inst.arg1_register())?;
                    self.registers
                        .set(inst.arg0_register(), if val != 0 { 1 } else { 0 })?;
                }
                OP_TYPE_CPU_HALT => {
                    inst_jump = 0;
                }
                _ => return Err(ProcessorError::UnknownInstruction(inst)),
            },
            OP_MATH => {
                let arith = self.get_arith_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode_type {
                    OP_MATH_ADD => arith.add(val_a, val_b)?,
                    OP_MATH_SUB => arith.sub(val_a, val_b)?,
                    OP_MATH_MUL => arith.mul(val_a, val_b)?,
                    OP_MATH_DIV => arith.div(val_a, val_b)?,
                    OP_MATH_REM => arith.rem(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers.set(Register::Status, if res.carry { 1 } else { 0 })?;
            },
            OP_BITS => {
                let bitwise = self.get_bitwise_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode_type {
                    OP_BITS_AND => bitwise.band(val_a, val_b)?,
                    OP_BITS_OR => bitwise.bor(val_a, val_b)?,
                    OP_BITS_XOR => bitwise.bxor(val_a, val_b)?,
                    OP_BITS_SHL => bitwise.bsftl(val_a, val_b)?,
                    OP_BITS_SHR => bitwise.bsftr(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers.set(Register::Status, if res.carry { 1 } else { 0 })?;
            },
            _ => return Err(ProcessorError::UnknownInstruction(inst)),
        }

        self.registers
            .set(Register::ProgramCounter, inst_jump * 4)?;

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

impl Default for Processor {
    fn default() -> Self {
        Self::new()
    }
}
