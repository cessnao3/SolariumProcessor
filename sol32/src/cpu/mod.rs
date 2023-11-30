mod instruction;
mod operations;
mod register;

use crate::cpu::instruction::DataType;
use crate::memory::{MemoryError, MemoryMap};

use self::instruction::{DataTypeError, Instruction};
use self::operations::{
    ArithmeticOperations, BinaryOperations, FloatOperations, IntegerI16Operations,
    IntegerI32Operations, IntegerI8Operations, IntegerU16Operations, IntegerU32Operations,
    IntegerU8Operations, OperationError,
};
use self::register::{Register, RegisterError, RegisterManager};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OpcodeCombination {
    base: u8,
    code: u8,
}

impl From<u8> for OpcodeCombination {
    fn from(value: u8) -> Self {
        Self {
            base: (value >> 4) & 0xF,
            code: value & 0xF,
        }
    }
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

    fn get_arith_operation(
        &self,
        dt: DataType,
    ) -> Result<&dyn ArithmeticOperations, ProcessorError> {
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
            _ => {
                return Err(ProcessorError::Operation(
                    OperationError::UnuspportedOperation,
                ))
            }
        })
    }

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        let mut inst_jump = 1;

        let pc = self.registers.get(Register::ProgramCounter)?;

        let inst = Instruction::new([
            self.memory.get_u8(pc)?,
            self.memory.get_u8(pc + 1)?,
            self.memory.get_u8(pc + 2)?,
            self.memory.get_u8(pc + 3)?,
        ]);

        let opcode = OpcodeCombination::from(inst.opcode());

        const OP_BASE_CPU: u8 = 0;
        const OP_NOOP: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 0,
        };
        const OP_RESET: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 1,
        };
        const OP_INTERRUPT: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 2,
        };
        const OP_INTERRUPT_REGISTER: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 3,
        };
        const OP_INTERRUPT_RETURN: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 4,
        };
        const OP_CALL: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 5,
        };
        const OP_RETURN: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 6,
        };
        const OP_PUSH: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 7,
        };
        const OP_POP: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 8,
        };
        const OP_POP_REG: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 9,
        };
        const OP_JUMP: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 10,
        };
        const OP_JUMP_REL: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 11,
        };
        const OP_JUMP_REL_IMM: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 12,
        };
        const OP_BOOL: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 13,
        };
        const OP_HALT: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_CPU,
            code: 15,
        };

        const OP_BASE_MEM: u8 = 1;
        const OP_LOAD: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MEM,
            code: 0,
        };
        const OP_SAVE: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MEM,
            code: 1,
        };
        const OP_LOAD_IMM: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MEM,
            code: 2,
        };

        const OP_BASE_MATH: u8 = 10;
        const OP_ADD: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MATH,
            code: 0,
        };
        const OP_SUB: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MATH,
            code: 1,
        };
        const OP_MUL: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MATH,
            code: 2,
        };
        const OP_DIV: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MATH,
            code: 3,
        };
        const OP_REM: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_MATH,
            code: 4,
        };

        const OP_BASE_BITS: u8 = 11;
        const OP_BAND: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_BITS,
            code: 0,
        };
        const OP_BOR: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_BITS,
            code: 1,
        };
        const OP_BXOR: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_BITS,
            code: 2,
        };
        const OP_BSHL: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_BITS,
            code: 3,
        };
        const OP_BSHR: OpcodeCombination = OpcodeCombination {
            base: OP_BASE_BITS,
            code: 4,
        };

        match opcode {
            OP_NOOP => (),
            OP_RESET => self.reset(ResetType::Soft)?,
            OP_INTERRUPT => self.trigger_interrupt(inst.arg0() as usize)?,
            OP_INTERRUPT_REGISTER => {
                self.trigger_interrupt(self.registers.get(inst.arg0_register())? as usize)?
            }
            OP_CALL => {
                for i in 0..RegisterManager::REGISTER_COUNT {
                    self.stack_push(self.registers.get(Register::GeneralPurpose(i))?)?;
                }
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
            }
            OP_RETURN | OP_INTERRUPT_RETURN => {
                for i in (0..RegisterManager::REGISTER_COUNT).rev() {
                    let val = self.stack_pop()?;
                    if i != Register::Return.get_index() || opcode == OP_INTERRUPT_RETURN {
                        self.registers.set(Register::GeneralPurpose(i), val)?;
                    }
                }
            }
            OP_PUSH => {
                let val = self.registers.get(inst.arg0_register())?;
                self.stack_push(val)?;
            }
            OP_POP => {
                for _ in 0..inst.arg0() {
                    self.stack_pop()?;
                }
            }
            OP_POP_REG => {
                let val = self.stack_pop()?;
                self.registers.set(inst.arg0_register(), val)?;
            }
            OP_JUMP => {
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
                inst_jump = 0;
            }
            OP_JUMP_REL => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + self.registers.get(inst.arg0_register())? as i32) as u32,
                )?;
                inst_jump = 0;
            }
            OP_JUMP_REL_IMM => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + inst.imm_signed()) as u32,
                )?;
                inst_jump = 0;
            }
            OP_BOOL => {
                let val = self.registers.get(inst.arg1_register())?;
                self.registers
                    .set(inst.arg0_register(), if val != 0 { 1 } else { 0 })?;
            }
            OP_HALT => {
                inst_jump = 0;
            }
            OP_LOAD => {
                let dt = inst.arg0_data_type()?;
                if dt.signed() {
                    match dt.word_size() {
                        1 => self.registers.set(
                            inst.arg0_register(),
                            (self
                                .memory
                                .get_u8(self.registers.get(inst.arg1_register())?)?
                                as i32) as u32,
                        )?,
                        2 => self.registers.set(
                            inst.arg0_register(),
                            (self
                                .memory
                                .get_u16(self.registers.get(inst.arg1_register())?)?
                                as i32) as u32,
                        )?,
                        4 => self.registers.set(
                            inst.arg0_register(),
                            self.memory
                                .get_u32(self.registers.get(inst.arg1_register())?)?,
                        )?,
                        _ => return Err(ProcessorError::UnknownInstruction(inst)),
                    }
                } else {
                    match dt.word_size() {
                        1 => self.registers.set(
                            inst.arg0_register(),
                            self.memory
                                .get_u8(self.registers.get(inst.arg1_register())?)?
                                as u32,
                        )?,
                        2 => self.registers.set(
                            inst.arg0_register(),
                            self.memory
                                .get_u16(self.registers.get(inst.arg1_register())?)?
                                as u32,
                        )?,
                        4 => self.registers.set(
                            inst.arg0_register(),
                            self.memory
                                .get_u32(self.registers.get(inst.arg1_register())?)?,
                        )?,
                        _ => return Err(ProcessorError::UnknownInstruction(inst)),
                    }
                }
            }
            OP_SAVE => {
                let dt = inst.arg0_data_type()?;
                match dt.word_size() {
                    1 => self.memory.set_u8(
                        self.registers.get(inst.arg0_register())?,
                        (self.registers.get(inst.arg1_register())? & 0xFF) as u8,
                    )?,
                    2 => self.memory.set_u16(
                        self.registers.get(inst.arg0_register())?,
                        (self.registers.get(inst.arg1_register())? & 0xFFFF) as u16,
                    )?,
                    4 => self.memory.set_u32(
                        self.registers.get(inst.arg0_register())?,
                        self.registers.get(inst.arg1_register())?,
                    )?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };
            }
            OP_LOAD_IMM => {
                let dt = inst.arg0_data_type()?;
                match dt {
                    DataType::I16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_signed() as u32)?,
                    DataType::U16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_unsigned())?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                }
            }
            OpcodeCombination {
                base: OP_BASE_MATH, ..
            } => {
                let arith = self.get_arith_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    OP_ADD => arith.add(val_a, val_b)?,
                    OP_SUB => arith.sub(val_a, val_b)?,
                    OP_MUL => arith.mul(val_a, val_b)?,
                    OP_DIV => arith.div(val_a, val_b)?,
                    OP_REM => arith.rem(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set(Register::Status, if res.carry { 1 } else { 0 })?;
            }
            OpcodeCombination {
                base: OP_BASE_BITS, ..
            } => {
                let bitwise = self.get_bitwise_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    OP_BAND => bitwise.band(val_a, val_b)?,
                    OP_BOR => bitwise.bor(val_a, val_b)?,
                    OP_BXOR => bitwise.bxor(val_a, val_b)?,
                    OP_BSHL => bitwise.bsftl(val_a, val_b)?,
                    OP_BSHR => bitwise.bsftr(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set(Register::Status, if res.carry { 1 } else { 0 })?;
            }
            _ => return Err(ProcessorError::UnknownInstruction(inst)),
        };

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
