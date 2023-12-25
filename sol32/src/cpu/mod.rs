mod instruction;
mod operations;
mod register;

use core::fmt;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub use crate::cpu::instruction::{DataType, DataTypeError};
use crate::device::{DeviceAction, ProcessorDevice};
use crate::memory::{MemoryError, MemoryMap, MemorySegment};

use self::instruction::Instruction;
use self::operations::{
    ArithmeticOperations, BinaryOperations, FloatOperations, IntegerI16Operations,
    IntegerI32Operations, IntegerI8Operations, IntegerU16Operations, IntegerU32Operations,
    IntegerU8Operations, OperationError, RelationalOperations,
};

use self::register::RegisterFlag;
pub use self::register::{Register, RegisterError, RegisterManager};

#[derive(Debug, Clone)]
pub enum ProcessorError {
    Memory(MemoryError),
    UnsupportedInterrupt(u32),
    Register(RegisterError),
    UnknownInstruction(Instruction),
    UnsupportedDataType(Instruction, DataType),
    Operation(OperationError),
    StackUnderflow,
    DataType(DataTypeError),
    OpcodeAlignment(u32),
}

impl fmt::Display for ProcessorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Memory(m) => write!(f, "Memory Error => {m}"),
            Self::DataType(dt) => write!(f, "Data Type Error => {dt}"),
            Self::UnsupportedInterrupt(i) => write!(f, "Unsupported Interrupt => {i}"),
            Self::Register(r) => write!(f, "Register Error => {r}"),
            Self::UnknownInstruction(i) => write!(f, "Unknown Instruction => {i}"),
            Self::UnsupportedDataType(i, dt) => {
                write!(f, "Unsupported Data Type {dt} for Instruction {i}")
            }
            Self::Operation(o) => write!(f, "Operation Error => {o}"),
            Self::StackUnderflow => write!(f, "Stack Underflow"),
            Self::OpcodeAlignment(o) => write!(f, "Opcode Alignment Error => 0x{o:08x}"),
        }
    }
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
pub struct Opcode {
    base: u8,
    code: u8,
}

impl Opcode {
    pub fn to_byte(&self) -> u8 {
        ((self.base & 0xF) << 4) | (self.code & 0xF)
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} ({})", self.base, self.code, self.to_byte())
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        Self {
            base: (value >> 4) & 0xF,
            code: value & 0xF,
        }
    }
}

impl Hash for Opcode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.code.hash(state);
    }
}

pub struct Processor {
    memory: MemoryMap,
    devices: Vec<Rc<RefCell<dyn ProcessorDevice>>>,
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
    /// Defines the number of bytes per memory address (size of the default memory word)
    pub const BYTES_PER_ADDRESS: u32 = std::mem::size_of::<u32>() as u32;

    /// Defines the hard reset vector number
    pub const HARD_RESET_VECTOR: u32 = 0;

    /// Defines the hard reset vector number
    pub const SOFT_RESET_VECTOR: u32 = Self::BYTES_PER_ADDRESS;

    /// Defines the number of supported interrupts
    pub const NUM_INTERRUPT: u32 = 32;

    ///Providesthe base address for the hardware interrupts
    pub const BASE_HW_INT_ADDR: u32 = 0x100;

    /// Provides the base address for the software interrupts
    pub const BASE_SW_INT_ADDR: u32 = Self::BYTES_PER_ADDRESS * Self::NUM_INTERRUPT;

    /// Provides the top address (next free address) after the vector memory segments
    pub const TOP_VEC_SEG_ADDR: u32 = Self::BASE_SW_INT_ADDR * Self::NUM_INTERRUPT;

    const OP_BASE_CPU: u8 = 0;
    pub const OP_NOOP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 0,
    };
    pub const OP_RESET: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 1,
    };
    pub const OP_INTERRUPT: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 2,
    };
    pub const OP_INTERRUPT_REGISTER: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 3,
    };
    pub const OP_INTERRUPT_RETURN: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 4,
    };
    pub const OP_CALL: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 5,
    };
    pub const OP_RETURN: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 6,
    };
    pub const OP_PUSH: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 7,
    };
    pub const OP_POP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 8,
    };
    pub const OP_POP_REG: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 9,
    };
    pub const OP_JUMP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 10,
    };
    pub const OP_JUMP_REL: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 11,
    };
    pub const OP_JUMP_REL_IMM: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 12,
    };
    pub const OP_HALT: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 15,
    };

    const OP_BASE_MEM: u8 = 1;
    pub const OP_LOAD: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 0,
    };
    pub const OP_LOAD_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 1,
    };
    pub const OP_LOAD_IMM: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 2,
    };
    pub const OP_LOAD_IMM_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 3,
    };
    pub const OP_LOAD_NEXT: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 4,
    };
    pub const OP_SAVE: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 5,
    };
    pub const OP_SAVE_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 6,
    };
    pub const OP_COPY: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 7,
    };
    pub const OP_CONV: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 8,
    };

    const OP_BASE_TEST: u8 = 2;
    pub const OP_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 0,
    };
    pub const OP_NEQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 1,
    };
    pub const OP_GREATER: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 2,
    };
    pub const OP_GREATER_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 3,
    };
    pub const OP_LESS: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 4,
    };
    pub const OP_LESS_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 5,
    };

    const OP_BASE_LOGIC: u8 = 3;
    pub const OP_NOT: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 0,
    };
    pub const OP_BOOL: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 1,
    };
    pub const OP_TEST_ZERO: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 2,
    };
    pub const OP_TEST_NOT_ZERO: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 3,
    };

    const OP_BASE_STATUS_FLAGS: u8 = 4;
    pub const OP_INTON: Opcode = Opcode {
        base: Self::OP_BASE_STATUS_FLAGS,
        code: 0,
    };
    pub const OP_INTOFF: Opcode = Opcode {
        base: Self::OP_BASE_STATUS_FLAGS,
        code: 1,
    };

    const OP_BASE_MATH: u8 = 10;
    pub const OP_ADD: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 0,
    };
    pub const OP_SUB: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 1,
    };
    pub const OP_MUL: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 2,
    };
    pub const OP_DIV: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 3,
    };
    pub const OP_REM: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 4,
    };

    const OP_BASE_BITS: u8 = 11;
    pub const OP_BAND: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 0,
    };
    pub const OP_BOR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 1,
    };
    pub const OP_BXOR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 2,
    };
    pub const OP_BSHL: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 3,
    };
    pub const OP_BSHR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 4,
    };

    pub fn new() -> Self {
        Self {
            memory: MemoryMap::default(),
            devices: Vec::new(),
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

        let reset_vec_addr = match reset_type {
            ResetType::Hard => Self::HARD_RESET_VECTOR,
            ResetType::Soft => Self::SOFT_RESET_VECTOR,
        };

        self.registers.reset();
        self.registers.set(
            Register::ProgramCounter,
            self.memory.get_u32(reset_vec_addr)?,
        )?;
        self.registers
            .set_flag(RegisterFlag::InterruptEnable, true)?;

        Ok(())
    }

    pub fn get_register_state(&self) -> RegisterManager {
        self.registers
    }

    pub fn hardware_interrupt(&mut self, interrupt_num: u32) -> Result<bool, ProcessorError> {
        self.call_interrupt(Self::BASE_HW_INT_ADDR + interrupt_num * Self::BYTES_PER_ADDRESS)
    }

    pub fn software_interrupt(&mut self, interrupt_num: u32) -> Result<bool, ProcessorError> {
        self.call_interrupt(Self::BASE_SW_INT_ADDR + interrupt_num * Self::BYTES_PER_ADDRESS)
    }

    fn call_interrupt(&mut self, interrupt_loc: u32) -> Result<bool, ProcessorError> {
        // Return false if interrupts are not allowed
        if !self.registers.get_flag(RegisterFlag::InterruptEnable)? {
            return Ok(false);
        }

        // Obtain the desired value from the program counter
        let new_pc = self.memory.get_u32(interrupt_loc)?;

        // Return false if the vector value is 0 (Disabled)
        if new_pc == 0 {
            return Ok(false);
        }

        // Push all register values to the stack
        self.push_all_registers()?;

        // Update the program counter to the value in the interrupt vector
        self.registers.set(Register::ProgramCounter, new_pc)?;

        // Return true if the interrupt was called
        Ok(true)
    }

    fn push_all_registers(&mut self) -> Result<(), ProcessorError> {
        let reg_vals = self.registers.get_state();

        for r in reg_vals {
            self.stack_push(r)?;
        }

        Ok(())
    }

    fn pop_all_registers(&mut self, save_return_register: bool) -> Result<(), ProcessorError> {
        let mut current_state = self.registers.get_state();

        for (i, v) in current_state.iter_mut().enumerate().rev() {
            let val = self.stack_pop()?;
            if !save_return_register || i != Register::Return.get_index() {
                *v = val;
            }
        }

        self.registers.set_state(current_state);

        Ok(())
    }

    pub fn memory_set(&mut self, address: u32, val: u8) -> Result<(), ProcessorError> {
        self.memory.set(address, val)?;
        Ok(())
    }

    pub fn memory_inspect(&self, address: u32) -> Result<u8, ProcessorError> {
        Ok(self.memory.inspect(address)?)
    }

    pub fn memory_inspect_u32(&self, address: u32) -> Result<u32, ProcessorError> {
        Ok(self.memory.inspect_u32(address)?)
    }

    pub fn memory_add_segment(
        &mut self,
        address: u32,
        seg: Rc<RefCell<dyn MemorySegment>>,
    ) -> Result<(), ProcessorError> {
        self.memory.add_segment(address, seg)?;
        Ok(())
    }

    pub fn device_add(
        &mut self,
        seg: Rc<RefCell<dyn ProcessorDevice>>,
    ) -> Result<(), ProcessorError> {
        self.devices.push(seg);
        Ok(())
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

    fn get_relative_operation(
        &self,
        dt: DataType,
    ) -> Result<&dyn RelationalOperations, ProcessorError> {
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

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        let mut inst_jump = Some(1);

        let pc = self.registers.get(Register::ProgramCounter)?;
        if pc % 4 != 0 {
            return Err(ProcessorError::OpcodeAlignment(pc));
        }

        let inst = Instruction::from(self.memory.get_u32(pc)?);

        let opcode = Opcode::from(inst.opcode());

        // TODO - Jump Condition

        match opcode {
            Self::OP_NOOP => (),
            Self::OP_RESET => self.reset(ResetType::Soft)?,
            Self::OP_INTON => self
                .registers
                .set_flag(RegisterFlag::InterruptEnable, true)?,
            Self::OP_INTOFF => self
                .registers
                .set_flag(RegisterFlag::InterruptEnable, false)?,
            Self::OP_INTERRUPT => {
                self.software_interrupt(inst.arg0() as u32)?;
                inst_jump = None;
            }
            Self::OP_INTERRUPT_REGISTER => {
                self.software_interrupt(self.registers.get(inst.arg0_register())?)?;
            }
            Self::OP_CALL => {
                // Increment the program counter before pushing registers so we return to the next instruction
                self.registers
                    .set(Register::ProgramCounter, pc + Self::BYTES_PER_ADDRESS)?;

                // Push all registers
                self.push_all_registers()?;

                // Set the new program counter and ensure that the next instruction
                // starts incrementing directly from the new value
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
                inst_jump = None;
            }
            Self::OP_RETURN | Self::OP_INTERRUPT_RETURN => {
                self.pop_all_registers(opcode == Self::OP_RETURN)?;
                inst_jump = None;
            }
            Self::OP_PUSH => {
                let val = self.registers.get(inst.arg0_register())?;
                self.stack_push(val)?;
            }
            Self::OP_POP => {
                for _ in 0..inst.arg0() {
                    self.stack_pop()?;
                }
            }
            Self::OP_POP_REG => {
                let val = self.stack_pop()?;
                self.registers.set(inst.arg0_register(), val)?;
            }
            Self::OP_JUMP => {
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
                inst_jump = None;
            }
            Self::OP_JUMP_REL => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + self.registers.get(inst.arg0_register())? as i32) as u32,
                )?;
                inst_jump = None;
            }
            Self::OP_JUMP_REL_IMM => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + inst.imm_signed()) as u32,
                )?;
                inst_jump = None;
            }
            Self::OP_HALT => {
                inst_jump = None;
            }
            Self::OP_NOT => {
                let val = self.registers.get(inst.arg1_register())?;
                self.registers
                    .set(inst.arg0_register(), if val != 0 { 0 } else { 1 })?;
            }
            Self::OP_BOOL => {
                let val = self.registers.get(inst.arg1_register())?;
                self.registers
                    .set(inst.arg0_register(), if val != 0 { 1 } else { 0 })?;
            }
            Self::OP_TEST_ZERO | Self::OP_TEST_NOT_ZERO => {
                let val = self.registers.get(inst.arg0_register())?;
                let is_true = match opcode {
                    Self::OP_TEST_ZERO => val == 0,
                    Self::OP_TEST_NOT_ZERO => val != 0,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                inst_jump = Some(if is_true { 1 } else { 2 });
            }
            Self::OP_LOAD_IMM => {
                let dt = inst.arg0_data_type()?;
                match dt {
                    DataType::I16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_signed() as u32)?,
                    DataType::U16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_unsigned())?,
                    _ => return Err(ProcessorError::UnsupportedDataType(inst, dt)),
                }
            }
            Self::OP_LOAD | Self::OP_LOAD_REL | Self::OP_LOAD_IMM_REL | Self::OP_LOAD_NEXT => {
                let dt = inst.arg0_data_type()?;
                let addr = match opcode {
                    Self::OP_LOAD => self.registers.get(inst.arg1_register())?,
                    Self::OP_LOAD_REL => {
                        self.registers.get(inst.arg1_register())?
                            + self.registers.get(Register::ProgramCounter)?
                    }
                    Self::OP_LOAD_IMM_REL => {
                        (self.registers.get(Register::ProgramCounter)? as i32 + inst.imm_signed())
                            as u32
                    }
                    Self::OP_LOAD_NEXT => {
                        inst_jump = Some(2);
                        pc + 4
                    }
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };
                let reg_target = inst.arg0_register();

                if dt.signed() {
                    match dt.word_size() {
                        1 => self
                            .registers
                            .set(reg_target, (self.memory.get(addr)? as i32) as u32)?,
                        2 => self
                            .registers
                            .set(reg_target, (self.memory.get_u16(addr)? as i32) as u32)?,
                        4 => self.registers.set(reg_target, self.memory.get_u32(addr)?)?,
                        _ => return Err(ProcessorError::UnknownInstruction(inst)),
                    }
                } else {
                    match dt.word_size() {
                        1 => self
                            .registers
                            .set(reg_target, self.memory.get(addr)? as u32)?,
                        2 => self
                            .registers
                            .set(reg_target, self.memory.get_u16(addr)? as u32)?,
                        4 => self.registers.set(reg_target, self.memory.get_u32(addr)?)?,
                        _ => return Err(ProcessorError::UnknownInstruction(inst)),
                    }
                }
            }
            Self::OP_SAVE | Self::OP_SAVE_REL => {
                let dt = inst.arg0_data_type()?;
                let source_reg = self.registers.get(inst.arg1_register())?;

                let addr = match opcode {
                    Self::OP_SAVE => self.registers.get(inst.arg0_register())?,
                    Self::OP_SAVE_REL => {
                        (inst.imm_signed() + self.registers.get(inst.arg0_register())? as i32)
                            as u32
                    }
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                match dt.word_size() {
                    1 => self.memory.set(addr, (source_reg & 0xFF) as u8)?,
                    2 => self.memory.set_u16(addr, (source_reg & 0xFFFF) as u16)?,
                    4 => self.memory.set_u32(addr, source_reg)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };
            }
            Self::OP_COPY => self.registers.set(
                inst.arg0_register(),
                self.registers.get(inst.arg1_register())?,
            )?,
            Self::OP_CONV => {
                let t_src = inst.arg1_data_type()?;
                let v_src = self.registers.get(inst.arg1_register())?;

                let t_dest = inst.arg0_data_type()?;

                let v_dest = match t_dest {
                    DataType::U8 => {
                        (match t_src {
                            DataType::U8 | DataType::U16 | DataType::U32 => v_src as u8,
                            DataType::I8 | DataType::I16 | DataType::I32 => (v_src as i32) as u8,
                            DataType::F32 => (f32::from_bits(v_src) as i32) as u8,
                        }) as u32
                    }
                    DataType::U16 => {
                        (match t_src {
                            DataType::U8 | DataType::U16 | DataType::U32 => v_src as u16,
                            DataType::I8 | DataType::I16 | DataType::I32 => (v_src as i32) as u16,
                            DataType::F32 => (f32::from_bits(v_src) as i32) as u16,
                        }) as u32
                    }
                    DataType::U32 => match t_src {
                        DataType::U8
                        | DataType::U16
                        | DataType::U32
                        | DataType::I8
                        | DataType::I16
                        | DataType::I32 => v_src,
                        DataType::F32 => (f32::from_bits(v_src) as i32) as u32,
                    },
                    DataType::I8 => {
                        ((match t_src {
                            DataType::U8 => (v_src as u8) as i8,
                            DataType::U16 => (v_src as u16) as i8,
                            DataType::U32 => v_src as i8,
                            DataType::I8 => v_src as i8,
                            DataType::I16 => (v_src as i16) as i8,
                            DataType::I32 => (v_src as i32) as i8,
                            DataType::F32 => (f32::from_bits(v_src) as i32) as i8,
                        }) as i32) as u32
                    }
                    DataType::I16 => {
                        ((match t_src {
                            DataType::U8 => (v_src as u8) as i16,
                            DataType::U16 => (v_src as u16) as i16,
                            DataType::U32 => v_src as i16,
                            DataType::I8 => (v_src as i8) as i16,
                            DataType::I16 => v_src as i16,
                            DataType::I32 => (v_src as i32) as i16,
                            DataType::F32 => (f32::from_bits(v_src) as i32) as i16,
                        }) as i32) as u32
                    }
                    DataType::I32 => {
                        (match t_src {
                            DataType::U8 => (v_src as u8) as i32,
                            DataType::U16 => (v_src as u16) as i32,
                            DataType::U32 => v_src as i32,
                            DataType::I8 => (v_src as i8) as i32,
                            DataType::I16 => (v_src as i16) as i32,
                            DataType::I32 => v_src as i32,
                            DataType::F32 => f32::from_bits(v_src) as i32,
                        }) as u32
                    }
                    DataType::F32 => (match t_src {
                        DataType::U8 => (v_src as u8) as f32,
                        DataType::U16 => (v_src as u16) as f32,
                        DataType::U32 => v_src as f32,
                        DataType::I8 => (v_src as i8) as f32,
                        DataType::I16 => (v_src as i16) as f32,
                        DataType::I32 => (v_src as i32) as f32,
                        DataType::F32 => f32::from_bits(v_src),
                    })
                    .to_bits(),
                };

                self.registers.set(inst.arg0_register(), v_dest)?;
            }
            Opcode {
                base: Self::OP_BASE_MATH,
                ..
            } => {
                let arith = self.get_arith_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_ADD => arith.add(val_a, val_b)?,
                    Self::OP_SUB => arith.sub(val_a, val_b)?,
                    Self::OP_MUL => arith.mul(val_a, val_b)?,
                    Self::OP_DIV => arith.div(val_a, val_b)?,
                    Self::OP_REM => arith.rem(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set(Register::Status, if res.carry { 1 } else { 0 })?;
            }
            Opcode {
                base: Self::OP_BASE_BITS,
                ..
            } => {
                let bitwise = self.get_bitwise_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_BAND => bitwise.band(val_a, val_b)?,
                    Self::OP_BOR => bitwise.bor(val_a, val_b)?,
                    Self::OP_BXOR => bitwise.bxor(val_a, val_b)?,
                    Self::OP_BSHL => bitwise.bsftl(val_a, val_b)?,
                    Self::OP_BSHR => bitwise.bsftr(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set(Register::Status, if res.carry { 1 } else { 0 })?;
            }
            Opcode {
                base: Self::OP_BASE_TEST,
                ..
            } => {
                let relative = self.get_relative_operation(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_EQ => relative.eq(val_a, val_b)?,
                    Self::OP_NEQ => relative.neq(val_a, val_b)?,
                    Self::OP_GREATER => relative.gt(val_a, val_b)?,
                    Self::OP_GREATER_EQ => relative.geq(val_a, val_b)?,
                    Self::OP_LESS => relative.lt(val_a, val_b)?,
                    Self::OP_LESS_EQ => relative.leq(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers
                    .set(inst.arg0_register(), if res { 1 } else { 0 })?;
            }
            _ => return Err(ProcessorError::UnknownInstruction(inst)),
        };

        // Define an action queue
        let mut dev_action_queue: Vec<DeviceAction> = Vec::new();

        // Check for any actions
        for dev in self.devices.iter() {
            if let Some(action) = dev.borrow_mut().on_step() {
                dev_action_queue.push(action);
            }
        }

        // Perform requested actions
        for action in dev_action_queue {
            match action {
                DeviceAction::CallInterrupt(num) => {
                    self.hardware_interrupt(num)?;
                }
            }
        }

        if let Some(jmp_val) = inst_jump {
            self.registers
                .set(Register::ProgramCounter, pc + jmp_val * 4)?;
        }

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
}

impl Default for Processor {
    fn default() -> Self {
        Self::new()
    }
}
