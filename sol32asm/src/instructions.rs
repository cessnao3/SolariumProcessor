use core::fmt;
use std::fmt::Debug;

use crate::{
    argument::{ArgumentError, ArgumentRegister, ArgumentType},
    immediate::{parse_imm_i16, ImmediateError},
};
use sol32::cpu::{Opcode, Processor};

const INST_SIZE: usize = 4;

#[derive(Debug, Clone)]
pub enum InstructionError {
    CountMismatch(usize, usize),
    Immediate(ImmediateError),
    Argument(ArgumentError),
}

impl fmt::Display for InstructionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CountMismatch(num, expected) => write!(f, "Found {num}, Expected {expected}"),
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::Argument(a) => write!(f, "Argument Error => {a}"),
        }
    }
}

impl From<ImmediateError> for InstructionError {
    fn from(value: ImmediateError) -> Self {
        Self::Immediate(value)
    }
}

impl From<ArgumentError> for InstructionError {
    fn from(value: ArgumentError) -> Self {
        Self::Argument(value)
    }
}

pub trait ToInstruction {
    fn to_instruction(&self) -> [u8; INST_SIZE];
}

macro_rules! InstNoArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 0;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), 0, 0, 0]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self)
                }
            }
        }
    };
}

macro_rules! InstSingleArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentRegister,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(arg: ArgumentRegister) -> Self {
                Self { arg }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), self.arg.to_byte(), 0, 0]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self::new(ArgumentRegister::try_from(args[0].as_ref())?))
                }
            }
        }
    };
}

macro_rules! InstSingleArgDataType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentType,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(arg: ArgumentType) -> Self {
                Self { arg }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), self.arg.to_byte(), 0, 0]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self::new(ArgumentType::try_from(args[0].as_ref())?))
                }
            }
        }
    };
}

macro_rules! InstImmediateArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            imm: u16,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(imm: u16) -> Self {
                Self { imm }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                let imm = self.imm.to_be_bytes();
                [Self::OP.to_byte(), 0, imm[0], imm[1]]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let imm = parse_imm_i16(&args[0])? as u16;
                    Ok(Self::new(imm))
                }
            }
        }
    };
}

macro_rules! InstSingleArgImm {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentType,
            val: u16,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg: ArgumentType, val: u16) -> Self {
                Self { arg, val }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                let imm = self.val.to_be_bytes();
                [Self::OP.to_byte(), self.arg.to_byte(), imm[0], imm[1]]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let imm = parse_imm_i16(&args[1])? as u16;
                    Ok(Self::new(a0, imm))
                }
            }
        }
    };
}

macro_rules! InstDoubleArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentRegister,
            arg1: ArgumentRegister,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentRegister, arg1: ArgumentRegister) -> Self {
                Self { arg0, arg1 }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentRegister::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }
    };
}

macro_rules! InstDoubleArgType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentRegister,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentType, arg1: ArgumentRegister) -> Self {
                Self { arg0, arg1 }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }
    };
}

macro_rules! InstDoubleArgDoubleType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentType,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentType, arg1: ArgumentType) -> Self {
                Self { arg0, arg1 }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentType::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }
    };
}

macro_rules! InstArith {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentRegister,
            arg2: ArgumentRegister,
        }

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 3;

            pub fn new(arg0: ArgumentType, arg1: ArgumentRegister, arg2: ArgumentRegister) -> Self {
                Self { arg0, arg1, arg2 }
            }
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    self.arg2.to_byte(),
                ]
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    let a2 = ArgumentRegister::try_from(args[2].as_ref())?;
                    Ok(Self::new(a0, a1, a2))
                }
            }
        }
    };
}

InstNoArg!(OpNoop, Processor::OP_NOOP);
InstNoArg!(OpReset, Processor::OP_RESET);
InstNoArg!(OpRetInt, Processor::OP_INTERRUPT_RETURN);
InstNoArg!(OpRet, Processor::OP_RETURN);
InstNoArg!(OpHalt, Processor::OP_HALT);

InstSingleArg!(OpInt, Processor::OP_INTERRUPT);
InstSingleArg!(OpIntr, Processor::OP_INTERRUPT_REGISTER);
InstSingleArg!(OpCall, Processor::OP_CALL);
InstSingleArg!(OpPush, Processor::OP_PUSH);
InstSingleArg!(OpPop, Processor::OP_POP);
InstSingleArg!(OpPopr, Processor::OP_POP_REG);
InstSingleArg!(OpJmp, Processor::OP_JUMP);
InstSingleArg!(OpJmpr, Processor::OP_JUMP_REL);
InstSingleArgDataType!(OpLoadNext, Processor::OP_LOAD_NEXT);

InstSingleArg!(OpTz, Processor::OP_TEST_ZERO);
InstSingleArg!(OpTnz, Processor::OP_TEST_NOT_ZERO);

InstImmediateArg!(OpJmpri, Processor::OP_JUMP_REL_IMM); // REL IMMEDIATE NEEDS TO HAVE DELTAS FROM CURRENT!
InstSingleArgImm!(OpLoadi, Processor::OP_LOAD_IMM);
InstSingleArgImm!(OpLoadri, Processor::OP_LOAD_IMM_REL);

InstDoubleArg!(OpNot, Processor::OP_NOT);
InstDoubleArg!(OpBool, Processor::OP_BOOL);
InstDoubleArg!(OpCopy, Processor::OP_COPY);

InstDoubleArgType!(OpSave, Processor::OP_SAVE);
InstDoubleArgType!(OpSaver, Processor::OP_SAVE_REL);
InstDoubleArgType!(OpLoad, Processor::OP_LOAD);
InstDoubleArgType!(OpLoadr, Processor::OP_LOAD_REL);

InstDoubleArgDoubleType!(OpConv, Processor::OP_CONV);

InstArith!(OpAdd, Processor::OP_ADD);
InstArith!(OpSub, Processor::OP_SUB);
InstArith!(OpMul, Processor::OP_MUL);
InstArith!(OpDiv, Processor::OP_DIV);
InstArith!(OpRem, Processor::OP_REM);
InstArith!(OpBand, Processor::OP_BAND);
InstArith!(OpBor, Processor::OP_BOR);
InstArith!(OpBxor, Processor::OP_BXOR);
InstArith!(OpBshl, Processor::OP_BSHL);
InstArith!(OpBshr, Processor::OP_BSHR);

InstArith!(OpTeq, Processor::OP_EQ);
InstArith!(OpTneq, Processor::OP_NEQ);
InstArith!(OpTgt, Processor::OP_GREATER);
InstArith!(OpTgeq, Processor::OP_GREATER_EQ);
InstArith!(OpTlt, Processor::OP_LESS);
InstArith!(OpTleq, Processor::OP_LESS_EQ);
