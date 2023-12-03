use crate::{immediate::{ImmediateError, parse_imm_i16}, argument::{ArgumentError, ArgumentRegister, ArgumentType}};
use sol32::cpu::{Processor, Opcode};

const INST_SIZE: usize = 4;

pub enum InstructionError {
    CountMismatch(usize, usize),
    Immediate(ImmediateError),
    Argument(ArgumentError),
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
    fn to_instruction(&self, args: &[&str]) -> [u8; INST_SIZE];
}

macro_rules! InstNoArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 0;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), 0, 0, 0]
            }
        }

        impl TryFrom<&[&str]> for $op_name {
            type Error = InstructionError;

            fn try_from(args: &[&str]) -> Result<Self, Self::Error> {
                if args.len() == Self::NUM_ARGS {
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
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok([Self::OP.to_byte(), ArgumentRegister::try_from(args[0])?.to_byte(), 0, 0])
                }
            }
        }
    };
}

macro_rules! InstSingleArgImm {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentRegister::try_from(args[0])?;
                    let imm = (parse_imm_i16(args[1])? as u16).to_be_bytes();
                    Ok([Self::OP.to_byte(), a0.to_byte(), imm[0], imm[1]])
                }
            }
        }
    };
}

macro_rules! InstDoubleArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentRegister::try_from(args[0])?;
                    let a1= ArgumentRegister::try_from(args[1])?;
                    Ok([Self::OP.to_byte(), a0.to_byte(), a1.to_byte(), 0])
                }
            }
        }
    };
}

macro_rules! InstDoubleArgType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0])?;
                    let a1= ArgumentRegister::try_from(args[1])?;
                    Ok([Self::OP.to_byte(), a0.to_byte(), a1.to_byte(), 0])
                }
            }
        }
    };
}

macro_rules! InstDoubleArgDoubleType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0])?;
                    let a1= ArgumentType::try_from(args[1])?;
                    Ok([Self::OP.to_byte(), a0.to_byte(), a1.to_byte(), 0])
                }
            }
        }
    };
}

macro_rules! InstArith {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Default)]
        pub struct $op_name;

        impl $op_name {
            const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 3;
        }

        impl ToInstruction for $op_name {
            fn to_instruction(&self, args: &[&str]) -> Result<[u8; INST_SIZE], InstructionError> {
                if args.len() == Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0])?;
                    let a1 = ArgumentRegister::try_from(args[1])?;
                    let a2 = ArgumentRegister::try_from(args[2])?;
                    Ok([Self::OP.to_byte(), a0.to_byte(), a1.to_byte(), a2.to_byte()])
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

InstSingleArgImm!(OpJmpri, Processor::OP_JUMP_REL_IMM);
InstSingleArgImm!(OpLoadi, Processor::OP_LOAD_IMM);
InstSingleArgImm!(OpLoadri, Processor::OP_LOAD_IMM_REL);

InstDoubleArg!(OpNot, Processor::OP_NOT);
InstDoubleArg!(OpBool, Processor::OP_BOOL);
InstDoubleArg!(OpTz, Processor::OP_TEST_ZERO);
InstDoubleArg!(OpTnz, Processor::OP_TEST_NOT_ZERO);
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
