use std::collections::HashMap;

pub use crate::argument::{Argument, ArgumentError};

use once_cell::sync::Lazy;
use sproc::common::{InstructionData, InstructionError};

use sproc::cpu::Register;

#[derive(Clone, Debug)]
pub enum OpcodeParseError {
    ArgumentCount { expected: usize, actual: usize },
    ArgumentError(ArgumentError),
}

impl std::fmt::Display for OpcodeParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentCount { expected, actual } => write!(f, "opcode expected {expected} arguments, got {actual}"),
            Self::ArgumentError(e) => write!(f, "{e}"),
        }
    }
}

pub trait AssemblyOpcode {
    fn to_instruction(&self) -> Result<InstructionData, InstructionError>;

    fn to_assembly_text(&self) -> String;
}

macro_rules! NoArgInstruction {
    ( $opcode: literal, $inst_name: ident ) => {
        #[derive(Copy, Clone)]
        pub struct $inst_name;

        impl $inst_name {
            pub fn new() -> Self {
                Self::default()
            }
        }

        impl Default for $inst_name {
            fn default() -> Self {
                Self
            }
        }

        impl AssemblyOpcode for $inst_name {
            fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
                InstructionData::new_arg0($opcode)
            }

            fn to_assembly_text(&self) -> String {
                format!("{}", stringify!($inst_name).to_lowercase())
            }
        }

        impl TryFrom<&[Argument]> for $inst_name {
            type Error = OpcodeParseError;
            fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
                if args.len() != 0 {
                    Err(OpcodeParseError::ArgumentCount { expected: 0, actual: args.len() })
                } else {
                    Ok(Self { })
                }
            }
        }

        impl std::fmt::Display for $inst_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}()", stringify!($inst_name))
            }
        }
    };
}

macro_rules! SingleReg {
    ( $opcode: literal, $inst_name: ident ) => {
        #[derive(Copy, Clone)]
        pub struct $inst_name {
            pub reg: Register,
        }

        impl $inst_name {
            pub fn new(reg: Register) -> Self {
                Self {
                    reg
                }
            }
        }

        impl AssemblyOpcode for $inst_name {
            fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
                InstructionData::new_arg1($opcode, u8::from(self.reg))
            }

            fn to_assembly_text(&self) -> String {
                format!("{} {}", stringify!($inst_name).to_lowercase(), self.reg)
            }
        }

        impl TryFrom<&[Argument]> for $inst_name {
            type Error = OpcodeParseError;
            fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
                if args.len() != 1 {
                    Err(OpcodeParseError::ArgumentCount { expected: 1, actual: args.len() })
                } else {
                    let reg = match args[0].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    Ok(Self {
                        reg,
                    })
                }
            }
        }

        impl std::fmt::Display for $inst_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($inst_name), self.reg)
            }
        }
    };
}

macro_rules! DoubleReg {
    ( $opcode: literal, $inst_name: ident ) => {
        #[derive(Copy, Clone)]
        pub struct $inst_name {
            pub reg_a: Register,
            pub reg_b: Register,
        }

        impl $inst_name {
            pub fn new(reg_a: Register, reg_b: Register) -> Self {
                Self {
                    reg_a,
                    reg_b,
                }
            }
        }

        impl AssemblyOpcode for $inst_name {
            fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
                InstructionData::new_arg2($opcode, u8::from(self.reg_b), u8::from(self.reg_a))
            }

            fn to_assembly_text(&self) -> String {
                format!("{} {}, {}", stringify!($inst_name).to_lowercase(), self.reg_a, self.reg_b)
            }
        }

        impl TryFrom<&[Argument]> for $inst_name {
            type Error = OpcodeParseError;
            fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
                if args.len() != 2 {
                    Err(OpcodeParseError::ArgumentCount { expected: 2, actual: args.len() })
                } else {
                    let reg_a = match args[0].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    let reg_b = match args[1].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    Ok(Self {
                        reg_a,
                        reg_b,
                    })
                }
            }
        }

        impl std::fmt::Display for $inst_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({}, {})", stringify!($inst_name), self.reg_a, self.reg_b)
            }
        }
    };
}

macro_rules! TripleReg {
    ( $opcode: literal, $inst_name: ident ) => {
        #[derive(Copy, Clone)]
        pub struct $inst_name {
            pub reg_dst: Register,
            pub reg_a: Register,
            pub reg_b: Register,
        }

        impl $inst_name {
            pub fn new(reg_dst: Register, reg_a: Register, reg_b: Register) -> Self {
                Self {
                    reg_dst,
                    reg_a,
                    reg_b,
                }
            }
        }

        impl AssemblyOpcode for $inst_name {
            fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
                InstructionData::new($opcode, u8::from(self.reg_b), u8::from(self.reg_a), u8::from(self.reg_dst))
            }

            fn to_assembly_text(&self) -> String {
                format!("{} {}, {}, {}", stringify!($inst_name).to_lowercase(), self.reg_dst, self.reg_a, self.reg_b)
            }
        }

        impl TryFrom<&[Argument]> for $inst_name {
            type Error = OpcodeParseError;
            fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
                if args.len() != 3 {
                    Err(OpcodeParseError::ArgumentCount { expected: 3, actual: args.len() })
                } else {
                    let reg_dst = match args[0].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    let reg_a = match args[1].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    let reg_b = match args[2].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    Ok(Self {
                        reg_dst,
                        reg_a,
                        reg_b,
                    })
                }
            }
        }

        impl std::fmt::Display for $inst_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({}, {}, {})", stringify!($inst_name), self.reg_dst, self.reg_a, self.reg_b)
            }
        }
    };
}

macro_rules! ImmediateReg {
    ( $opcode: literal, $inst_name: ident ) => {
        #[derive(Copy, Clone)]
        pub struct $inst_name {
            pub reg_dst: Register,
            pub immediate: i8,
        }

        impl $inst_name {
            pub fn new(reg_dst: Register, immediate: i8) -> Self {
                Self {
                    reg_dst,
                    immediate,
                }
            }
        }

        impl AssemblyOpcode for $inst_name {
            fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
                let imm = ImmedateByteValues::from(self.immediate);
                InstructionData::new($opcode, imm.get_high_nybble(), imm.get_low_nybble(), u8::from(self.reg_dst))
            }

            fn to_assembly_text(&self) -> String {
                format!("{} {}, {}", stringify!($inst_name).to_lowercase(), self.reg_dst, self.immediate)
            }
        }

        impl TryFrom<&[Argument]> for $inst_name {
            type Error = OpcodeParseError;
            fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
                if args.len() != 2 {
                    Err(OpcodeParseError::ArgumentCount { expected: 2, actual: args.len() })
                } else {
                    let reg_dst = match args[0].to_register_val() {
                        Ok(v) => v,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    let immediate = match args[1].to_u8() {
                        Ok(v) => v as i8,
                        Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
                    };

                    Ok(Self {
                        reg_dst,
                        immediate,
                    })
                }
            }
        }

        impl std::fmt::Display for $inst_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({}, {})", stringify!($inst_name), self.reg_dst, self.immediate)
            }
        }
    };
}

#[derive(Copy, Clone)]
struct ImmedateByteValues {
    val: u8,
}

impl ImmedateByteValues {
    pub fn get_low_nybble(&self) -> u8 {
        self.val & 0xF
    }

    pub fn get_high_nybble(&self) -> u8 {
        (self.val & 0xF0) >> 4
    }
}

impl From<u8> for ImmedateByteValues {
    fn from(value: u8) -> Self {
        Self {
            val: value
        }
    }
}

impl From<i8> for ImmedateByteValues {
    fn from(value: i8) -> Self {
        ImmedateByteValues::from(value as u8)
    }
}

NoArgInstruction!(0, Noop);
NoArgInstruction!(1, IntOn);
NoArgInstruction!(2, IntOff);
NoArgInstruction!(3, Reset);
NoArgInstruction!(4, Pop);
NoArgInstruction!(5, Ret);
NoArgInstruction!(6, RetInt);
NoArgInstruction!(7, Halt);

SingleReg!(1, Jmp);
SingleReg!(2, Jmpr);
SingleReg!(3, Push);
SingleReg!(4, Popr);
SingleReg!(5, Call);
SingleReg!(6, Int);
SingleReg!(7, Intr);
SingleReg!(8, Tz);
SingleReg!(9, Tnz);
SingleReg!(10, Bool);
SingleReg!(11, Not);
SingleReg!(12, Ldn);
SingleReg!(13, Neg);
SingleReg!(14, Bnot);

#[derive(Copy, Clone)]
pub struct Jmpri {
    immediate: i8,
}

impl Jmpri {
    pub fn new(immediate: i8) -> Self {
        Self {
            immediate,
        }
    }
}

impl AssemblyOpcode for Jmpri {
    fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
        let imm = ImmedateByteValues::from(self.immediate);
        InstructionData::new_arg2(1, imm.get_high_nybble(), imm.get_low_nybble())
    }

    fn to_assembly_text(&self) -> String {
        format!("{} {}", stringify!($inst_name).to_lowercase(), self.immediate)
    }
}

impl TryFrom<&[Argument]> for Jmpri {
    type Error = OpcodeParseError;
    fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
        if args.len() != 1 {
            Err(OpcodeParseError::ArgumentCount { expected: 1, actual: args.len() })
        } else {
            let val = match args[0].to_u8() {
                Ok(v) => v as i8,
                Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
            };

            Ok(Self { immediate: val })
        }
    }
}

impl std::fmt::Display for Jmpri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", stringify!(Jmpri), self.immediate)
    }
}

DoubleReg!(2, Ld);
DoubleReg!(3, Sav);
DoubleReg!(4, Ldr);
DoubleReg!(5, Savr);
DoubleReg!(6, Cpy);
DoubleReg!(7, Tg);
DoubleReg!(8, Tgs);
DoubleReg!(9, Tl);
DoubleReg!(10, Tls);
DoubleReg!(11, Teq);

#[derive(Copy, Clone)]
pub struct Arg {
    reg: Register,
    arg: u8,
}

impl Arg {
    pub fn new(reg: Register, arg: u8) -> Self {
        Self {
            reg,
            arg,
        }
    }
}

impl AssemblyOpcode for Arg {
    fn to_instruction(&self) -> Result<InstructionData, InstructionError> {
        InstructionData::new_arg2(12, self.arg, u8::from(self.reg))
    }

    fn to_assembly_text(&self) -> String {
        format!("{} {}, {}", stringify!($inst_name).to_lowercase(), self.reg, self.arg)
    }
}

impl TryFrom<&[Argument]> for Arg {
    type Error = OpcodeParseError;
    fn try_from(args: &[Argument]) -> Result<Self, Self::Error> {
        if args.len() != 2 {
            Err(OpcodeParseError::ArgumentCount { expected: 2, actual: args.len() })
        } else {
            let reg = match args[0].to_register_val() {
                Ok(v) => v,
                Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
            };

            let arg = match args[1].to_u8() {
                Ok(v) => v,
                Err(e) => return Err(OpcodeParseError::ArgumentError(e)),
            };

            Ok(Self { reg, arg })
        }
    }
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}, {})", stringify!(Arg), self.reg, self.arg)
    }
}

ImmediateReg!(1, Ldi);
ImmediateReg!(2, Ldri);
TripleReg!(3, Add);
TripleReg!(4, Sub);
TripleReg!(5, Mul);
TripleReg!(6, Div);
TripleReg!(7, Mod);
TripleReg!(8, Muls);
TripleReg!(9, Divs);
TripleReg!(10, Mods);
TripleReg!(11, Band);
TripleReg!(12, Bor);
TripleReg!(13, Bxor);
TripleReg!(14, Bshft);
TripleReg!(15, Ashft);

pub type AssemblyResult = Result<Box<dyn AssemblyOpcode>, OpcodeParseError>;

fn inst_transformation<T: AssemblyOpcode + 'static>(val: Result<T, OpcodeParseError>) -> AssemblyResult {
    match val {
        Ok(v) => Ok(Box::new(v) as Box<dyn AssemblyOpcode>),
        Err(e) => Err(e),
    }
}

pub type InstructionFunction = fn(&[Argument]) -> AssemblyResult;
type InstructionMap = HashMap<String, InstructionFunction>;

pub static INSTRUCTION_MAP: Lazy<InstructionMap> = Lazy::new(|| {
    let mut instructions: InstructionMap = HashMap::new();

    macro_rules! add_inst {
        ( $name: ident ) => {
            instructions.insert(stringify!($name).to_lowercase(), |i| inst_transformation($name::try_from(i)));
        };
    }

    add_inst!(Noop);
    add_inst!(IntOn);
    add_inst!(IntOff);
    add_inst!(Reset);
    add_inst!(Pop);
    add_inst!(Ret);
    add_inst!(RetInt);
    add_inst!(Halt);

    add_inst!(Jmp);
    add_inst!(Jmpr);
    add_inst!(Push);
    add_inst!(Popr);
    add_inst!(Call);
    add_inst!(Int);
    add_inst!(Intr);
    add_inst!(Tz);
    add_inst!(Tnz);
    add_inst!(Bool);
    add_inst!(Not);
    add_inst!(Ldn);
    add_inst!(Neg);
    add_inst!(Bnot);

    add_inst!(Jmpri);
    add_inst!(Ld);
    add_inst!(Sav);
    add_inst!(Ldr);
    add_inst!(Savr);
    add_inst!(Cpy);
    add_inst!(Tg);
    add_inst!(Tgs);
    add_inst!(Tl);
    add_inst!(Tls);
    add_inst!(Teq);
    add_inst!(Arg);

    add_inst!(Ldi);
    add_inst!(Ldri);
    add_inst!(Add);
    add_inst!(Sub);
    add_inst!(Mul);
    add_inst!(Div);
    add_inst!(Mod);
    add_inst!(Muls);
    add_inst!(Divs);
    add_inst!(Mods);
    add_inst!(Band);
    add_inst!(Bor);
    add_inst!(Bxor);
    add_inst!(Bshft);
    add_inst!(Ashft);

    instructions
});
