use super::register::Register;

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
    const NUM_IMM_BITS: u32 = { u8::BITS * 3 };

    const IMM_SIGN_BIT: u32 = { 1 << (Self::NUM_IMM_BITS - 1) };

    const IMM_NEG_MASK: u32 = {
        let mut val = 0;
        let mut i = Self::NUM_IMM_BITS;
        while i < u32::BITS {
            val |= i << i;
            i += 1;
        }
        val
    };

    pub fn new(data: [u8; 4]) -> Self {
        Self { data }
    }

    pub fn opcode(&self) -> u8 {
        self.data[0]
    }

    fn reg_from_arg(arg: u8) -> Register {
        Register::GeneralPurpose((arg & 0x1F) as usize)
    }

    fn dt_from_arg(arg: u8) -> Result<DataType, DataTypeError> {
        let dt = (arg >> 5) & 7;
        DataType::try_from(dt)
    }

    pub fn arg0(&self) -> u8 {
        self.data[1]
    }

    pub fn arg0_data_type(&self) -> Result<DataType, DataTypeError> {
        Self::dt_from_arg(self.arg0())
    }

    pub fn arg0_register(&self) -> Register {
        Self::reg_from_arg(self.arg0())
    }

    pub fn arg1(&self) -> u8 {
        self.data[2]
    }

    pub fn arg1_data_type(&self) -> Result<DataType, DataTypeError> {
        Self::dt_from_arg(self.arg1())
    }

    pub fn arg1_register(&self) -> Register {
        Self::reg_from_arg(self.arg1())
    }

    pub fn arg2(&self) -> u8 {
        self.data[3]
    }

    pub fn arg2_data_type(&self) -> Result<DataType, DataTypeError> {
        Self::dt_from_arg(self.arg2())
    }

    pub fn arg2_register(&self) -> Register {
        Self::reg_from_arg(self.arg2())
    }

    pub fn imm_unsigned(&self) -> u32 {
        ((self.arg0() as u32) << 16) | ((self.arg1() as u32) << 8) | (self.arg2() as u32)
    }

    pub fn imm_signed(&self) -> i32 {
        let mut val = self.imm_unsigned();
        if (val & Self::IMM_SIGN_BIT) != 0 {
            val |= Self::IMM_NEG_MASK;
        }
        val as i32
    }
}
