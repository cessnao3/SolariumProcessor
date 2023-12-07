use core::fmt;

#[derive(Debug, Clone)]
pub struct ImmediateError(pub String);

impl fmt::Display for ImmediateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Immediate Error => {}", self.0)
    }
}

macro_rules! gen_read_immediate {
    ($fnname:ident, $t:ident) => {
        pub fn $fnname(arg: &str) -> Result<$t, ImmediateError> {
            let res = if arg.starts_with("0x") || arg.starts_with("0X") {
                $t::from_str_radix(arg, 16)
            } else {
                arg.parse::<$t>()
            };

            match res {
                Ok(v) => Ok(v),
                Err(_) => Err(ImmediateError(arg.to_string())),
            }
        }
    };
}

gen_read_immediate!(parse_imm_i8, i8);
gen_read_immediate!(parse_imm_u8, u8);
gen_read_immediate!(parse_imm_i16, i16);
gen_read_immediate!(parse_imm_u16, u16);
gen_read_immediate!(parse_imm_i32, i32);
gen_read_immediate!(parse_imm_u32, u32);
