use std::collections::HashMap;

use self::types::TypeInfo;

mod expressions;
mod statement;
mod types;

pub struct CompilerState {
    local_asm: Vec<String>,
    static_asm: Vec<String>,
    processor_state: u16,
    guid_count: u64,
    types: HashMap<String, TypeInfo>
}

impl CompilerState {
    fn flag_state_for_math(&self) -> String {
        "".to_string()
    }
}

struct CompilerError {
    msg: String,
    loc: String
}

impl CompilerError {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string(),
            loc: "Unknown".to_string()
        }
    }

    pub fn not_implemented() -> Self {
        Self::new("not implemented")
    }
}

impl ToString for CompilerError {
    fn to_string(&self) -> String {
        format!("{}: {}", self.loc, self.msg)
    }
}

trait CodeComponent: ToString {
    fn generate_code(&self, state: &mut CompilerState) -> Result<(), CompilerError>;
}
