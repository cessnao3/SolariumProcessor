use std::collections::HashMap;

use self::types::TypeInfo;

mod expressions;
mod statement;
mod types;

pub struct StatusState {
    signed_ops: bool
}

impl StatusState {
    pub fn new() -> Self {
        Self { signed_ops: false }
    }
}

pub struct CompilerState {
    local_asm: Vec<String>,
    static_asm: Vec<String>,
    processor_state: StatusState,
    guid_count: u64,
    types: HashMap<String, TypeInfo>
}

impl CompilerState {
    pub fn new() -> Self {
        let mut state = Self {
            local_asm: Vec::new(),
            static_asm: Vec::new(),
            processor_state: StatusState::new(),
            guid_count: 0,
            types: HashMap::new()
        };

        for t in TypeInfo::get_default_types() {
            let old = state.types.insert(t.name.to_string(), t);
            if old.is_some() {
                panic!("duplicate default name provided for type!");
            }
        }

        state
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
