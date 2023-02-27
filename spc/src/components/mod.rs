mod expressions;
mod statements;
mod types;

pub struct CodeGenerationManager {
    local_asm: Vec<String>,
    static_asm: Vec<String>,
    processor_state: u16,
    guid_count: u64
}

impl CodeGenerationManager {
    fn flag_state_for_math(&self) -> String {
        "".to_string()
    }
}

trait CodeComponent: ToString {
    fn generate_local_code(&self) -> Vec<String>;

    fn generate_static_code(&self) -> Vec<String>;
}
