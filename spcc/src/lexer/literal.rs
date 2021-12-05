pub struct Literal
{
    value: u16
}

impl LoadValue for Literal
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!("; Load Literal {0:} to Register {1:}", self.value, register));
        assembly.push("jmpri 2".to_string());
        assembly.push(format!(".load {0:}", self.value));
        assembly.push(format!("ldri {0:}, -1", register));

        return assembly;
    }
}
