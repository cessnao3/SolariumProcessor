use super::common::*;

pub struct FunctionDefinition
{
    name: String,
    num_args: usize,
    label: String
}

impl FunctionDefinition
{
    pub fn new(name: &str, num_args: usize, label: &str) -> FunctionDefinition
    {
        return Self
        {
            name: name.to_string(),
            num_args,
            label: label.to_string()
        };
    }
}

impl EmitAssembly for FunctionDefinition
{
    fn emit(&self, _: &mut ScopeManager) -> Vec<String>
    {
        return Vec::new();
    }
}

impl FunctionCall for FunctionDefinition
{
    fn get_name(&self) -> String
    {
        return self.name.clone();
    }

    fn load_function_address(&self, register: usize) -> Vec<String>
    {
        return vec![
            format!("jmpri 2"),
            format!(".loadloc {0:}", self.label),
            format!("ldri {0:}, -1", register)
        ];
    }

    fn num_args(&self) -> usize
    {
        return self.num_args;
    }
}
