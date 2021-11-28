use super::common::*;

pub struct StaticVariable
{
    name: String,
    label: String
}

impl StaticVariable
{
    pub fn new(name: &str, label: &str) -> StaticVariable
    {
        return Self
        {
            name: name.to_string(),
            label: label.to_string()
        };
    }

    pub fn get_name(&self) -> String
    {
        return self.name.clone();
    }
}

impl LoadValue for StaticVariable
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!("; Load Variable {0:}/{1:} to Register {2:}", self.name, self.label, register));
        assembly.push("jmpri 2".to_string());
        assembly.push(format!(".loadloc {0:}", self.label));
        assembly.push(format!("ldir {0:}, -1", register));

        return assembly;
    }
}

pub struct Variable
{
    name: String,
    offset: i32
}

impl Variable
{
    pub fn new(name: &str, offset: i32) -> Variable
    {
        return Self
        {
            name: name.to_string(),
            offset
        };
    }

    pub fn get_name(&self) -> String
    {
        return self.name.clone();
    }
}

impl LoadValue for Variable
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!("; Load Variable {0:}/{1:} to Register {2:}", self.name, self.offset, register));
        assembly.push("jmpri 2".to_string());
        assembly.push(format!(".load {0:}", self.offset));
        assembly.push(format!("ldir {0:}, -1", register));
        assembly.push(format!("add {0:}, {0:}, {1:}", register, REG_FRAME_SP_VALUE));
        assembly.push(format!("ld {0:}, {0:}", register));

        return assembly;
    }
}
