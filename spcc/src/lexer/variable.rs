use super::common::*;


fn load_variable_to_register(address_load: Vec<String>, register: usize, variable_name: &str) -> Vec<String>
{
    let mut assembly = Vec::new();

    assembly.push(format!("; Load Variable {0:} to Register {1:}", variable_name, register));
    assembly.extend(address_load);
    assembly.push(format!("ld {0:}, {0:}", register));

    return assembly;
}

fn save_register_to_variable(address_load: Vec<String>, register_value: usize, register_loc: usize, variable_name: &str) -> Vec<String>
{
    let mut assembly = Vec::new();

    assembly.push(format!("; Set Variable {0:} from Register {1:} using {2:}", variable_name, register_value, register_loc));
    assembly.extend(address_load);
    assembly.push(format!("sav {0:}, {1:}", register_loc, register_value));

    return assembly;
}

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
}

impl LoadValue for StaticVariable
{
    fn load_value_to_register(&self, register: usize, _: usize) -> Vec<String>
    {
        return load_variable_to_register(
            self.load_address_to_register(register),
            register,
            &self.get_name());
    }
}

impl NamedMemoryValue for StaticVariable
{
    fn set_value_from_register(&self, register_from: usize, register_spare: usize) -> Vec<String>
    {
        return save_register_to_variable(
            self.load_address_to_register(register_spare),
            register_from,
            register_spare,
            &self.get_name());
    }

    fn get_name(&self) -> String
    {
        return self.name.clone();
    }

    fn load_address_to_register(&self, register: usize) -> Vec<String>
    {
        return vec![
            format!("ldn {0:}", register),
            format!(".loadloc {0:}", self.label)
        ];
    }
}

impl ToString for StaticVariable
{
    fn to_string(&self) -> String
    {
        return format!("static({0:})", self.name);
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
}

impl LoadValue for Variable
{
    fn load_value_to_register(&self, register: usize, _: usize) -> Vec<String>
    {
        return load_variable_to_register(
            self.load_address_to_register(register),
            register,
            &self.get_name());
    }
}

impl NamedMemoryValue for Variable
{
    fn set_value_from_register(&self, register_from: usize, register_spare: usize) -> Vec<String>
    {
        return save_register_to_variable(
            self.load_address_to_register(register_spare),
            register_from,
            register_spare,
            &self.get_name());
    }

    fn load_address_to_register(&self, register: usize) -> Vec<String>
    {
        return vec![
            format!("ldn {0:}", register),
            format!(".load {0:}", self.offset),
            format!("add {0:}, {0:}, {1:}", register, REG_FRAME_SP_BASE),
            format!("add {0:}, {0:}, {1:}", register, REG_FRAME_SP_VALUE)
        ];
    }

    fn get_name(&self) -> String
    {
        return self.name.clone();
    }
}

impl ToString for Variable
{
    fn to_string(&self) -> String
    {
        return format!("var({0:} -> {1:})", self.name, self.offset);
    }
}
