use super::common::{LoadValue};

pub struct Expression
{

}

impl Expression
{
    pub new() -> Expression
    {
        return Self
        {

        };
    }
}

impl ToString for Expression
{
    fn to_string(&self) -> String
    {
        return format!("Expression");
    }
}

impl LoadValue for Expression
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!("; Loading expression {0:} to {1:}", self.to_string(), register));
    }
}
