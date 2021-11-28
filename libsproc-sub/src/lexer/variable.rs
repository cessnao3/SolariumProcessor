pub struct Variable
{
    name: String,
    label: String
}

impl Variable
{
    pub fn new(name: &str, label: &str) -> Variable
    {
        return Self
        {
            name: name.to_string(),
            label: label.to_string()
        };
    }

    pub fn get_label(&self) -> String
    {
        return self.label.clone();
    }
}

impl LoadValue for Variable
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!("; Load Variable {0:}/{1:} to Register {2:}", self.label, self.name, register));
        assembly.push("jmpri 2".to_string());
        assembly.push(format!(".loadloc {0:}", self.label));
        assembly.push(format!("ldri {0:}, -1", register));

        return assembly;
    }
}
