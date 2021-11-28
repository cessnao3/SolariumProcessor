use super::common::{EmitAssembly, ScopeManager, LoadValue};

pub struct IfStatement
{
    test_expression: Box<dyn LoadValue>,
    true_statements: Vec<Box<dyn EmitAssembly>>,
    false_statements: Vec<Box<dyn EmitAssembly>>
}

impl EmitAssembly for IfStatement
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>
    {
        // Define the assembly value
        let mut assembly = Vec::new();
        let if_label = format!("statement_if_{0:}", scopes.generate_index_name());

        // Load the expression value into the test array
        assembly.extend(self.test_expression.load_value_to_register(5));

        // Start the if statement
        assembly.push(format!("; {0:}", if_label));

        // Load the location values into the correct registers
        assembly.push("jmpri 3".to_string());
        assembly.push(format!(".loadloc {0:}_true", if_label));
        assembly.push(format!(".loadloc {0:}_false", if_label));
        assembly.push("ldir 6, -2".to_string());
        assembly.push("ldir 7, -2".to_string());

        // Perform the test and jump to the correct locations
        assembly.push("tnz 5".to_string());
        assembly.push("jmp 6".to_string());
        assembly.push("jmp 7".to_string());

        // If True

        assembly.push(format!(":{0:}_true", if_label));

        for s in self.true_statements.iter()
        {
            assembly.extend(s.emit(scopes));
        }

        assembly.push("jmp 2".to_string());
        assembly.push(format!(".loadloc {0:}_end", if_label));
        assembly.push("ldir 5, -1".to_string());
        assembly.push("jmp 5".to_string());

        // If False
        assembly.push(format!(":{0:}_false", if_label));

        for s in self.false_statements.iter()
        {
            assembly.extend(s.emit(scopes));
        }

        // Ending
        assembly.push(format!(":{0:}_end", if_label));

        return assembly;
    }
}
