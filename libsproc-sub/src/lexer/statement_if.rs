use super::common::*;

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
        let if_label = format!("statement_if_{0:}", scopes.generate_index());

        // Load the expression value into the test array
        assembly.extend(self.test_expression.load_value_to_register(REG_DEFAULT_TEST_RESULT, REG_DEFAULT_SPARE));

        // Start the if statement
        assembly.push(format!("; {0:}", if_label));

        // Load the location values into the correct registers
        assembly.push("jmpri 3".to_string());
        assembly.push(format!(".loadloc {0:}_true", if_label));
        assembly.push(format!(".loadloc {0:}_false", if_label));
        assembly.push(format!("ldir {0:}, -2", REG_DEFAULT_TEST_JUMP_A));
        assembly.push(format!("ldir {0:}, -2", REG_DEFAULT_TEST_JUMP_B));

        // Perform the test and jump to the correct locations
        assembly.push(format!("tnz {0:}", REG_DEFAULT_TEST_RESULT));
        assembly.push(format!("jmp {0:}", REG_DEFAULT_TEST_JUMP_A));
        assembly.push(format!("jmp {0:}", REG_DEFAULT_TEST_JUMP_B));

        // If True

        assembly.push(format!(":{0:}_true", if_label));

        for s in self.true_statements.iter()
        {
            assembly.extend(s.emit(scopes));
        }

        assembly.push("jmp 2".to_string());
        assembly.push(format!(".loadloc {0:}_end", if_label));
        assembly.push(format!("ldir {0:}, -1", REG_DEFAULT_TEST_RESULT));
        assembly.push(format!("jmp {0:}", REG_DEFAULT_TEST_RESULT));

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
