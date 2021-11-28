use super::common::*;

pub struct StatementWhile
{
    test_expression: Box<dyn LoadValue>,
    while_statements: Vec<Box<dyn EmitAssembly>>
}

impl EmitAssembly for StatementWhile
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>
    {
        let mut assembly = Vec::new();
        let while_label = format!("statement_while_{0:}", scopes.generate_index());

        // Define the test
        assembly.push(format!(":{0:}_test", while_label));

        // Load the expression value into the test array
        assembly.extend(self.test_expression.load_value_to_register(REG_DEFAULT_TEST_RESULT));

        // Start the if statement
        assembly.push(format!("; {0:}", while_label));

        // Load the location values into the correct registers
        assembly.push("jmpri 2".to_string());
        assembly.push(format!(".loadloc {0:}_end", while_label));
        assembly.push(format!("ldir {0:}, -1", REG_DEFAULT_TEST_JUMP_A));

        // Perform the test and jump to end if false
        assembly.push(format!("tz {0:}", REG_DEFAULT_TEST_RESULT));
        assembly.push(format!("jmp {0:}", REG_DEFAULT_TEST_JUMP_A));

        assembly.push(format!("; {0:} Loop", while_label));

        for s in self.while_statements.iter()
        {
            assembly.extend(s.emit(scopes));
        }

        assembly.push("jmp 2".to_string());
        assembly.push(format!(".loadloc {0:}_test", while_label));
        assembly.push("ldir 5, -1".to_string());
        assembly.push("jmp 5".to_string());

        // Ending
        assembly.push(format!(":{0:}_end", while_label));
        return assembly;
    }
}
