use crate::components::{types::TypeInfo, CompilerError, CompilerState, CodeComponent};

use super::Expression;

struct VariableExpression {
    name: String,
    var_type: TypeInfo,
}

impl ToString for VariableExpression {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl CodeComponent for VariableExpression {
    fn generate_code(&self, state: &mut CompilerState) -> Result<(), CompilerError> {
        Err(CompilerError::new("not implemented"))
    }
}

impl Expression for VariableExpression {
    fn evaluate_to_register(&self, target: usize, state: &mut CompilerState) -> Result<Vec<String>, CompilerError> {
        Err(CompilerError::new("not implemented"))
    }

    fn get_type(&self) -> Result<TypeInfo, CompilerError> {
        return Ok(self.var_type.clone())
    }
}
