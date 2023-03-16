mod binary_expression;
mod function_call;
mod unary_expression;
mod variable;

use super::{CodeComponent, CompilerError, CompilerState};
use super::types::TypeInfo;

trait Expression: CodeComponent {
    fn evaluate_to_register(&self, target: usize, state: &mut CompilerState) -> Result<Vec<String>, CompilerError>;

    fn get_type(&self) -> Result<TypeInfo, CompilerError>;
}
