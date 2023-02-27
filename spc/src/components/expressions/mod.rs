mod binary_expression;
mod function_call;
mod unary_expression;
mod variable;

use super::CodeComponent;

trait Expression: CodeComponent {
    fn store_result_to_register(&self);

    fn get_type(&self) -> String;
}
