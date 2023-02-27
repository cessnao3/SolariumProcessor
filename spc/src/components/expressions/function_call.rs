use super::Expression;

struct FunctionCallExpression {
    name: String,
    arguments: Vec<Box<dyn Expression>>
}
