use super::Expression;

struct VariableExpression {
    name: String
}

impl ToString for VariableExpression {
    fn to_string(&self) -> String {
        self.name
    }
}

impl Expression for VariableExpression {

}
