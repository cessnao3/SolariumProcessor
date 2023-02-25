trait CodeComponent {
    fn generate_local_code() -> Vec<String>;

    fn generate_static_code() -> Vec<String>;
}

struct Identifier {
    value: String
}

struct Statement {

}

struct BaseStatement {

}

struct FunctionCall {
    name: Identifier,
    statements: Vec<Box<Statement>>,
}

struct Expression {

}

struct AssignmentOperator {

}

impl CodeComponent for AssignmentOperator {

}

struct VariableOperator {

}
