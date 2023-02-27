

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
