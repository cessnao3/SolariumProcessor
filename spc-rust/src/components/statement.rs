struct Statement { // For things within statement groups

}

struct StatementGroup {
    statements: Vec<Statement>
}

struct BaseStatement { // For things like globals, functions
    next: Box<Statement>
}
