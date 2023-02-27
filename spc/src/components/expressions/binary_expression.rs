use super::Expression;

enum BinaryExpressionType {
    Assignment,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    BitShiftRight,
    BitShiftLeft,
}

impl ToString for BinaryExpressionType {
    fn to_string(&self) -> String {
        match *self {
            Self::Assignment => "=",
            Self::GreaterThan => ">",
            Self::GreaterThanEqual => ">=",
            Self::LessThan => "<",
            Self::LessThanEqual => "<=",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::BinaryAnd => "&",
            Self::BinaryOr => "|",
            Self::BinaryXor => "^", // TODO: Keep?
            Self::BitShiftRight => ">>",
            Self::BitShiftLeft => "<<",
        }.to_string()
    }
}

struct BinaryExpression {
    expr: BinaryExpressionType,
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
}
