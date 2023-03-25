use crate::components::{CodeComponent, CompilerError, CompilerState, types::TypeInfo};

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

impl Expression for BinaryExpression {
    fn evaluate_to_register(&self, target: usize, state: &mut CompilerState) -> Result<Vec<String>, CompilerError> {
        Err(CompilerError::not_implemented())
    }

    fn get_type(&self) -> Result<TypeInfo, CompilerError> {
        if self.lhs.get_type()? != self.rhs.get_type()? {
            Err(CompilerError::new(&format!("mismatch in types: {} != {}", self.lhs.get_type()?.name, self.rhs.get_type()?.name)))
        } else {
            Ok(self.lhs.get_type()?.clone())
        }
    }
}

impl CodeComponent for BinaryExpression {
    fn generate_code(&self, state: &mut CompilerState) -> Result<(), CompilerError> {
        Err(CompilerError::not_implemented())
    }
}

impl ToString for BinaryExpression {
    fn to_string(&self) -> String {
        format!("({} {} {})", self.lhs.to_string(), self.expr.to_string(), self.rhs.to_string())
    }
}
