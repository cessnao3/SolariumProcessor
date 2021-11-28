use super::common::*;

pub enum BinaryExpressionType
{
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    Equal,
    NotEqual
}

impl ToString for BinaryExpressionType
{
    fn to_string(&self) -> String
    {
        let s = match self
        {
            BinaryExpressionType::Addition => "+",
            BinaryExpressionType::Subtraction => "-",
            BinaryExpressionType::Multiplication => "*",
            BinaryExpressionType::Division => "/",
            BinaryExpressionType::Greater => ">",
            BinaryExpressionType::Less => "<",
            BinaryExpressionType::GreaterEqual => ">=",
            BinaryExpressionType::LessEqual => "<=",
            BinaryExpressionType::And => "&&",
            BinaryExpressionType::Or => "||",
            BinaryExpressionType::BitwiseAnd => "&",
            BinaryExpressionType::BitwiseOr => "|",
            BinaryExpressionType::Equal => "==",
            BinaryExpressionType::NotEqual => "!="
        };

        return s.to_string();
    }
}

pub struct BinaryExpression
{
    left: Box<dyn LoadValue>,
    right: Box<dyn LoadValue>,
    op_type: BinaryExpressionType
}

impl ToString for BinaryExpression
{
    fn to_string(&self) -> String
    {
        return format!(
            "({0:} {1:} {2:})",
            self.left.to_string(),
            self.op_type.to_string(),
            self.right.to_string());
    }
}

impl LoadValue for BinaryExpression
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>
    {
        // Define the assembly output
        let mut assembly = Vec::new();
        assembly.push(format!("; {0:}", self.to_string()));

        // Define the next register to use
        let reg_other = register + 1;

        // Load the right value to the final register, and left into the next register up
        self.right.load_value_to_register(register);
        self.left.load_value_to_register(reg_other);

        // Apply the correct operation
        match self.op_type
        {
            BinaryExpressionType::Addition |
            BinaryExpressionType::Subtraction |
            BinaryExpressionType::Multiplication |
            BinaryExpressionType::Division |
            BinaryExpressionType::BitwiseAnd |
            BinaryExpressionType::BitwiseOr |
            BinaryExpressionType::And |
            BinaryExpressionType::Or =>
            {
                let arith_inst = match self.op_type
                {
                    BinaryExpressionType::Addition => "add",
                    BinaryExpressionType::Subtraction => "sub",
                    BinaryExpressionType::Multiplication => "mul",
                    BinaryExpressionType::Division => "div",
                    BinaryExpressionType::BitwiseAnd => "band",
                    BinaryExpressionType::BitwiseOr => "bor",
                    BinaryExpressionType::And => "band",
                    BinaryExpressionType::Or => "bor",
                    _ => panic!()
                };

                assembly.push(format!("{0:} {1:}, {1:}, {2:}", arith_inst, register, reg_other));

                match self.op_type
                {
                    BinaryExpressionType::And |
                    BinaryExpressionType::Or =>
                    {
                        assembly.push(format!("bool {0:}", register))
                    }
                    _ => ()
                }
            },
            BinaryExpressionType::Greater |
            BinaryExpressionType::Less |
            BinaryExpressionType::GreaterEqual |
            BinaryExpressionType::LessEqual |
            BinaryExpressionType::Equal |
            BinaryExpressionType::NotEqual =>
            {
                assembly.push(format!("tg {0:}, {1:}", register, reg_other));
                assembly.push(format!("ldi {0:}, 1", register));
                assembly.push(format!("ldi {0:}, 0", register));

                let test_inst = match self.op_type
                {
                    BinaryExpressionType::Greater => "tg",
                    BinaryExpressionType::GreaterEqual => "tge",
                    BinaryExpressionType::Less => "tl",
                    BinaryExpressionType::LessEqual => "tle",
                    BinaryExpressionType::Equal |
                    BinaryExpressionType::NotEqual => "teq",
                    _ => panic!()
                };

                assembly.push(format!("{0:} {1:}, {2:}", test_inst, register, reg_other));
                assembly.push("jmpri 3".to_string());
                assembly.push(format!("ldi {0:}, 0", register));
                assembly.push("jmpri 2".to_string());
                assembly.push(format!("ldi {0:}, 1", register));

                match self.op_type
                {
                    BinaryExpressionType::NotEqual =>
                    {
                        assembly.push(format!("bnot {0:}", register));
                    },
                    _ => ()
                }
            }
        }

        return assembly;
    }
}
