use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, AssemblerError, OpAdd, OpLd, OpLdi, OpLdn, OpPopr, OpPush,
    OpSav, OpSub, OpTz,
};

use crate::{TokenError, compiler::Statement, expressions::RegisterDef, tokenizer::Token};

pub fn load_to_register(reg: Register, val: u32) -> Box<[AsmToken]> {
    if val <= u16::MAX as u32 {
        Box::new([AsmToken::OperationLiteral(Box::new(OpLdi::new(
            ArgumentType::new(reg, DataType::U16),
            val as u16,
        )))])
    } else {
        Box::new([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(reg, DataType::U32)))),
            AsmToken::Literal4(val),
        ])
    }
}

#[derive(Debug, Clone)]
pub struct MemcpyStatement {
    token: Token,
    from_addr: Register,
    to_addr: Register,
    size: usize,
    id: usize,
}

impl MemcpyStatement {
    fn get_avail_reg(&self, used_regs: &[Register]) -> Result<Register, TokenError> {
        for i in RegisterDef::FIRST_USEABLE.get_index()..=Register::last_register().get_index() {
            let r = Register::GeneralPurpose(i);

            if !used_regs.contains(&r) {
                return Ok(r);
            }
        }

        Err(self
            .token
            .clone()
            .into_err("unable to find available register for memcpy"))
    }

    fn check_assembler<T>(&self, val: Result<T, AssemblerError>) -> Result<T, TokenError> {
        match val {
            Ok(x) => Ok(x),
            Err(e) => Err(self
                .token
                .clone()
                .into_err(format!("unable to create assembler - {e}"))),
        }
    }
}

impl Statement for MemcpyStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        let spare_reg = RegisterDef::SPARE;
        let val_reg = self.get_avail_reg(&[self.from_addr, self.to_addr, spare_reg])?;
        let num_reg = self.get_avail_reg(&[self.from_addr, self.to_addr, spare_reg, val_reg])?;

        asm.extend_from_slice(&load_to_register(spare_reg, self.size as u32));

        asm.push(AsmToken::OperationLiteral(Box::new(OpPush::new(
            self.to_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpPush::new(
            self.from_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpLdi::new(
            ArgumentType::new(num_reg, DataType::U16),
            1,
        ))));

        let label_base = format!("asm_memcpy_{}", self.id);
        let label_loop = format!("{label_base}_loop");
        let label_end = format!("{label_base}_end");

        asm.push(AsmToken::CreateLabel(label_loop.clone()));
        asm.push(AsmToken::OperationLiteral(Box::new(OpTz::new(
            spare_reg.into(),
        ))));
        asm.push(self.check_assembler(AsmToken::try_from(format!("jmpri {label_end}").as_ref()))?);
        asm.push(AsmToken::OperationLiteral(Box::new(OpLd::new(
            ArgumentType::new(val_reg, DataType::U8),
            self.from_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpSav::new(
            ArgumentType::new(self.to_addr, DataType::U8),
            val_reg.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
            ArgumentType::new(self.to_addr, DataType::U32),
            self.to_addr.into(),
            num_reg.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
            ArgumentType::new(self.from_addr, DataType::U32),
            self.from_addr.into(),
            num_reg.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpSub::new(
            ArgumentType::new(spare_reg, DataType::U32),
            spare_reg.into(),
            num_reg.into(),
        ))));
        asm.push(self.check_assembler(AsmToken::try_from(format!("jmpri {label_loop}").as_ref()))?);
        asm.push(AsmToken::CreateLabel(label_end));

        asm.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
            self.from_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
            self.to_addr.into(),
        ))));

        Ok(self.token.to_asm_iter(asm).into_iter().collect())
    }
}
