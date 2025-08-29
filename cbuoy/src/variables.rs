use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpConv, OpCopy, OpLd, OpSav};

use crate::{
    TokenError,
    compiler::{CompilingState, GlobalStatement, Statement},
    expressions::{Expression, RegisterDef, parse_expression},
    literals::Literal,
    tokenizer::{Token, TokenIter, get_identifier},
    typing::Type,
    utilities::{MemcpyStatement, load_to_register},
};

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    name: Token,
    dtype: Type,
    label: Rc<str>,
    init_expr: Option<Rc<dyn Expression>>,
}

impl GlobalVariable {
    pub fn new(
        token: Token,
        id: usize,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<Self, TokenError> {
        let label = format!("global_variable_{}_{}", id, get_identifier(&token)?).into();
        Ok(Self {
            name: token,
            dtype,
            label,
            init_expr,
        })
    }

    pub fn access_label(&self) -> &str {
        self.label.as_ref()
    }

    pub fn get_name(&self) -> &str {
        self.name.get_value()
    }

    fn to_token_loc<T: IntoIterator<Item = AsmToken>>(
        &self,
        it: T,
    ) -> impl Iterator<Item = AsmTokenLoc> {
        it.into_iter().map(|x| self.name.to_asm(x))
    }
}

impl Display for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl Expression for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.dtype.clone())
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        if let Some(p) = self.dtype.primitive_type() {
            let vals = [
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg.reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLd::new(
                    ArgumentType::new(reg.reg, p),
                    reg.reg.into(),
                ))),
            ];
            Ok(self.to_token_loc(vals).collect())
        } else {
            Err(self.name.clone().into_err(format!(
                "unable to load value for {} to register",
                self.dtype
            )))
        }
    }

    fn load_address_to_register(&self, reg: RegisterDef) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .to_token_loc([
                AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
                    reg.reg,
                    jib::cpu::DataType::U32,
                )))),
                AsmToken::LoadLoc(self.access_label().into()),
            ])
            .collect())
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariableStatement {
    global_var: Rc<GlobalVariable>,
}

impl GlobalVariableStatement {
    pub fn new(var: Rc<GlobalVariable>) -> Self {
        Self {
            global_var: var.clone(),
        }
    }

    fn simplified_literal(&self) -> Option<Literal> {
        self.global_var.init_expr.clone().and_then(|x| x.simplify())
    }
}

impl Statement for GlobalVariableStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

impl GlobalStatement for GlobalVariableStatement {
    fn get_static_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let name = self.global_var.get_token();
        let var = &self.global_var;
        let mut asm_static = Vec::new();

        asm_static.push(name.to_asm(AsmToken::CreateLabel(var.access_label().into())));

        if let Some(a) = self.simplified_literal() {
            asm_static.push(
                name.to_asm(
                    a.get_value()
                        .convert(var.get_primitive_type()?)
                        .as_asm_literal(),
                ),
            );
        } else {
            // Create space for the variable
            let mut needed_size = var.get_type()?.byte_size();
            while needed_size > 0 {
                let tok = if needed_size >= 4 {
                    needed_size -= 4;
                    AsmToken::Literal4(0)
                } else if needed_size >= 2 {
                    needed_size -= 2;
                    AsmToken::Literal2(0)
                } else {
                    needed_size -= 1;
                    AsmToken::Literal1(0)
                };

                asm_static.push(name.to_asm(tok));
            }
        }

        Ok(asm_static)
    }

    fn get_init_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm_init = Vec::new();

        if self.simplified_literal().is_none()
            && let Some(init_expr) = &self.global_var.init_expr
        {
            let name = self.global_var.get_token();

            if let Ok(init_type) = init_expr.get_primitive_type() {
                let var = &self.global_var;

                // TODO - Replace with a better expression (operation expression value?)
                let reg_state_var = RegisterDef::default();
                let reg_state_init = reg_state_var.increment_token(name)?;

                asm_init.push(name.to_asm(AsmToken::Comment(format!(
                    "Initializing Global Variable {}",
                    self.global_var.get_name()
                ))));

                asm_init.extend_from_slice(&var.load_address_to_register(reg_state_var)?);
                asm_init.extend_from_slice(&init_expr.load_value_to_register(reg_state_init)?);

                let reg_init = reg_state_init.reg;
                let reg_var = reg_state_var.reg;

                let var_type = var.get_primitive_type()?;

                if init_type != var_type {
                    asm_init.push(name.to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                        ArgumentType::new(reg_init, init_type),
                        ArgumentType::new(reg_init, var_type),
                    )))));
                }

                asm_init.push(name.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                    ArgumentType::new(reg_var, var_type),
                    reg_init.into(),
                )))));
            } else {
                return Err(name
                    .clone()
                    .into_err("type does not have a valid primitive type"));
            }
        }

        Ok(asm_init)
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    token: Token,
    dtype: Type,
    base: Register,
    offset: usize,
    init_expr: Option<Rc<dyn Expression>>,
}

impl LocalVariable {
    pub fn new(
        name: Token,
        dtype: Type,
        base: Register,
        offset: usize,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<Self, TokenError> {
        get_identifier(&name)?;
        Ok(Self {
            token: name,
            dtype,
            base,
            offset,
            init_expr,
        })
    }
}

impl Statement for LocalVariable {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        if let Some(var_type) = self.dtype.primitive_type() {
            if let Some(e) = &self.init_expr {
                let expr_type = e.get_primitive_type()?;

                let def = RegisterDef::default();
                let load_val = def.increment_token(&self.token)?;

                asm.push(self.token.to_asm(AsmToken::Comment(format!(
                    "initializing local variable \"{}\" with offset {} from {}",
                    self.token.get_value(),
                    self.offset,
                    self.base
                ))));

                let addr_reg = if self.offset > 0 {
                    asm.extend_from_slice(&self.load_address_to_register(def)?);
                    def.reg
                } else {
                    self.base
                };
                asm.extend_from_slice(&e.load_value_to_register(load_val)?);

                if var_type != expr_type {
                    asm.push(
                        self.token
                            .to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                                ArgumentType::new(load_val.reg, var_type),
                                ArgumentType::new(load_val.reg, expr_type),
                            )))),
                    );
                }

                asm.push(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                            ArgumentType::new(addr_reg, var_type),
                            load_val.reg.into(),
                        )))),
                );
            }
        } else if let Some(e) = &self.init_expr
            && let Ok(t) = e.get_type()
        {
            if t == self.dtype {
                let def = RegisterDef::default();
                let load_val = def.increment_token(&self.token)?;

                let local_reg = if self.offset > 0 {
                    asm.extend_from_slice(&self.load_address_to_register(def)?);
                    def.reg
                } else {
                    self.base
                };

                asm.extend_from_slice(&e.load_address_to_register(load_val)?);

                let mem = MemcpyStatement::new(
                    self.token.clone(),
                    load_val.reg,
                    local_reg,
                    self.dtype.byte_size(),
                );

                asm.extend_from_slice(&mem.get_exec_code()?);
            } else {
                return Err(self.token.clone().into_err("mismatch in data type"));
            }
        } else {
            return Err(self.token.clone().into_err(
                "unable to obtain a valid type for the provided variable init expression",
            ));
        }

        Ok(asm)
    }
}

impl Display for LocalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.get_value())
    }
}

impl Expression for LocalVariable {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.dtype.clone())
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        if self.offset > 0 {
            asm.extend_from_slice(&load_to_register(reg.reg, self.offset as u32));
            asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(reg.reg, DataType::U32),
                reg.reg.into(),
                self.base.into(),
            ))));
        } else {
            asm.push(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                reg.reg.into(),
                self.base.into(),
            ))))
        }

        Ok(self.token.to_asm_iter(asm).into_iter().collect())
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        if let Some(dt) = self.dtype.primitive_type() {
            let mut asm = self.load_address_to_register(reg)?;
            asm.push(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                        ArgumentType::new(reg.reg, dt),
                        reg.reg.into(),
                    )))),
            );
            Ok(asm)
        } else {
            Err(self
                .token
                .clone()
                .into_err("unable to move non-primitive type into register"))
        }
    }
}

pub struct VariableDefinition {
    pub token: Token,
    pub dtype: Type,
    pub init_expr: Option<Rc<dyn Expression>>,
}

impl VariableDefinition {
    pub fn parse(
        def_name: &str,
        tokens: &mut TokenIter,
        state: &mut CompilingState,
    ) -> Result<Self, TokenError> {
        tokens.expect(def_name)?;
        let name_token = tokens.next()?;
        tokens.expect(":")?;

        let dtype = Type::read_type(tokens, state)?;
        let init_expr = if tokens.expect_peek("=") {
            tokens.next()?;
            let mut expr_tokens = Vec::new();
            while let Some(t) = tokens.next_if(|s| s != ";") {
                expr_tokens.push(t);
            }
            Some(parse_expression(&mut TokenIter::from(&expr_tokens), state)?)
        } else {
            None
        };

        tokens.expect(";")?;

        Ok(Self {
            token: name_token,
            dtype,
            init_expr,
        })
    }

    pub fn into_literal(self) -> Result<Literal, TokenError> {
        if let Some(expr) = self.init_expr {
            if let Some(lit) = expr.simplify() {
                let dt = match self.dtype.primitive_type() {
                    Some(x) => x,
                    None => {
                        return Err(self
                            .token
                            .into_err("constant must have a primitive data type"));
                    }
                };

                Ok(Literal::new(self.token, lit.get_value().convert(dt)))
            } else {
                Err(self
                    .token
                    .into_err("constant must have a constant expression value"))
            }
        } else {
            Err(self
                .token
                .into_err("constant must have initialization statement"))
        }
    }
}
