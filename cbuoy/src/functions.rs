use std::rc::Rc;

use jib::cpu::{DataType, Register};
use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpCopy, OpRet};

use crate::{
    TokenError,
    compiler::{CompilingState, GlobalStatement, ScopeManager, Statement},
    expressions::RegisterDef,
    tokenizer::{Token, TokenIter, get_identifier},
    typing::Type,
    utilities::load_to_register,
};

#[derive(Debug)]
pub struct FunctionDefinition {
    name: Token,
    entry_label: String,
    end_label: String,
    statements: Vec<Rc<dyn Statement>>,
    parameters: Vec<(String, Type)>,
    scope_manager: ScopeManager,
}

impl FunctionDefinition {
    pub fn new(id: usize, name: Token, scope_manager: ScopeManager) -> Result<Self, TokenError> {
        let ident = get_identifier(&name)?;
        Ok(Self {
            name,
            entry_label: format!("func_{id}_{ident}_start"),
            end_label: format!("func_{id}_{ident}_end"),
            statements: Vec::new(),
            parameters: Vec::new(),
            scope_manager,
        })
    }

    pub fn get_token(&self) -> &Token {
        &self.name
    }
}

impl Statement for FunctionDefinition {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        if !self.parameters.is_empty() {
            Err(self
                .name
                .clone()
                .into_err("parameters currently unsupported"))
        } else if !self.statements.is_empty() {
            Err(self
                .name
                .clone()
                .into_err("statements within functions currently unsupported"))
        } else {
            let mut asm = Vec::new();

            asm.push(AsmToken::CreateLabel(self.entry_label.clone()));
            asm.push(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                RegisterDef::FN_BASE.into(),
                Register::StackPointer.into(),
            ))));

            let scope_size = self.scope_manager.scope_full_size()?;

            if scope_size > 0 {
                asm.extend_from_slice(&load_to_register(RegisterDef::SPARE, scope_size as u32));
                asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(Register::StackPointer, DataType::U32),
                    Register::StackPointer.into(),
                    RegisterDef::SPARE.into(),
                ))));
            }

            asm.push(AsmToken::Comment(
                "TODO VARIABLE INIT STATEMENTS HERE!".to_string(),
            ));
            asm.push(AsmToken::CreateLabel(self.end_label.clone()));
            asm.push(AsmToken::OperationLiteral(Box::new(OpRet)));

            Ok(self.name.to_asm_iter(asm).into_iter().collect())
        }
    }
}

impl GlobalStatement for FunctionDefinition {
    fn get_init_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_static_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

pub fn parse_fn_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<(), TokenError> {
    tokens.expect("fn")?;
    let name_token = tokens.next()?;
    let name = get_identifier(&name_token)?;

    if !state.get_scopes().is_empty() {
        return Err(name_token.into_err(format!(
            "unable to start function {name} with non-empty scope"
        )));
    }

    state.get_scopes_mut().add_scope(name_token.clone());

    tokens.expect("(")?;
    let mut params_iter = Vec::new();
    while let Some(t) = tokens.next_if(|t| t != ")") {
        params_iter.push(t);
    }
    tokens.expect(")")?;

    if !params_iter.is_empty() {
        todo!("params not yet supported!");
    }

    let mut return_iter = Vec::new();
    while let Some(t) = tokens.next_if(|t| t != "{") {
        return_iter.push(t);
    }
    tokens.expect("{")?;

    while let Some(s) = parse_statement(tokens, state)? {
        todo!();
    }

    tokens.expect("}")?;

    let def = Rc::new(FunctionDefinition::new(
        state.get_next_id(),
        name_token.clone(),
        state.extract_scope(),
    )?);
    state.add_function(def)
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Option<Rc<dyn Statement>>, TokenError> {
    Ok(None)
}
