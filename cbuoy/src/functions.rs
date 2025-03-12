use std::rc::Rc;

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpConv, OpCopy, OpJmp, OpLdn, OpRet, OpSav, OpTnz,
    OpTz,
};

use crate::{
    TokenError,
    compiler::{CompilingState, GlobalStatement, ScopeManager, Statement},
    expressions::{Expression, RegisterDef, parse_expression},
    parser::parse_generic_var,
    tokenizer::{EndOfTokenStream, Token, TokenIter, get_identifier},
    typing::Type,
    utilities::load_to_register,
    variables::LocalVariable,
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
    pub fn new(
        id: usize,
        name: Token,
        statements: Vec<Rc<dyn Statement>>,
        scope_manager: ScopeManager,
    ) -> Result<Self, TokenError> {
        let ident = get_identifier(&name)?;
        Ok(Self {
            name,
            entry_label: format!("func_{id}_{ident}_start"),
            end_label: format!("func_{id}_{ident}_end"),
            statements,
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
        } else {
            let mut asm = Vec::new();

            asm.push(
                self.name
                    .to_asm(AsmToken::CreateLabel(self.entry_label.clone())),
            );
            asm.push(
                self.name
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                        RegisterDef::FN_BASE.into(),
                        Register::StackPointer.into(),
                    )))),
            );

            let scope_size = self.scope_manager.scope_full_size()?;

            if scope_size > 0 {
                asm.extend(self.name.to_asm_iter(
                    load_to_register(RegisterDef::SPARE, scope_size as u32).into_iter(),
                ));
                asm.push(
                    self.name
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                            ArgumentType::new(Register::StackPointer, DataType::U32),
                            Register::StackPointer.into(),
                            RegisterDef::SPARE.into(),
                        )))),
                );
            }

            for s in self.statements.iter() {
                asm.extend_from_slice(&s.get_exec_code()?);
            }

            asm.push(
                self.name
                    .to_asm(AsmToken::CreateLabel(self.end_label.clone())),
            );
            asm.push(
                self.name
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpRet))),
            );

            Ok(asm)
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

    let mut statements = Vec::new();

    while let Some(s) = parse_statement(tokens, state)? {
        statements.push(s);
    }

    tokens.expect("}")?;

    let def = Rc::new(FunctionDefinition::new(
        state.get_next_id(),
        name_token.clone(),
        statements,
        state.extract_scope(),
    )?);
    state.add_function(def)
}

#[derive(Debug, Clone)]
struct StatementGroup {
    statements: Vec<Rc<dyn Statement>>,
}

impl Statement for StatementGroup {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();
        for s in self.statements.iter() {
            asm.extend(s.get_exec_code()?.into_iter());
        }
        Ok(asm)
    }
}

#[derive(Debug, Clone)]
struct IfStatement {
    token: Token,
    id: usize,
    test_expr: Rc<dyn Expression>,
    true_statement: Rc<dyn Statement>,
    false_statement: Option<Rc<dyn Statement>>,
}

impl Statement for IfStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let def = RegisterDef::default();

        let label_base = format!("if_statement_{}", self.id);

        let mut asm = vec![self.token.to_asm(AsmToken::Comment(format!(
            "if statement for {} {}",
            self.id, self.token,
        )))];

        asm.extend(self.test_expr.load_value_to_register(def)?.into_iter());

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                def.spare,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(format!("{label_base}_false")),
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        asm.extend(self.true_statement.get_exec_code()?);

        asm.push(
            self.token
                .to_asm(AsmToken::CreateLabel(format!("{label_base}_false"))),
        );

        if let Some(fe) = &self.false_statement {
            asm.extend(fe.get_exec_code()?);
        }

        Ok(asm)
    }
}

#[derive(Debug, Clone)]
struct WhileStatement {
    token: Token,
    id: usize,
    test_expr: Rc<dyn Expression>,
    statement: Rc<dyn Statement>,
}

impl Statement for WhileStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let def = RegisterDef::default();

        let label_base = format!("while_statement_{}", self.id);

        let mut asm = vec![
            self.token.to_asm(AsmToken::Comment(format!(
                "while statement for {} {}",
                self.id, self.token,
            ))),
            self.token
                .to_asm(AsmToken::CreateLabel(format!("{label_base}_test"))),
        ];

        asm.extend(self.test_expr.load_value_to_register(def)?.into_iter());

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                def.spare,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(format!("{label_base}_end")),
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        asm.extend(self.statement.get_exec_code()?);

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                def.spare,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(format!("{label_base}_test")),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        asm.push(
            self.token
                .to_asm(AsmToken::CreateLabel(format!("{label_base}_end"))),
        );

        Ok(asm)
    }
}

#[derive(Default, Debug, Clone)]
struct EmptyStatement;

impl Statement for EmptyStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Option<Rc<dyn Statement>>, TokenError> {
    if let Some(next) = tokens.peek() {
        if next.get_value() == "def" {
            let def = parse_generic_var("def", tokens, state)?;
            Ok(Some(state.get_scopes_mut().add_var(def)?))
        } else if next.get_value() == "const" {
            let def = parse_generic_var("const", tokens, state)?;
            Ok(Some(state.get_scopes_mut().add_const(def)?))
        } else if next.get_value() == "{" {
            state.get_scopes_mut().add_scope(tokens.expect("{")?);
            let mut statements = Vec::new();
            while let Some(s) = parse_statement(tokens, state)? {
                statements.push(s);
            }
            tokens.expect("}")?;
            state.get_scopes_mut().remove_scope()?;
            Ok(Some(Rc::new(StatementGroup { statements })))
        } else if next.get_value() == "if" {
            let if_token = tokens.expect("if")?;
            let id = state.get_next_id();
            tokens.expect("(")?;
            let test_expr = parse_expression(tokens, state)?;
            tokens.expect(")")?;

            let true_statement = match parse_statement(tokens, state)? {
                Some(s) => s,
                None => {
                    return Err(
                        if_token.into_err("must have a valid statement after if expression")
                    );
                }
            };

            let false_statement = if tokens.expect_peek("else") {
                tokens.expect("else")?;
                parse_statement(tokens, state)?
            } else {
                None
            };

            Ok(Some(Rc::new(IfStatement {
                token: if_token,
                id,
                test_expr,
                true_statement,
                false_statement,
            })))
        } else if next.get_value() == "while" {
            let while_token = tokens.expect("while")?;
            let id = state.get_next_id();
            tokens.expect("(")?;
            let test_expr = parse_expression(tokens, state)?;
            tokens.expect(")")?;

            let statement = match parse_statement(tokens, state)? {
                Some(s) => s,
                None => {
                    return Err(while_token.into_err("while statement must have a valid statement"));
                }
            };

            Ok(Some(Rc::new(WhileStatement {
                token: while_token,
                id,
                test_expr,
                statement: statement,
            })))
        } else if next.get_value() == "}" {
            Ok(None)
        } else if next.get_value() == ";" {
            Ok(Some(Rc::new(EmptyStatement)))
        } else {
            match parse_assignment_statement(tokens, state) {
                Ok(s) => Ok(Some(s)),
                Err(e) => Err(e),
            }
        }
    } else {
        Err(EndOfTokenStream.into())
    }
}

#[derive(Debug, Clone)]
struct AssignmentStatement {
    addr_expr: Rc<dyn Expression>,
    val_expr: Rc<dyn Expression>,
}

impl Statement for AssignmentStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let tok = self.addr_expr.get_token();

        let mut asm = Vec::new();
        asm.push(self.addr_expr.get_token().to_asm(AsmToken::Comment(format!(
            "assignment of {} to {}",
            self.addr_expr, self.val_expr
        ))));

        if let Some(dest_dtype) = self.addr_expr.get_type()?.primitive_type() {
            let val_dtype = self.val_expr.get_primitive_type()?;

            let addr_def = RegisterDef::default();
            let val_def = addr_def.increment_token(tok)?;

            asm.extend(
                self.addr_expr
                    .load_address_to_register(addr_def)?
                    .into_iter(),
            );
            asm.extend(self.val_expr.load_value_to_register(val_def)?.into_iter());

            if val_dtype != dest_dtype {
                asm.push(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                    ArgumentType::new(val_def.reg, dest_dtype),
                    ArgumentType::new(val_def.reg, val_dtype),
                )))));
            }

            asm.push(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                ArgumentType::new(addr_def.reg, dest_dtype),
                val_def.reg.into(),
            )))));

            Ok(asm)
        } else {
            Err(tok
                .clone()
                .into_err("currently struct assignment is unsupported"))
        }
    }
}

fn parse_assignment_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Rc<dyn Statement>, TokenError> {
    let mut init_tokens = Vec::new();
    while !tokens.expect_peek("=") {
        init_tokens.push(tokens.next()?);
    }

    tokens.expect("=")?;

    let mut end_tokens = Vec::new();

    while !tokens.expect_peek(";") {
        end_tokens.push(tokens.next()?);
    }

    tokens.expect(";")?;

    let addr_expr = parse_expression(&mut TokenIter::from(&init_tokens), state)?;
    let val_expr = parse_expression(&mut TokenIter::from(&end_tokens), state)?;

    Ok(Rc::new(AssignmentStatement {
        addr_expr,
        val_expr,
    }))
}
