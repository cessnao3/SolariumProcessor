use std::{fmt::Display, rc::Rc};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpConv, OpCopy, OpJmp, OpLdn, OpRet, OpSav, OpSub,
    OpTz,
};

use crate::{
    TokenError,
    compiler::{CompilingState, GlobalStatement, ScopeManager, Statement},
    expressions::{Expression, RegisterDef, parse_expression},
    tokenizer::{EndOfTokenStream, Token, TokenIter, get_identifier},
    typing::{Function, Type},
    utilities::load_to_register,
    variables::VariableDefinition,
};

#[derive(Debug)]
pub struct FunctionDefinition {
    name: Token,
    entry_label: String,
    end_label: String,
    statements: Vec<Rc<dyn Statement>>,
    dtype: Function,
    scope_manager: ScopeManager,
}

impl FunctionDefinition {
    pub fn new(
        id: usize,
        name: Token,
        func: Function,
        statements: Vec<Rc<dyn Statement>>,
        scope_manager: ScopeManager,
    ) -> Result<Self, TokenError> {
        let ident = get_identifier(&name)?.to_string();
        Ok(Self {
            name,
            entry_label: format!("func_{id}_{ident}_start"),
            end_label: format!("func_{id}_{ident}_end"),
            statements,
            dtype: func,
            scope_manager,
        })
    }

    pub fn get_entry_label(&self) -> &str {
        &self.entry_label
    }

    pub fn get_token(&self) -> &Token {
        &self.name
    }

    pub fn as_expr(&self) -> Rc<dyn Expression> {
        Rc::new(FunctionLabelExpr {
            name: self.name.clone(),
            dtype: self.dtype.clone(),
            entry_label: self.entry_label.clone(),
        })
    }
}

#[derive(Debug, Clone)]
struct FunctionLabelExpr {
    name: Token,
    dtype: Function,
    entry_label: String,
}

impl Expression for FunctionLabelExpr {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Function(Rc::new(self.dtype.clone())))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
    ) -> Result<Vec<jib_asm::AsmTokenLoc>, TokenError> {
        let asm = vec![
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg.reg,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(self.entry_label.clone()),
        ];
        Ok(self.name.to_asm_iter(asm).into_iter().collect())
    }
}

impl Display for FunctionLabelExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.get_value())
    }
}

impl Statement for FunctionDefinition {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        if !self.dtype.parameters.is_empty() {
            Err(self
                .name
                .clone()
                .into_err("parameters currently unsupported"))
        } else {
            let mut init_asm = vec![
                AsmToken::CreateLabel(self.entry_label.clone()),
                AsmToken::OperationLiteral(Box::new(OpCopy::new(
                    RegisterDef::FN_BASE.into(),
                    Register::StackPointer.into(),
                ))),
            ];

            let scope_size = self.scope_manager.get_max_size();

            if scope_size > 0 {
                init_asm.extend(load_to_register(RegisterDef::SPARE, scope_size as u32));
                init_asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(Register::StackPointer, DataType::U32),
                    Register::StackPointer.into(),
                    RegisterDef::SPARE.into(),
                ))));
            }

            let mut asm = self
                .name
                .to_asm_iter(init_asm)
                .into_iter()
                .collect::<Vec<_>>();

            for s in self.statements.iter() {
                asm.extend_from_slice(&s.get_exec_code()?);
            }

            let mut asm_end = Vec::new();

            asm_end.push(AsmToken::CreateLabel(self.end_label.clone()));
            if scope_size > 0 {
                asm_end.extend(load_to_register(RegisterDef::SPARE, scope_size as u32));
                asm_end.push(AsmToken::OperationLiteral(Box::new(OpSub::new(
                    ArgumentType::new(Register::StackPointer, DataType::U32),
                    Register::StackPointer.into(),
                    RegisterDef::SPARE.into(),
                ))));
            }
            asm_end.push(AsmToken::OperationLiteral(Box::new(OpRet)));

            asm.extend(self.name.to_asm_iter(asm_end));

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
    let name = get_identifier(&name_token)?.to_string();

    if !state.get_scopes().is_empty() {
        return Err(name_token.into_err(format!(
            "unable to start function {name} with non-empty scope"
        )));
    }

    state.get_scopes_mut().add_scope(name_token.clone());

    let func_type = Function::read_tokens(tokens, state, true)?;

    if !func_type.parameters.is_empty() {
        panic!("empty_values")
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
        func_type,
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

        asm.extend(self.test_expr.load_value_to_register(def)?);

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

        asm.extend(self.test_expr.load_value_to_register(def)?);

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

#[derive(Debug, Clone)]
struct ExpressionStatement {
    expr: Rc<dyn Expression>,
}

impl Statement for ExpressionStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        self.expr.load_value_to_register(RegisterDef::default())
    }
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<Option<Rc<dyn Statement>>, TokenError> {
    if let Some(next) = tokens.peek() {
        if next.get_value() == "def" {
            let def = VariableDefinition::parse("def", tokens, state)?;
            Ok(Some(state.get_scopes_mut().add_var(def)?))
        } else if next.get_value() == "const" {
            let def = VariableDefinition::parse("const", tokens, state)?;
            state.get_scopes_mut().add_const(def)?;
            Ok(Some(Rc::new(EmptyStatement)))
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
                statement,
            })))
        } else if next.get_value() == "}" {
            Ok(None)
        } else if next.get_value() == ";" {
            Ok(Some(Rc::new(EmptyStatement)))
        } else {
            let expr = parse_expression(tokens, state)?;
            tokens.expect(";")?;
            Ok(Some(Rc::new(ExpressionStatement { expr })))
        }
    } else {
        Err(EndOfTokenStream.into())
    }
}
