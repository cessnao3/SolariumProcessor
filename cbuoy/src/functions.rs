use std::{fmt::Display, rc::Rc};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpBrk, OpConv, OpCopy, OpJmp, OpLdn, OpRet, OpSub,
    OpTz,
};

use crate::{
    TokenError,
    compiler::{CompilingState, GlobalStatement, ScopeManager, Statement},
    expressions::{
        Expression, ExpressionData, RegisterDef, TemporaryStackTracker, parse_expression,
    },
    tokenizer::{EndOfTokenStream, Token, TokenIter, get_identifier},
    typing::{Function, Type},
    utilities::load_to_register,
    variables::{LocalVariable, VariableDefinition},
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
    pub fn create_label_base(id: usize, name: &Token) -> Result<String, TokenError> {
        let ident = get_identifier(name)?;
        Ok(format!("func_{id}_{ident}"))
    }

    pub fn new(
        label_base: &str,
        name: Token,
        func: Function,
        statements: Vec<Rc<dyn Statement>>,
        scope_manager: ScopeManager,
    ) -> Result<Self, TokenError> {
        Ok(Self {
            name,
            entry_label: format!("{label_base}_start"),
            end_label: format!("{label_base}_end"),
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
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let asm = vec![
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                reg.reg,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(self.entry_label.clone()),
        ];
        Ok(ExpressionData::new(self.name.to_asm_iter(asm)))
    }
}

impl Display for FunctionLabelExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.get_value())
    }
}

impl GlobalStatement for FunctionDefinition {
    fn get_init_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_static_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_func_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut init_asm = vec![
            AsmToken::CreateLabel(self.entry_label.clone()),
            AsmToken::LocationComment(format!("+func({})", self.name)),
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                RegisterDef::FN_VAR_BASE.into(),
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

        let mut stack_requirements = TemporaryStackTracker::default();
        let mut statement_asm = Vec::new();

        for s in self.statements.iter() {
            let mut local_stack = TemporaryStackTracker::default();
            statement_asm.extend(s.get_exec_code(&mut local_stack)?);
            stack_requirements.merge(local_stack);
        }

        init_asm.extend(stack_requirements.get_start_code());

        let mut asm = self
            .name
            .to_asm_iter(init_asm)
            .into_iter()
            .collect::<Vec<_>>();

        asm.extend(statement_asm);

        let mut asm_end = Vec::new();

        asm_end.push(AsmToken::CreateLabel(self.end_label.clone()));
        asm_end.extend(stack_requirements.get_end_code());
        if scope_size > 0 {
            asm_end.extend(load_to_register(RegisterDef::SPARE, scope_size as u32));
            asm_end.push(AsmToken::OperationLiteral(Box::new(OpSub::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                RegisterDef::SPARE.into(),
            ))));
        }
        asm_end.push(AsmToken::OperationLiteral(Box::new(OpRet)));
        asm_end.push(AsmToken::LocationComment(format!("-func({})", self.name)));

        asm.extend(self.name.to_asm_iter(asm_end));

        Ok(asm)
    }
}

pub fn parse_fn_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
) -> Result<(), TokenError> {
    tokens.expect("fn")?;
    let name_token = tokens.next()?;
    get_identifier(&name_token)?;

    state.init_scope(name_token.clone())?;

    let func_type = Function::read_tokens(tokens, state, true)?;

    for p in func_type.parameters.iter() {
        state.get_scopes_mut()?.add_parameter(p.clone())?;
    }

    state.get_scopes_mut()?.add_scope();
    tokens.expect("{")?;

    let mut statements = Vec::new();

    let base_label = FunctionDefinition::create_label_base(state.get_next_id(), &name_token)?;

    while let Some(s) = parse_statement(
        tokens,
        state,
        &format!("{base_label}_end"),
        func_type.return_type.as_ref(),
    )? {
        statements.push(s);
    }

    tokens.expect("}")?;

    let def = Rc::new(FunctionDefinition::new(
        &base_label,
        name_token.clone(),
        func_type,
        statements,
        state.extract_scope()?,
    )?);
    state.add_function(def)
}

#[derive(Debug, Clone)]
struct StatementGroup {
    statements: Vec<Rc<dyn Statement>>,
}

impl Statement for StatementGroup {
    fn get_exec_code(
        &self,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();
        for s in self.statements.iter() {
            let mut local_stack = TemporaryStackTracker::default();
            asm.extend(s.get_exec_code(&mut local_stack)?.into_iter());
            required_stack.merge(local_stack);
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
    fn get_exec_code(
        &self,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let def = RegisterDef::default();

        let label_base = format!("if_statement_{}", self.id);

        let mut asm = vec![self.token.to_asm(AsmToken::Comment(format!(
            "if statement for {} {}",
            self.id, self.token,
        )))];

        asm.extend(
            self.test_expr
                .load_value_to_register(def, required_stack)?
                .into_asm(),
        );

        let false_label = format!("{label_base}_false");

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                def.spare,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(false_label.clone()),
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        let mut true_stack = TemporaryStackTracker::default();
        asm.extend(self.true_statement.get_exec_code(&mut true_stack)?);
        required_stack.merge(true_stack);

        let end_label = format!("{label_base}_end");
        if self.false_statement.is_some() {
            asm.extend(self.token.to_asm_iter([
                AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                    def.reg,
                    DataType::U32,
                )))),
                AsmToken::LoadLoc(end_label.clone()),
                AsmToken::OperationLiteral(Box::new(OpJmp::new(def.reg.into()))),
            ]));
        }

        asm.push(
            self.token
                .to_asm(AsmToken::CreateLabel(format!("{label_base}_false"))),
        );

        if let Some(fe) = &self.false_statement {
            let mut false_stack = TemporaryStackTracker::default();
            asm.extend(fe.get_exec_code(&mut false_stack)?);
            asm.push(self.token.to_asm(AsmToken::CreateLabel(end_label)));
            required_stack.merge(false_stack);
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
    fn get_exec_code(
        &self,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
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

        asm.push(self.token.to_asm(AsmToken::Comment(format!(
            "while test using {}",
            self.test_expr
        ))));
        asm.extend(
            self.test_expr
                .load_value_to_register(def, required_stack)?
                .into_asm(),
        );

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                def.spare,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(format!("{label_base}_end")),
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        let mut statement_stack = TemporaryStackTracker::default();
        asm.extend(self.statement.get_exec_code(&mut statement_stack)?);
        required_stack.merge(statement_stack);

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
    fn get_exec_code(
        &self,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

#[derive(Debug, Clone)]
struct ExpressionStatement {
    expr: Rc<dyn Expression>,
}

impl Statement for ExpressionStatement {
    fn get_exec_code(
        &self,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .expr
            .load_value_to_register(RegisterDef::default(), required_stack)?
            .into_asm())
    }
}

#[derive(Debug, Clone)]
struct DebugBreakpointStatement {
    token: Token,
}

impl Statement for DebugBreakpointStatement {
    fn get_exec_code(
        &self,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(vec![
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpBrk))),
        ])
    }
}

#[derive(Debug, Clone)]
struct ReturnStatementTempVar {
    token: Token,
    temp_var: LocalVariable,
    jump_label: String,
}

impl Statement for ReturnStatementTempVar {
    fn get_exec_code(
        &self,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = self.temp_var.get_exec_code(temporary_stack_tracker)?;

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                RegisterDef::SPARE,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(self.jump_label.clone()),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(RegisterDef::SPARE.into()))),
        ]));

        Ok(asm)
    }
}

#[derive(Debug, Clone)]
struct ReturnStatementRegVar {
    token: Token,
    jump_label: String,
    expr: Option<(DataType, Rc<dyn Expression>)>,
}

impl Statement for ReturnStatementRegVar {
    fn get_exec_code(
        &self,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        asm.extend(
            self.token.to_asm_iter([
                AsmToken::Comment(
                    self.expr
                        .as_ref()
                        .map(|(dt, e)| format!("return ({e}) : {dt}"))
                        .unwrap_or("return".to_string()),
                ),
                AsmToken::LocationComment("return_test_value".to_string()),
                AsmToken::OperationLiteral(Box::new(OpBrk)),
            ]),
        );

        if let Some((ret_type, e)) = &self.expr {
            let expr_type = e.get_primitive_type()?;
            let rd = RegisterDef::default();
            asm.extend(
                e.load_value_to_register(rd, temporary_stack_tracker)?
                    .into_asm(),
            );

            asm.push(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                        Register::Return.into(),
                        rd.reg.into(),
                    )))),
            );

            if expr_type != *ret_type {
                asm.push(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                            ArgumentType::new(Register::Return, *ret_type),
                            ArgumentType::new(Register::Return, expr_type),
                        )))),
                );
            }
        }

        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(
                RegisterDef::SPARE,
                DataType::U32,
            )))),
            AsmToken::LoadLoc(self.jump_label.clone()),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(RegisterDef::SPARE.into()))),
        ]));

        Ok(asm)
    }
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
    return_jump_label: &str,
    return_type: Option<&Type>,
) -> Result<Option<Rc<dyn Statement>>, TokenError> {
    if let Some(next) = tokens.peek() {
        if next.get_value() == "def" {
            let def = VariableDefinition::parse("def", tokens, state)?;
            Ok(Some(state.get_scopes_mut()?.add_var(def)?))
        } else if next.get_value() == "const" {
            let def = VariableDefinition::parse("const", tokens, state)?;
            state.get_scopes_mut()?.add_const(def)?;
            Ok(Some(Rc::new(EmptyStatement)))
        } else if next.get_value() == "{" {
            tokens.expect("{")?;
            state.get_scopes_mut()?.add_scope();
            let mut statements = Vec::new();
            while let Some(s) = parse_statement(tokens, state, return_jump_label, return_type)? {
                statements.push(s);
            }
            tokens.expect("}")?;
            state.get_scopes_mut()?.remove_scope()?;
            Ok(Some(Rc::new(StatementGroup { statements })))
        } else if next.get_value() == "if" {
            let if_token = tokens.expect("if")?;
            let id = state.get_next_id();
            tokens.expect("(")?;
            let test_expr = parse_expression(tokens, state)?;
            tokens.expect(")")?;

            let true_statement =
                match parse_statement(tokens, state, return_jump_label, return_type)? {
                    Some(s) => s,
                    None => {
                        return Err(
                            if_token.into_err("must have a valid statement after if expression")
                        );
                    }
                };

            let false_statement = if tokens.expect_peek("else") {
                tokens.expect("else")?;
                parse_statement(tokens, state, return_jump_label, return_type)?
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

            let statement = match parse_statement(tokens, state, return_jump_label, return_type)? {
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
        } else if next.get_value() == "brkpt" {
            let tok = tokens.expect("brkpt")?;
            tokens.expect(";")?;
            Ok(Some(Rc::new(DebugBreakpointStatement { token: tok })))
        } else if next.get_value() == "return" {
            let tok = tokens.expect("return")?;

            let ret_statement: Rc<dyn Statement> = if let Some(rt) = return_type {
                if let Some(pt) = rt.primitive_type() {
                    Rc::new(ReturnStatementRegVar {
                        token: tok,
                        jump_label: return_jump_label.to_string(),
                        expr: Some((pt, parse_expression(tokens, state)?)),
                    })
                } else {
                    let temp_var = LocalVariable::new_unchecked(
                        tok.clone(),
                        rt.clone(),
                        Register::Return,
                        0,
                        Some(parse_expression(tokens, state)?),
                    );
                    Rc::new(ReturnStatementTempVar {
                        jump_label: return_jump_label.to_string(),
                        token: tok,
                        temp_var,
                    })
                }
            } else {
                Rc::new(ReturnStatementRegVar {
                    token: tok,
                    jump_label: return_jump_label.to_string(),
                    expr: None,
                })
            };

            tokens.expect(";")?;
            Ok(Some(ret_statement))
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
