use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Debug,
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, LocationInfo, OpCall, OpJmpri, OpLdi, OpLdn, OpSub,
};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef},
    functions::FunctionDefinition,
    literals::Literal,
    tokenizer::{Token, get_identifier},
    typing::StructDefinition,
    utilities::load_to_register,
    variables::{GlobalVariable, GlobalVariableStatement, LocalVariable, VariableDefinition},
};

pub trait Statement: Debug {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

pub trait GlobalStatement: Statement {
    fn get_init_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
    fn get_static_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

#[derive(Debug, Clone)]
enum GlobalType {
    Variable(Rc<GlobalVariable>),
    Function(Rc<FunctionDefinition>),
    Constant(Rc<Literal>),
    Structure(Rc<StructDefinition>),
}

impl GlobalType {
    fn get_token(&self) -> &Token {
        match self {
            Self::Variable(v) => v.get_token(),
            Self::Function(v) => v.get_token(),
            Self::Constant(v) => v.get_token(),
            Self::Structure(s) => s.get_token(),
        }
    }

    fn get_statement(&self) -> Option<Rc<dyn GlobalStatement>> {
        match self {
            Self::Variable(var) => Some(Rc::new(GlobalVariableStatement::new(var.clone()))),
            Self::Function(func) => Some(func.clone()),
            Self::Constant(_) => None,
            Self::Structure(_) => None,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ScopeManager {
    scopes: Vec<ScopeBlock>,
    max_size: usize,
}

impl ScopeManager {
    pub fn add_scope(&mut self, token: Token) {
        self.scopes.push(ScopeBlock::new(token));
    }

    pub fn remove_scope(&mut self) -> Result<Box<ScopeRemoveStatement>, TokenError> {
        if let Some(s) = self.scopes.pop() {
            let size = s.size()?;
            Ok(Box::new(ScopeRemoveStatement {
                token: s.token,
                size,
            }))
        } else {
            Err(TokenError {
                token: None,
                msg: "already removed all scopes".to_string(),
            })
        }
    }

    pub fn get_max_size(&self) -> usize {
        self.max_size
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.max_size = 0;
    }

    pub fn scope_full_size(&self) -> Result<usize, TokenError> {
        self.scopes.iter().map(|x| x.size()).sum()
    }

    pub fn add_var(&mut self, def: VariableDefinition) -> Result<Rc<LocalVariable>, TokenError> {
        let ident = get_identifier(&def.token)?.to_string();
        let offset = self.scope_full_size()?;

        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => {
                    let var_size = def.dtype.byte_size();
                    let var = Rc::new(LocalVariable::new(
                        def.token,
                        def.dtype,
                        RegisterDef::FN_BASE,
                        offset,
                        def.init_expr,
                    )?);

                    // Update the scope values and maximum size of the stack
                    e.insert(ScopeVariables::Local(var.clone()));
                    self.max_size = self.max_size.max(offset + var_size);
                    Ok(var)
                }
                Entry::Occupied(_) => Err(def
                    .token
                    .into_err("dupliate variable name exists within the same scope")),
            }
        } else {
            Err(def
                .token
                .into_err("unable to add variable without a scope block"))
        }
    }

    pub fn add_const(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let ident = get_identifier(&def.token)?.to_string();
        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => {
                    let literal_expr = Rc::new(def.into_literal()?);
                    e.insert(ScopeVariables::Const(literal_expr.clone()));
                    Ok(())
                }
                Entry::Occupied(_) => Err(def
                    .token
                    .into_err("dupliate variable name exists within the same scope")),
            }
        } else {
            Err(def
                .token
                .into_err("unable to add variable without a scope block"))
        }
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?;

        for s in self.scopes.iter().rev() {
            if let Some(var) = s.variables.get(ident) {
                return Ok(var.get_expr());
            }
        }

        Err(name
            .clone()
            .into_err("no variable with provided name found"))
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }
}

#[derive(Debug, Clone)]
enum ScopeVariables {
    Local(Rc<LocalVariable>),
    Const(Rc<Literal>),
}

impl ScopeVariables {
    fn stack_size(&self) -> Result<usize, TokenError> {
        match self {
            Self::Local(var) => var.get_type().map(|x| x.byte_size()),
            Self::Const(_) => Ok(0),
        }
    }

    fn get_expr(&self) -> Rc<dyn Expression> {
        match self {
            Self::Local(var) => var.clone(),
            Self::Const(lit) => lit.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct ScopeBlock {
    token: Token,
    variables: HashMap<String, ScopeVariables>,
}

impl ScopeBlock {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            variables: HashMap::new(),
        }
    }

    pub fn size(&self) -> Result<usize, TokenError> {
        self.variables.values().map(|v| v.stack_size()).sum()
    }
}

#[derive(Debug, Clone)]
pub struct ScopeRemoveStatement {
    token: Token,
    size: usize,
}

impl Statement for ScopeRemoveStatement {
    fn get_exec_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();
        asm.extend_from_slice(&load_to_register(RegisterDef::SPARE, self.size as u32));

        asm.push(AsmToken::OperationLiteral(Box::new(OpSub::new(
            ArgumentType::new(Register::StackPointer, DataType::U32),
            Register::StackPointer.into(),
            RegisterDef::SPARE.into(),
        ))));

        Ok(self.token.to_asm_iter(asm).into_iter().collect())
    }
}

#[derive(Debug)]
pub struct CompilingState {
    init_loc: u32,
    statements: Vec<Rc<dyn GlobalStatement>>,
    global_scope: HashMap<String, GlobalType>,
    current_id: usize,
    full_program: bool,
    scope_manager: ScopeManager,
}

impl Default for CompilingState {
    fn default() -> Self {
        Self {
            init_loc: 0x2000,
            global_scope: HashMap::new(),
            current_id: 0,
            statements: Vec::new(),
            full_program: true,
            scope_manager: ScopeManager::default(),
        }
    }
}

impl CompilingState {
    fn blank_token_loc(tok: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok,
            loc: LocationInfo::default(),
        }
    }

    pub fn get_next_id(&mut self) -> usize {
        let current = self.current_id;
        self.current_id += 1;
        current
    }

    pub fn get_assembler(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        fn add_name(asm: &mut Vec<AsmTokenLoc>, name: &str) {
            asm.extend_from_slice(&[
                CompilingState::blank_token_loc(AsmToken::Comment(name.into())),
                CompilingState::blank_token_loc(AsmToken::AlignInstruction),
            ]);
        }

        let init_label = "program_init".to_string();

        let mut asm = if self.full_program {
            vec![
                Self::blank_token_loc(AsmToken::LoadLoc(init_label.clone())),
                Self::blank_token_loc(AsmToken::LoadLoc(init_label.clone())),
                Self::blank_token_loc(AsmToken::ChangeAddress(self.init_loc)),
            ]
        } else {
            Vec::new()
        };

        add_name(&mut asm, "Initialization");
        asm.push(Self::blank_token_loc(AsmToken::CreateLabel(
            init_label.clone(),
        )));
        asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
            OpLdi::new(
                ArgumentType::new(Register::StackPointer, DataType::U16),
                0x1000,
            ),
        ))));

        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_init_code()?);
        }

        if let Some(GlobalType::Function(f)) = self.global_scope.get("main") {
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpLdn::new(ArgumentType::new(
                    Register::first_gp_register(),
                    DataType::U32,
                )),
            ))));
            asm.push(Self::blank_token_loc(AsmToken::LoadLoc(
                f.get_entry_label().to_string(),
            )));
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpCall::new(Register::first_gp_register().into()),
            ))));
        }

        asm.extend_from_slice(
            &[
                AsmToken::Comment("Program Halt".into()),
                AsmToken::OperationLiteral(Box::new(OpJmpri::new(0))),
            ]
            .map(Self::blank_token_loc),
        );

        add_name(&mut asm, "Static");
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_static_code()?);
        }

        add_name(&mut asm, "Functions");
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_exec_code()?);
        }

        Ok(asm)
    }

    pub fn get_scopes(&self) -> &ScopeManager {
        &self.scope_manager
    }

    pub fn get_scopes_mut(&mut self) -> &mut ScopeManager {
        &mut self.scope_manager
    }

    pub fn extract_scope(&mut self) -> ScopeManager {
        let sm = self.scope_manager.clone();
        self.scope_manager.clear();
        sm
    }

    pub fn get_struct(&self, name: &Token) -> Result<Rc<StructDefinition>, TokenError> {
        if let Some(GlobalType::Structure(s)) = self.global_scope.get(name.get_value()) {
            Ok(s.clone())
        } else {
            Err(name
                .clone()
                .into_err("no structure with provided name found"))
        }
    }

    pub fn add_global_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let var = Rc::new(GlobalVariable::new(
            def.token,
            self.get_next_id(),
            def.dtype,
            def.init_expr,
        )?);
        self.add_to_global_scope(GlobalType::Variable(var))
    }

    pub fn add_const_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Constant(Rc::new(def.into_literal()?)))
    }

    pub fn add_function(&mut self, func: Rc<FunctionDefinition>) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Function(func))
    }

    pub fn add_struct(&mut self, s: StructDefinition) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Structure(Rc::new(s)))
    }

    fn add_to_global_scope(&mut self, t: GlobalType) -> Result<(), TokenError> {
        let name = get_identifier(t.get_token())?;

        let statement = t.get_statement();
        match self.global_scope.entry(name.to_string()) {
            Entry::Vacant(e) => e.insert(t),
            Entry::Occupied(_) => {
                return Err(t
                    .get_token()
                    .clone()
                    .into_err(format!("global scope already contains name \"{name}\"")));
            }
        };

        if let Some(s) = statement {
            self.statements.push(s);
        }

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?.to_string();

        if let Ok(v) = self.scope_manager.get_variable(name) {
            return Ok(v);
        }

        match self.global_scope.get(&ident).map_or(
            Err(name
                .clone()
                .into_err("no variable with matching name found")),
            |x| Ok(x.clone()),
        )? {
            GlobalType::Variable(v) => Ok(v),
            GlobalType::Constant(v) => Ok(v),
            x => Err(x
                .get_token()
                .clone()
                .into_err("global is not a variable type")),
        }
    }
}
