use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Debug,
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc, LocationInfo, OpJmpri, OpSub};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef},
    functions::FunctionDefinition,
    parser::VariableDefinition,
    tokenizer::{Token, get_identifier},
    utilities::load_to_register,
    variables::{GlobalVariable, GlobalVariableStatement, LocalVariable},
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
}

impl GlobalType {
    fn get_token(&self) -> &Token {
        match self {
            Self::Variable(v) => v.get_token(),
            Self::Function(v) => v.get_token(),
        }
    }

    fn get_statement(&self) -> Rc<dyn GlobalStatement> {
        match self {
            Self::Variable(var) => Rc::new(GlobalVariableStatement::new(var.clone())),
            Self::Function(func) => func.clone(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ScopeManager {
    scopes: Vec<ScopeBlock>,
    all_variables: Vec<Rc<LocalVariable>>,
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

    pub fn clear(&mut self) {
        self.scopes.clear();
    }

    pub fn scope_full_size(&self) -> Result<usize, TokenError> {
        self.scopes.iter().map(|x| x.size()).sum()
    }

    pub fn add_var(&mut self, def: VariableDefinition) -> Result<Rc<LocalVariable>, TokenError> {
        let ident = get_identifier(&def.token)?;
        let offset = self.scope_full_size()?;

        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => Ok(e
                    .insert(Rc::new(LocalVariable::new(
                        def.token,
                        def.dtype,
                        RegisterDef::FN_BASE,
                        offset,
                        def.init_expr,
                    )?))
                    .clone()),
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
        let ident: String = get_identifier(name)?;

        for s in self.scopes.iter().rev() {
            if let Some(var) = s.variables.get(&ident) {
                return Ok(var.clone());
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
struct ScopeBlock {
    token: Token,
    variables: HashMap<String, Rc<LocalVariable>>,
    current_offset: usize,
}

impl ScopeBlock {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            variables: HashMap::new(),
            current_offset: 0,
        }
    }

    pub fn size(&self) -> Result<usize, TokenError> {
        self.variables
            .values()
            .map(|v| v.get_type().map(|x| x.byte_size()))
            .sum()
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

        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_init_code()?);
        }

        asm.extend_from_slice(
            &[
                AsmToken::Comment("Program Halt".into()),
                AsmToken::OperationLiteral(Box::new(OpJmpri::new(0))),
                AsmToken::ChangeAddress(0x10000), // TODO - TEMP!
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

    pub fn add_global_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let var = Rc::new(GlobalVariable::new(
            def.token,
            self.get_next_id(),
            def.dtype,
            def.init_expr,
        )?);
        self.add_to_global_scope(GlobalType::Variable(var))
    }

    pub fn add_function(&mut self, func: Rc<FunctionDefinition>) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Function(func))
    }

    fn add_to_global_scope(&mut self, t: GlobalType) -> Result<(), TokenError> {
        let name = get_identifier(t.get_token())?;
        let statement = t.get_statement();
        match self.global_scope.entry(name.clone()) {
            Entry::Vacant(e) => e.insert(t),
            Entry::Occupied(_) => {
                return Err(t
                    .get_token()
                    .clone()
                    .into_err(format!("global scope already contains name \"{name}\"")));
            }
        };

        self.statements.push(statement);

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident: String = get_identifier(name)?;

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
            x => Err(x
                .get_token()
                .clone()
                .into_err("global is not a variable type")),
        }
    }
}
