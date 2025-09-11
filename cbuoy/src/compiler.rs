use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    fmt::Debug,
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, LocationInfo, OpCall, OpCopy, OpHalt, OpLdi, OpLdn,
};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef, TemporaryStackTracker},
    functions::FunctionDefinition,
    literals::{Literal, StringLiteral},
    tokenizer::{Token, get_identifier},
    typing::{FunctionParameter, StructDefinition},
    variables::{GlobalVariable, GlobalVariableStatement, LocalVariable, VariableDefinition},
};

pub trait Statement: Debug {
    fn get_exec_code(
        &self,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

pub trait GlobalStatement: Debug {
    fn get_init_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
    fn get_static_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
    fn get_func_code(&self) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

#[derive(Debug, Clone)]
enum GlobalType {
    Variable(Rc<GlobalVariable>),
    Function(Rc<dyn FunctionDefinition>),
    Constant(Rc<Literal>),
    Structure(Token, Rc<StructDefinition>),
    OpaqueType(Token),
}

impl GlobalType {
    fn get_token(&self) -> &Token {
        match self {
            Self::Variable(v) => v.get_token(),
            Self::Function(v) => v.get_token(),
            Self::Constant(v) => v.get_token(),
            Self::Structure(t, _) => t,
            Self::OpaqueType(t) => t,
        }
    }

    fn get_statement(&self) -> Option<Rc<dyn GlobalStatement>> {
        match self {
            Self::Variable(var) => Some(Rc::new(GlobalVariableStatement::new(var.clone()))),
            Self::Function(func) => Some(func.clone()),
            Self::Constant(..) => None,
            Self::Structure(..) => None,
            Self::OpaqueType(..) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeManager {
    token: Token,
    scopes: Vec<ScopeBlock>,
    max_size: usize,
    parameters: ScopeBlock,
}

impl ScopeManager {
    pub fn new(t: Token) -> Self {
        Self {
            token: t,
            scopes: Vec::new(),
            max_size: 0,
            parameters: ScopeBlock::default(),
        }
    }

    pub fn add_scope(&mut self) {
        self.scopes.push(ScopeBlock::default());
    }

    pub fn remove_scope(&mut self) -> Result<(), TokenError> {
        if self.scopes.pop().is_some() {
            Ok(())
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

    fn scope_full_size(&self) -> Result<usize, TokenError> {
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
                        RegisterDef::FN_VAR_BASE,
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

    pub fn add_parameter(&mut self, def: FunctionParameter) -> Result<(), TokenError> {
        let token = match &def.name {
            Some(name) => name,
            None => {
                return Err(self
                    .token
                    .clone()
                    .into_err("no token associated with function parameter"));
            }
        };

        if !self.scopes.is_empty() {
            return Err(self
                .token
                .clone()
                .into_err("cannot add a parameter when a scope has already been added!"));
        }

        let ident = get_identifier(token)?.to_string();
        let offset = self.parameters.size()?;

        match self.parameters.variables.entry(ident) {
            Entry::Vacant(e) => {
                let var = Rc::new(LocalVariable::new(
                    token.clone(),
                    def.dtype,
                    Register::ArgumentBase,
                    offset,
                    None,
                )?);

                // Update the scope values and maximum size of the stack
                e.insert(ScopeVariables::Local(var.clone()));
                Ok(())
            }
            Entry::Occupied(_) => Err(token
                .clone()
                .into_err("dupliate variable name exists within the same scope")),
        }
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?;

        for s in self.scopes.iter().rev() {
            if let Some(var) = s.variables.get(ident) {
                return Ok(var.get_expr());
            }
        }

        if let Some(p) = self.parameters.variables.get(ident) {
            Ok(p.get_expr())
        } else {
            Err(name
                .clone()
                .into_err("no variable with provided name found"))
        }
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

#[derive(Debug, Default, Clone)]
struct ScopeBlock {
    variables: HashMap<String, ScopeVariables>,
}

impl ScopeBlock {
    pub fn size(&self) -> Result<usize, TokenError> {
        self.variables.values().map(|v| v.stack_size()).sum()
    }
}

#[derive(Debug, Clone)]
pub enum UserTypeOptions {
    Struct(Rc<StructDefinition>),
    OpaqueType(Token),
}

#[derive(Debug, Default, Clone)]
pub struct UserTypes {
    types: HashMap<String, UserTypeOptions>,
}

impl UserTypes {
    pub fn get_struct(&self, t: &Token) -> Result<Rc<StructDefinition>, TokenError> {
        if let Some(e) = self.types.get(t.get_value()) {
            match e {
                UserTypeOptions::OpaqueType(next) => {
                    if next.get_value() != t.get_value() {
                        self.get_struct(next)
                    } else {
                        Err(t.clone().into_err(
                            "recursive loop found when parsing opaque type into structure",
                        ))
                    }
                }
                UserTypeOptions::Struct(s) => Ok(s.clone()),
            }
        } else {
            Err(t.clone().into_err("no valid type with provided name found"))
        }
    }
}

#[derive(Debug)]
pub struct CompilingState {
    init_loc: u32,
    statements: Vec<Rc<dyn GlobalStatement>>,
    global_scope: HashMap<String, GlobalType>,
    user_types: Rc<RefCell<UserTypes>>,
    string_literals: HashMap<String, Rc<StringLiteral>>,
    current_id: usize,
    full_program: bool,
    scope_manager: Option<ScopeManager>,
}

impl Default for CompilingState {
    fn default() -> Self {
        Self {
            init_loc: 0x2000,
            global_scope: HashMap::new(),
            user_types: Rc::new(RefCell::new(UserTypes::default())),
            string_literals: HashMap::new(),
            current_id: 0,
            statements: Vec::new(),
            full_program: true,
            scope_manager: None,
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

        for gv in self.global_scope.values() {
            if let GlobalType::Structure(tok, s) = gv {
                asm.push(tok.to_asm(AsmToken::LocationComment(format!(
                    "+struct({}) {}",
                    tok,
                    s.get_size(),
                ))));

                let mut struct_vals = s
                    .get_fields()
                    .iter()
                    .map(|(k, v)| (v.offset, k, v.dtype.clone()))
                    .collect::<Vec<_>>();
                struct_vals.sort_by(|a, b| a.0.cmp(&b.0));

                for (offset, name, dtype) in struct_vals {
                    asm.push(tok.to_asm(AsmToken::LocationComment(format!(
                        "+field({}) +{} : {}{}",
                        name,
                        offset,
                        dtype,
                        dtype.primitive_type().map(|x| format!(" ({x})")).unwrap_or(String::new()),
                    ))));
                }
            }
        }

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
                OpCopy::new(Register::ArgumentBase.into(), Register::StackPointer.into()),
            ))));
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpLdn::new(ArgumentType::new(RegisterDef::SPARE, DataType::U32)),
            ))));
            asm.push(Self::blank_token_loc(AsmToken::LoadLoc(
                f.get_entry_label().to_string(),
            )));
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpCall::new(RegisterDef::SPARE.into()),
            ))));
        }

        asm.extend_from_slice(
            &[
                AsmToken::Comment("Program Halt".into()),
                AsmToken::OperationLiteral(Box::new(OpHalt)),
            ]
            .map(Self::blank_token_loc),
        );

        add_name(&mut asm, "Static");
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_static_code()?);
        }

        for s in self.string_literals.values() {
            asm.extend_from_slice(&s.get_static_code()?);
        }

        add_name(&mut asm, "Functions");
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_func_code()?);
        }

        Ok(asm)
    }

    pub fn get_scopes(&self) -> Result<&ScopeManager, TokenError> {
        if let Some(s) = &self.scope_manager {
            Ok(s)
        } else {
            Err(TokenError {
                msg: "no scope initialized".to_string(),
                token: None,
            })
        }
    }

    pub fn get_scopes_mut(&mut self) -> Result<&mut ScopeManager, TokenError> {
        if let Some(s) = &mut self.scope_manager {
            Ok(s)
        } else {
            Err(TokenError {
                msg: "no scope initialized".to_string(),
                token: None,
            })
        }
    }

    pub fn init_scope(&mut self, token: Token) -> Result<(), TokenError> {
        if let Some(s) = &self.scope_manager {
            Err(s.token.clone().into_err("scope has not been cleared"))
        } else {
            self.scope_manager = Some(ScopeManager::new(token));
            Ok(())
        }
    }

    pub fn extract_scope(&mut self) -> Result<ScopeManager, TokenError> {
        if let Some(manager) = self.scope_manager.take() {
            Ok(manager)
        } else {
            Err(TokenError {
                msg: "unable to extract scope from uninitialized vlaues".to_string(),
                token: None,
            })
        }
    }

    pub fn get_struct(&self, name: &Token) -> Result<Rc<StructDefinition>, TokenError> {
        if let Some(GlobalType::Structure(_, s)) = self.global_scope.get(name.get_value()) {
            Ok(s.clone())
        } else {
            Err(name
                .clone()
                .into_err("no structure with provided name found"))
        }
    }

    pub fn get_opaque_type(&self, name: &Token) -> Result<String, TokenError> {
        if let Some(GlobalType::OpaqueType(t)) = self.global_scope.get(name.get_value()) {
            Ok(t.get_value().to_string())
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

    pub fn add_string_literal(&mut self, token: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        if let Some(existing) = self.string_literals.get(token.get_value()) {
            Ok(existing.clone())
        } else {
            let sv = Rc::new(StringLiteral::new(token.clone(), self.get_next_id())?);
            self.string_literals
                .insert(token.get_value().to_string(), sv.clone());
            Ok(sv)
        }
    }

    pub fn add_const_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Constant(Rc::new(def.into_literal()?)))
    }

    pub fn add_function(&mut self, func: Rc<dyn FunctionDefinition>) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Function(func))
    }

    fn update_user_types(&mut self) {
        let mut tv = self.user_types.borrow_mut();
        tv.types.clear();

        for (n, v) in self.global_scope.iter() {
            if let GlobalType::Structure(_, s) = v {
                tv.types
                    .insert(n.clone(), UserTypeOptions::Struct(s.clone()));
            } else if let GlobalType::OpaqueType(t) = v {
                tv.types
                    .insert(n.clone(), UserTypeOptions::OpaqueType(t.clone()));
            }
        }
    }

    pub fn get_user_type_db(&self) -> Rc<RefCell<UserTypes>> {
        self.user_types.clone()
    }

    pub fn add_opaque_type(&mut self, name: Token) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::OpaqueType(name))?;
        self.update_user_types();
        Ok(())
    }

    pub fn upgrade_opaque_type(
        &mut self,
        name: Token,
        s: Rc<StructDefinition>,
    ) -> Result<(), TokenError> {
        if self.get_opaque_type(&name)? == name.get_value() {
            self.global_scope
                .insert(name.get_value().to_string(), GlobalType::Structure(name, s));
            self.update_user_types();
            Ok(())
        } else {
            Err(name.clone().into_err(format!(
                "opaque type with provided name {name} not found for upgrade"
            )))
        }
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

        self.update_user_types();

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?.to_string();

        if let Ok(v) = self.get_scopes().and_then(|x| x.get_variable(name)) {
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
            GlobalType::Function(f) => Ok(f.as_expr()),
            x => Err(x
                .get_token()
                .clone()
                .into_err("global is not a variable type")),
        }
    }
}
