use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Debug,
    rc::Rc,
};

use jib_asm::{AsmToken, AsmTokenLoc, LocationInfo, OpJmpri};

use crate::{
    TokenError,
    expressions::Expression,
    parser::FunctionDefinition,
    tokenizer::{Token, get_identifier},
    typing::Type,
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

#[derive(Debug, Clone)]
struct ScopeBlock {
    variables: HashMap<String, Rc<LocalVariable>>,
    current_offset: usize,
}

#[derive(Debug)]
pub struct CompilingState {
    init_loc: u32,
    statements: Vec<Rc<dyn GlobalStatement>>,
    global_scope: HashMap<String, GlobalType>,
    scopes: Vec<ScopeBlock>,
    current_id: usize,
    full_program: bool,
}

impl Default for CompilingState {
    fn default() -> Self {
        Self {
            init_loc: 0x2000,
            global_scope: HashMap::new(),
            current_id: 0,
            statements: Vec::new(),
            scopes: Vec::new(),
            full_program: true,
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
                AsmToken::ChangeAddress(0x3000), // TODO - TEMP!
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

    pub fn add_global_var(
        &mut self,
        name: Token,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<(), TokenError> {
        let var = Rc::new(GlobalVariable::new(
            name,
            self.get_next_id(),
            dtype,
            init_expr,
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

    pub fn add_local_var(
        &mut self,
        name: Token,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<(), TokenError> {
        let ident = get_identifier(&name)?;

        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => {
                    e.insert(Rc::new(LocalVariable::new(name, dtype, init_expr)?));
                    Ok(())
                }
                Entry::Occupied(_) => {
                    Err(name.into_err("dupliate variable name exists within the same scope"))
                }
            }
        } else {
            Err(name.into_err("unable to add variable without a scope block"))
        }
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?;

        for s in self.scopes.iter().rev() {
            if let Some(var) = s.variables.get(&ident) {
                return Ok(var.clone());
            }
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
