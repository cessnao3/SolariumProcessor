use std::collections::HashMap;
use std::rc::Rc;

pub const REG_DEFAULT_SPARE: usize = 6;
pub const REG_DEFAULT_PRIMARY_A: usize = 7;
pub const REG_DEFAULT_PRIMARY_B: usize = 8;

pub const REG_DEFAULT_JUMP_LOC: usize = 13;
pub const REG_FRAME_SP_VALUE: usize = 15;

pub trait EmitAssembly {
    fn emit_primary(&self) -> Vec<String>;

    fn emit_static(&self) -> Vec<String>;
}

pub trait LoadValue: ToString {
    fn load_value_to_register(&self, register: usize, register_spare: usize) -> Vec<String>;
}

pub trait NamedMemoryValue: LoadValue {
    fn set_value_from_register(&self, register_from: usize, register_spare: usize) -> Vec<String>;

    fn get_name(&self) -> String;

    fn load_address_to_register(&self, register: usize) -> Vec<String>;
}

struct Scope {
    pub id: usize,
    pub variables: HashMap<String, Rc<dyn NamedMemoryValue>>,
    pub stack_offset: usize,
    init_stack_offset: usize,
    pub function_end_label: Option<String>,
}

impl Scope {
    pub fn new(id: usize) -> Scope {
        Self {
            id,
            variables: HashMap::new(),
            stack_offset: 0,
            init_stack_offset: 0,
            function_end_label: None,
        }
    }
}

pub struct ScopeManager {
    scopes: Vec<Scope>,
    gen_index: usize,
}

impl ScopeManager {
    pub fn new() -> ScopeManager {
        Self {
            scopes: Vec::new(),
            gen_index: 0,
        }
    }

    pub fn add_scope(&mut self) -> Vec<String> {
        let next_ind = self.generate_index();
        let mut next_scope = Scope::new(next_ind);
        next_scope.stack_offset = match self.scopes.last() {
            Some(s) => s.stack_offset,
            None => 0,
        };
        next_scope.init_stack_offset = next_scope.stack_offset;

        let rvec = vec![format!("; opening scope {0:}", next_scope.id), format!(";")];

        self.scopes.push(next_scope);

        rvec
    }

    pub fn add_function_scope(&mut self, end_label: &str) -> Vec<String> {
        let mut assembly = self.add_scope();
        self.scopes.last_mut().unwrap().function_end_label = Some(end_label.to_string());
        assembly.push(format!("copy {0:}, $sp", REG_FRAME_SP_VALUE));
        assembly.push(";".to_string());
        assembly
    }

    fn pop_scope_assembly(&self, s: &Scope) -> Vec<String> {
        let mut assembly = vec![format!("; popping scope {0:}", s.id)];

        assembly.extend((s.init_stack_offset..s.stack_offset).map(|_| "pop".to_string()));
        assembly.push(";".to_string());
        assembly
    }

    pub fn pop_scope(&mut self) -> Vec<String> {
        let assembly = self.pop_scope_assembly(self.scopes.last().unwrap());
        self.scopes.remove(self.scopes.len() - 1);
        assembly
    }

    pub fn assembly_to_pop_for_return(&self) -> Vec<String> {
        // Find the index of the last scope back
        let mut index_val = None;
        for (i, val) in self.scopes.iter().enumerate().rev() {
            if val.function_end_label.is_some() {
                index_val = Some(i);
            }
        }

        // Determine the vector for scopes to pop
        let pop_scope_inds = match index_val {
            Some(v) => (v..self.scopes.len()).rev().collect(),
            None => Vec::new(),
        };

        // Pop the appropriate scopes and create the assembly
        let mut assembly = Vec::new();
        for i in pop_scope_inds {
            assembly.extend(self.pop_scope_assembly(&self.scopes[i]));
        }
        assembly
    }

    pub fn get_variable(&self, name: &str) -> Result<Rc<dyn NamedMemoryValue>, String> {
        for s in self.scopes.iter().rev() {
            if let Some(v) = s.variables.get(name) {
                return Ok(v.clone());
            }
        }

        Err(format!(
            "no variable named {0:} found in any available scopes",
            name
        ))
    }

    pub fn add_variable(
        &mut self,
        name: &str,
        var: Rc<dyn NamedMemoryValue>,
    ) -> Result<(), String> {
        return match self.scopes.last_mut() {
            Some(v) => {
                if v.variables.contains_key(name) {
                    Err(format!("scope already contains variable {0:}", name))
                } else {
                    v.variables.insert(name.to_string(), var);
                    Ok(())
                }
            }
            None => Err("no scope available".to_string()),
        };
    }

    pub fn get_function_end_label(&self) -> Option<String> {
        for s in self.scopes.iter().rev() {
            if let Some(label) = &s.function_end_label {
                return Some(label.clone());
            }
        }

        None
    }

    pub fn generate_index(&mut self) -> usize {
        let last = self.gen_index;
        self.gen_index += 1;
        last
    }

    pub fn get_stack_offset(&self) -> usize {
        self.scopes.last().unwrap().stack_offset
    }

    pub fn add_stack_offset(&mut self, incr: usize) {
        self.scopes.last_mut().unwrap().stack_offset += incr;
    }
}
