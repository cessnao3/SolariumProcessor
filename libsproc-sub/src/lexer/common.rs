use std::collections::HashMap;

pub trait EmitAssembly
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>;
}

pub trait LoadValue
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>;
}

pub trait FunctionCall
{
    fn call_function(&self, arg_values: Vec<Box<dyn LoadValue>>) -> String;
}

pub struct Scope
{
    id: usize,
    variables: HashMap<String, Box<dyn LoadValue>>,
    functions: HashMap<String, Box<dyn FunctionCall>>
}

impl Scope
{
    pub fn new(id: usize) -> Scope
    {
        return Self
        {
            id,
            variables: HashMap::new(),
            functions: HashMap::new()
        };
    }

    pub fn get_name(&self) -> String
    {
        return format!("s{0:}", self.id);
    }
}

pub struct ScopeManager
{
    scopes: Vec<Scope>,
    gen_index: usize
}

impl ScopeManager
{
    pub fn new() -> ScopeManager
    {
        return Self
        {
            scopes: Vec::new(),
            gen_index: 0
        };
    }

    pub fn add_scope(&mut self)
    {
        self.scopes.push(Scope::new(self.generate_index()));
    }

    pub fn pop_scope(&mut self)
    {
        self.scopes.remove(self.scopes.len() - 1);
    }

    pub fn get_full_name(&self) -> String
    {
        let names: Vec<String> = self.scopes.iter().map(|v| v.get_name()).collect();
        return names.join("_");
    }

    pub fn generate_index(&mut self) -> usize
    {
        let last = self.gen_index;
        self.gen_index += 1;
        return last;
    }
}
