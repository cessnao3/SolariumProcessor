use std::collections::HashMap;
use std::rc::Rc;

pub const REG_DEFAULT_TEST_RESULT: usize = 5;
pub const REG_DEFAULT_TEST_JUMP_A: usize = 6;
pub const REG_DEFAULT_TEST_JUMP_B: usize = 7;
pub const REG_FRAME_SP_VALUE: usize = 15;

pub trait EmitAssembly
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>;
}

pub trait LoadValue: ToString
{
    fn load_value_to_register(&self, register: usize) -> Vec<String>;
}

pub trait FunctionCall
{
    fn call_function(&self, arg_values: Vec<Rc<dyn LoadValue>>) -> String;

    fn get_name(&self) -> String;
}

pub trait NamedMemoryValue: LoadValue
{
    fn set_value_from_register(&self, register: usize) -> Vec<String>;

    fn get_name(&self) -> String;
}

struct Scope
{
    id: usize,
    variables: HashMap<String, Rc<dyn NamedMemoryValue>>,
    functions: HashMap<String, Rc<dyn FunctionCall>>,
    stack_offset: usize
}

impl Scope
{
    pub fn new(id: usize) -> Scope
    {
        return Self
        {
            id,
            variables: HashMap::new(),
            functions: HashMap::new(),
            stack_offset: 0
        };
    }

    pub fn add_variable(&mut self, name: &str, var: Rc<dyn NamedMemoryValue>) -> Result<(), String>
    {
        if self.variables.contains_key(name)
        {
            return Err(format!("variable {0:} already exists in the current {1:}", name, self.get_name()));
        }
        else
        {
            self.variables.insert(
                name.into(),
                var);
            return Ok(());
        }
    }

    pub fn add_function(&mut self, name: &str, func: Rc<dyn FunctionCall>) -> Result<(), String>
    {
        if self.functions.contains_key(name)
        {
            return Err(format!("function {0:} already exists in the current {1:}", name, self.get_name()));
        }
        else
        {
            self.functions.insert(
                name.into(),
                func);
            return Ok(());
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<dyn NamedMemoryValue>>
    {
        return match self.variables.get(name)
        {
            Some(v) => Some(v.clone()),
            None => None
        };
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<dyn FunctionCall>>
    {
        return match self.functions.get(name)
        {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    pub fn get_name(&self) -> String
    {
        return format!("s{0:}", self.id);
    }

    pub fn get_stack_offset(&self) -> usize
    {
        return self.stack_offset;
    }

    pub fn add_stack_offset(&mut self, incr: usize)
    {
        self.stack_offset += incr;
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

    pub fn add_scope(&mut self) -> Vec<String>
    {
        let next_ind = self.generate_index();
        self.scopes.push(Scope::new(next_ind));

        return vec![
            format!("copy {0:}, $sp", REG_FRAME_SP_VALUE)
        ];
    }

    pub fn pop_scope(&mut self) -> Vec<String>
    {
        let assembly = (0..self.scopes.last().unwrap().get_stack_offset())
            .map(|_| "pop".to_string())
            .collect();

        self.scopes.remove(self.scopes.len() - 1);

        return assembly;
    }

    pub fn get_full_name(&self) -> String
    {
        let names: Vec<String> = self.scopes.iter().map(|v| v.get_name()).collect();
        return names.join("_");
    }

    pub fn get_variable(&self, name: &str) -> Result<Rc<dyn NamedMemoryValue>, String>
    {
        for s in self.scopes.iter().rev()
        {
            match s.get_variable(name)
            {
                Some(v) => return Ok(v),
                None => ()
            };
        }

        return Err(format!("no variable named {0:} found in any available scopes", name));
    }

    pub fn get_function(&self, name: &str) -> Result<Rc<dyn FunctionCall>, String>
    {
        for s in self.scopes.iter().rev()
        {
            match s.get_function(name)
            {
                Some(v) => return Ok(v),
                None => ()
            };
        }

        return Err(format!("no function named {0:} found in any available scopes", name));
    }

    pub fn add_variable(&mut self, name: &str, var: Rc<dyn NamedMemoryValue>) -> Result<(), String>
    {
        return match self.scopes.last_mut()
        {
            Some(v) => v.add_variable(name, var),
            None => Err(format!("no scope available"))
        };
    }

    pub fn add_function(&mut self, name: &str, func: Rc<dyn FunctionCall>) -> Result<(), String>
    {
        return match self.scopes.last_mut()
        {
            Some(v) => v.add_function(name, func),
            None => Err(format!("no scope available"))
        };
    }

    pub fn generate_index(&mut self) -> usize
    {
        let last = self.gen_index;
        self.gen_index += 1;
        return last;
    }

    pub fn get_stack_offset(&self) -> usize
    {
        return self.scopes.last().unwrap().get_stack_offset();
    }

    pub fn add_stack_offset(&mut self, incr: usize)
    {
        self.scopes.last_mut().unwrap().add_stack_offset(incr);
    }
}
