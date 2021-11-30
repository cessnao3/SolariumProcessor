use std::collections::HashMap;
use std::rc::Rc;

pub const REG_DEFAULT_TEST_RESULT: usize = 5;
pub const REG_DEFAULT_SPARE: usize = 6;
pub const REG_DEFAULT_TEST_JUMP_A: usize = 7;
pub const REG_DEFAULT_TEST_JUMP_B: usize = 8;
pub const REG_FRAME_SP_VALUE: usize = 15;

pub trait EmitAssembly
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>;
}

pub trait LoadValue: ToString
{
    fn load_value_to_register(&self, register: usize, register_spare: usize) -> Vec<String>;
}

pub trait FunctionCall
{
    fn call_function(&self, arg_values: Vec<Rc<dyn LoadValue>>) -> String;

    fn get_name(&self) -> String;

    fn get_start_label(&self) -> String;

    fn get_end_label(&self) -> String;
}

pub trait NamedMemoryValue: LoadValue
{
    fn set_value_from_register(&self, register_from: usize, register_spare: usize) -> Vec<String>;

    fn get_name(&self) -> String;

    fn load_address_to_register(&self, register: usize) -> Vec<String>;
}

struct Scope
{
    pub id: usize,
    pub variables: HashMap<String, Rc<dyn NamedMemoryValue>>,
    pub stack_offset: usize,
    pub function_end_label: Option<String>
}

impl Scope
{
    pub fn new(id: usize) -> Scope
    {
        return Self
        {
            id,
            variables: HashMap::new(),
            stack_offset: 0,
            function_end_label: None
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
    functions: HashMap<String, Rc<dyn FunctionCall>>,
    gen_index: usize
}

impl ScopeManager
{
    pub fn new() -> ScopeManager
    {
        return Self
        {
            scopes: Vec::new(),
            functions: HashMap::new(),
            gen_index: 0
        };
    }

    pub fn add_scope(&mut self) -> Vec<String>
    {
        let next_ind = self.generate_index();
        let mut next_scope = Scope::new(next_ind);
        next_scope.stack_offset = match self.scopes.last()
        {
            Some(s) => s.stack_offset,
            None => 0
        };

        let rvec = vec![
            format!("; opening scope {0:}", next_scope.id),
            format!("copy {0:}, $sp", REG_FRAME_SP_VALUE),
            format!(";")
        ];

        self.scopes.push(next_scope);

        return rvec;
    }

    pub fn add_function_scope(&mut self, end_label: &str) -> Vec<String>
    {
        let assembly = self.add_scope();
        self.scopes.last_mut().unwrap().function_end_label = Some(end_label.to_string());
        return assembly;
    }

    fn pop_scope_assembly(&self, s: &Scope) -> Vec<String>
    {
        let mut assembly = vec![
            format!("; popping scope {0:}", s.id)
        ];

        assembly.extend((0..s.stack_offset).map(|_| "pop".to_string()));
        assembly.push(format!(";"));
        return assembly;
    }

    pub fn pop_scope(&mut self) -> Vec<String>
    {
        let assembly = self.pop_scope_assembly(self.scopes.last().unwrap());
        self.scopes.remove(self.scopes.len() - 1);
        return assembly;
    }

    fn scopes_to_pop_for_return(&self) -> Vec<usize>
    {
        let mut vals = Vec::new();
        for (i, val) in self.scopes.iter().enumerate().rev()
        {
            if i > 1 && val.function_end_label.is_none()
            {
                vals.push(i);
            }
        }
        return vals;
    }

    pub fn assembly_to_pop_for_return(&self) -> Vec<String>
    {
        let mut assembly = Vec::new();

        for i in self.scopes_to_pop_for_return()
        {
            assembly.extend(self.pop_scope_assembly(&self.scopes[i]));
        }

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
            match s.variables.get(name)
            {
                Some(v) => return Ok(v.clone()),
                None => ()
            };
        }

        return Err(format!("no variable named {0:} found in any available scopes", name));
    }

    pub fn get_function(&self, name: &str) -> Result<Rc<dyn FunctionCall>, String>
    {
        return match self.functions.get(name)
        {
            Some(v) => Ok(v.clone()),
            None => Err(format!("function {0:} not found in scope", name))
        };
    }

    pub fn add_variable(&mut self, name: &str, var: Rc<dyn NamedMemoryValue>) -> Result<(), String>
    {
        return match self.scopes.last_mut()
        {
            Some(v) =>
            {
                if v.variables.contains_key(name)
                {
                    Err(format!("scope already contains variable {0:}", name))
                }
                else
                {
                    v.variables.insert(name.to_string(), var);
                    Ok(())
                }
            }
            None => Err(format!("no scope available"))
        };
    }

    pub fn add_function(&mut self, name: &str, func: Rc<dyn FunctionCall>) -> Result<(), String>
    {
        if self.functions.contains_key(name)
        {
            return Err(format!("function {0:} already exists in scope", name));
        }
        else
        {
            self.functions.insert(
                name.into(),
                func);
            return Ok(());
        }
    }

    pub fn get_function_end_label(&self) -> Option<String>
    {
        for s in self.scopes.iter().rev()
        {
            if let Some(label) = &s.function_end_label
            {
                return Some(label.clone())
            }
        }

        return None;
    }

    pub fn generate_index(&mut self) -> usize
    {
        let last = self.gen_index;
        self.gen_index += 1;
        return last;
    }

    pub fn get_stack_offset(&self) -> usize
    {
        return self.scopes.last().unwrap().stack_offset;
    }

    pub fn add_stack_offset(&mut self, incr: usize)
    {
        self.scopes.last_mut().unwrap().stack_offset += incr;
    }
}
