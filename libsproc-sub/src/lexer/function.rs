use super::common::*;

pub trait FunctionCall
{
    fn call_function(&self, args: Vec<Box<dyn LoadValue>>) -> Vec<String>;
}

fn call_function_with_lcoation(loadloc: String, args: Vec<Box<dyn LoadValue>>) -> Vec<String>
{
    const REG_PUSH_ARG: usize = 5;

    let mut assembly = Vec::new();
    for a in args.iter().rev()
    {
        assembly.extend(a.load_value_to_register(REG_PUSH_ARG));
        assembly.push(format!("push {0:}", REG_PUSH_ARG));
    }

    assembly.push("jmpri 2".to_string());
    assembly.push(loadloc);
    assembly.push("ldir 5, -1".to_string());
    assembly.push("call 5".to_string());

    for _ in args.iter()
    {
        assembly.push("pop".to_string());
    }

    return assembly;
}

pub struct FunctionExtern
{
    name: String,
    args: Vec<String>,
    location: usize
}

impl FunctionCall for FunctionExtern
{
    fn call_function(&self, args: Vec<Box<dyn LoadValue>>) -> Vec<String>
    {
        return call_function_with_lcoation(
            format!(".load {0:}", self.location),
            args);
    }
}

pub struct FunctionDefinition
{
    name: String,
    args: Vec<String>,
    statements: Vec<Box<dyn EmitAssembly>>
}

impl FunctionDefinition
{
    pub fn get_label(&self) -> String
    {
        return format!("function_call_{0:}", self.name);
    }
}

impl EmitAssembly for FunctionDefinition
{
    fn emit(&self, scopes: &mut ScopeManager) -> Vec<String>
    {
        let mut assembly = Vec::new();

        assembly.push(format!(":{0:}", self.get_label()));

        assembly.push(format!("copy {0:}, $sp", REG_FRAME_SP_VALUE));

        for s in self.statements.iter()
        {
            assembly.extend(s.emit(scopes));
        }

        assembly.push("ret".to_string());

        return assembly;
    }
}

impl FunctionCall for FunctionDefinition
{
    fn call_function(&self, args: Vec<Box<dyn LoadValue>>) -> Vec<String>
    {
        return call_function_with_lcoation(
            format!(".loadloc {0:}", self.get_label()),
            args);
    }
}
