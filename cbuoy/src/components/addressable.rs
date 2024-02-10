use jasm::AssemblerToken;
use jib::cpu::Register;

pub trait Addressable {
    fn load_address(&self, reg: Register) -> Vec<AssemblerToken>;
}
