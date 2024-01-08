use jasm::AssemblerToken;
use jib::cpu::Register;

pub trait Addressable {
    fn get_address(&self, reg: Register) -> Vec<AssemblerToken>;
}
