use jasm::Token;
use jib::cpu::Register;

pub trait Addressable {
    fn get_address(&self, reg: Register) -> Vec<Token>;
}
