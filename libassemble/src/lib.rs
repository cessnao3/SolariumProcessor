use libscpu::memory::MemoryWord;

pub fn assemble(lines: Vec<String>) -> Result<Vec<MemoryWord>, String>
{
    return Err("not implemented".to_string());
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
