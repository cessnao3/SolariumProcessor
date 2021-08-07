use crate::memory::MemoryWord;
use std::str::FromStr;

pub struct AsmInstructionLine
{
    line: String,
    words: Vec<String>
}

impl FromStr for AsmInstructionLine
{
    type Err = String;

    fn from_str(s: &str) -> Result<AsmInstructionLine, Self::Err>
    {
        // Define the line
        let line = s.trim().to_string();

        // Extract words
        let words: Vec<String> = s
            .split(' ')
            .map(|v| v.trim().to_ascii_lowercase())
            .filter(|v| v.len() > 0)
            .collect();

        // Check length
        return if words.is_empty()
        {
            Err("provided assembly line must not be empty".to_string())
        }
        else
        {
            Ok(AsmInstructionLine
            {
                line,
                words
            })
        }
    }
}

impl AsmInstructionLine
{
    pub fn inst_word(&self) -> &str
    {
        return &self.words[0];
    }

    pub fn arg_words(&self) -> Vec<&String>
    {
        return self.words[1..]
            .iter()
            .map(|v| v)
            .collect::<Vec<&String>>();
    }

    pub fn get_line(&self) -> &str
    {
        return &self.line;
    }
}

pub struct AsmInstruction
{
    pub opcode: u8,
    pub arg0: u8,
    pub arg1: u8,
    pub arg2: u8
}

impl AsmInstruction
{
    pub fn new() -> AsmInstruction
    {
        return AsmInstruction
        {
            opcode: 0,
            arg0: 0,
            arg1: 0,
            arg2: 0
        };
    }

    pub fn to_word(&self) -> MemoryWord
    {
        return
            self.opcode as MemoryWord |
                (self.arg0 as MemoryWord) << 8 |
                (self.arg1 as MemoryWord) << 16 |
                (self.arg2 as MemoryWord) << 24;
    }
}
