use crate::common::{MemoryWord, SolariumError};

/// Converts an input character into a memory-word supported by the SProc
pub fn character_to_word(c: char) -> Result<MemoryWord, SolariumError>
{
    const NULL: u8 = '\0' as u8;
    const NEWLINE: u8 = '\n' as u8;

    let char_val: u8 = match c as u8
    {
        NULL => 0x00,
        NEWLINE => 0x0A,
        0x20..=0x7E => c as u8,
        _ => return Err(SolariumError::CharacterToWord(c))
    };

    return Ok(MemoryWord::new(char_val as u16));
}

/// Converts a memory word into a text character
pub fn word_to_character(word: MemoryWord) -> Result<char, SolariumError>
{
    let char_val = match word.get()
    {
        0 => '\0',
        0x0A => '\n',
        0x20..=0x7E => (word.get() as u8) as char,
        _ => return Err(SolariumError::WordToCharacter(word))
    };

    return Ok(char_val);
}
