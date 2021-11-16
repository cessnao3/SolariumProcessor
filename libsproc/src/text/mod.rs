use crate::common::{MemoryWord, SolariumError};

pub fn character_to_word(c: char) -> Result<MemoryWord, SolariumError>
{
    return Err(SolariumError::CharacterToWord(c));
}

pub fn word_to_character(word: MemoryWord) -> Result<char, SolariumError>
{
    return Err(SolariumError::WordToCharacter(word));
}
