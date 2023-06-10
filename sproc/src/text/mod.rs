use crate::common::MemoryWord;

#[derive(Copy, Clone, Debug)]
pub enum CharacterError {
    CharacterToMemoryWord(char),
    MemoryWordToCharacter(MemoryWord),
}

impl std::fmt::Display for CharacterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CharacterToMemoryWord(c) => write!(f, "unable to convert {:02X} to word", *c as u8),
            Self::MemoryWordToCharacter(word) => write!(f, "unable to convert {:04X} to character", word.get()),
        }
    }
}

/// Converts an input character into a memory-word supported by the SProc
pub fn character_to_word(c: char) -> Result<MemoryWord, CharacterError> {
    const NULL: u8 = b'\0';
    const NEWLINE: u8 = b'\n';

    let char_val: u8 = match c as u8 {
        NULL => 0x00,
        NEWLINE => 0x0A,
        0x20..=0x7E => c as u8,
        _ => return Err(CharacterError::CharacterToMemoryWord(c)),
    };

    Ok(MemoryWord::from(char_val as u16))
}

/// Converts a memory word into a text character
pub fn word_to_character(word: MemoryWord) -> Result<char, CharacterError> {
    let char_val = match word.get() {
        0 => '\0',
        0x0A => '\n',
        0x20..=0x7E => (word.get() as u8) as char,
        _ => return Err(CharacterError::MemoryWordToCharacter(word)),
    };

    Ok(char_val)
}
