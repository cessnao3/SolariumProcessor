#[derive(Copy, Clone, Debug)]
pub enum CharacterError {
    CharacterToByte(char),
    ByteToCharacter(u8),
}

impl std::fmt::Display for CharacterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CharacterToByte(c) => write!(f, "unable to convert {:02X} to word", *c as u8),
            Self::ByteToCharacter(b) => write!(f, "unable to convert {:02X} to character", b),
        }
    }
}

/// Converts an input character into a memory-word supported by the SProc
pub fn character_to_byte(c: char) -> Result<u8, CharacterError> {
    const NULL: u8 = b'\0';
    const NEWLINE: u8 = b'\n';

    let char_val: u8 = match c as u8 {
        NULL => 0x00,
        NEWLINE => 0x0A,
        0x20..=0x7E => c as u8,
        _ => return Err(CharacterError::CharacterToByte(c)),
    };

    Ok(char_val)
}

/// Converts a memory word into a text character
pub fn byte_to_character(b: u8) -> Result<char, CharacterError> {
    let char_val = match b {
        0 => '\0',
        0x0A => '\n',
        0x20..=0x7E => b as char,
        _ => return Err(CharacterError::ByteToCharacter(b)),
    };

    Ok(char_val)
}
