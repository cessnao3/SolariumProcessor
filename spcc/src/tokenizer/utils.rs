use lazy_static::lazy_static;

use super::symbol::Symbol;

pub fn is_separator(c: char) -> bool {
    lazy_static! {
        static ref SEPARATOR_CHARS: Vec<char> = Symbol::get_symbol_list()
            .iter()
            .filter(|(s, _)| s.len() == 1)
            .map(|(s, _)| s.chars().next().unwrap())
            .collect();
    }

    c.is_ascii_whitespace() || SEPARATOR_CHARS.contains(&c)
}
