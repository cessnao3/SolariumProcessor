use once_cell::sync::Lazy;

use super::symbol::Symbol;

pub fn is_separator(c: char) -> bool {
    static SEPARATOR_CHARS: Lazy<Vec<char>> = Lazy::new(|| {
        Symbol::get_symbol_list()
            .iter()
            .filter(|(s, _)| s.len() == 1)
            .map(|(s, _)| s.chars().next().unwrap())
            .collect()
    });

    c.is_ascii_whitespace() || SEPARATOR_CHARS.contains(&c)
}
