pub fn try_match_name(input: &str) -> Option<(String, usize)> {
    let mut length = 0usize;
    for (i, c) in input.chars().enumerate() {
        if i == 0 && !c.is_alphabetic() {
            break;
        } else if c.is_alphabetic() || c.is_numeric() || c == '_' {
            // Pass
        } else {
            break;
        }

        length += 1;
    }

    if length == 0 {
        None
    } else {
        Some((input[0..length].to_string(), length))
    }
}
