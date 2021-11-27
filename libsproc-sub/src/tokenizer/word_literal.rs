fn char_is_hex(c: char) -> bool
{
    const HEX_CHARS: [char; 6] = [
        'A',
        'B',
        'C',
        'D',
        'E',
        'F',
    ];

    return c.is_numeric() || HEX_CHARS.contains(&c.to_ascii_uppercase());
}

pub fn try_match_integer_literal(input: &str) -> Option<(u16, usize)>
{
    if input.len() == 0
    {
        return None;
    }

    let first_char = input.chars().next().unwrap();

    if first_char == '-'
    {
        let mut current = 1usize;

        for c in input[1..input.len()].chars()
        {
            if !c.is_numeric()
            {
                break;
            }

            current += 1;
        }

        if current == 1
        {
            return None;
        }

        return match &input[0..current].parse::<i16>()
        {
            Ok(v) => Some((*v as u16, current)),
            Err(_) => None
        };
    }
    else if first_char.is_numeric()
    {
        let mut current = 1usize;
        let mut is_hex = false;
        for (i, c) in input[1..input.len()].chars().enumerate()
        {
            if i == 0 && (c == 'x' || c == 'X') && first_char == '0'
            {
                is_hex = true;
            }
            else if is_hex && char_is_hex(c)
            {
                // Skip
            }
            else if !c.is_numeric()
            {
                break;
            }

            current += 1;
        }

        let word_parse_result;

        if is_hex
        {
            word_parse_result = u16::from_str_radix(&input[2..current], 16);
        }
        else
        {
            word_parse_result = input[0..current].parse::<u16>();
        }

        return match word_parse_result
        {
            Ok(v) => Some((v, current)),
            Err(_) => None
        };
    }

    return None;
}
