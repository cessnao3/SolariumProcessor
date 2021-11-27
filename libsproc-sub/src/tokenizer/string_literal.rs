pub fn print_string_literal(input: &str) -> String
{
    let replace_vals = vec![
        ("\\", "\\\\"),
        ("\n", "\\n"),
        ("\r", "\\r"),
        ("\"", "\\\"")
    ];

    let mut sval = input.to_string();
    for (from, to) in replace_vals.iter()
    {
        sval = sval.replace(from, to);
    }

    return sval;
}

pub fn try_string_literal(input: &str) -> Option<(String, usize)>
{
    if input.len() == 0
    {
        return None;
    }

    let first_char = input.chars().next().unwrap();

    if first_char != '"'
    {
        return None;
    }

    let mut char_vec: Vec<char> = Vec::new();
    let mut last_was_escape = false;
    let mut finished = false;
    let mut skip_val = 1usize;

    for c in input[1..input.len()].chars()
    {
        if c.is_whitespace() && (c != ' ' && c != '\t')
        {
            break;
        }

        if !last_was_escape
        {
            if c == '\\'
            {
                last_was_escape = true;
            }
            else if c == '"'
            {
                finished = true;
                skip_val += 1;
                break;
            }
            else
            {
                char_vec.push(c);
            }
        }
        else
        {
            if c == 'n'
            {
                char_vec.push('\n');
            }
            else if c == 'r'
            {
                char_vec.push('\r');
            }
            else if c == '\\'
            {
                char_vec.push('\\');
            }
            else if c == '"'
            {
                char_vec.push('"');
            }

            last_was_escape = false;
        }

        skip_val += 1;
    }

    if finished
    {
        return Some((char_vec.iter().collect(), skip_val));
    }
    else
    {
        return None;
    }
}
