struct Token {
    pub value: String,
    pub line: usize,
    pub column: usize,
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut v = Vec::new();

    let mut tok_init = None;

    let mut n_col = 0;
    let mut n_line = 1;

    for (i, c) in s.char_indices() {
        if c == '\n' {
            n_line += 1;
            n_col = 0;
        }

        n_col += 1;

        if tok_init.is_none() {
            if c.is_whitespace() {
                continue;
            } else {
                tok_init = Some(i);
            }
        }

        if let Some(init_i) = tok_init {
            // Extract the current point
            let so_far = &s[init_i..i];

            // Compile regexes for identifiers/numerics/etc?


            // Check for identifier

            // Check for number + decimal

            // Check for operators
        }
    }

    v
}
