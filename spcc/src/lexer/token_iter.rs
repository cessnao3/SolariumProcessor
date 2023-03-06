use crate::tokenizer::Token;

pub struct TokenIter {
    list: Vec<Token>,
    current: usize,
    started: bool,
}

impl TokenIter {
    pub fn new(tokens: Vec<Token>) -> TokenIter {
        Self {
            list: tokens,
            current: 0,
            started: false,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        if !self.started {
            self.started = true;
        } else if self.current < self.list.len() {
            self.current += 1;
        }

        self.get_index_val(self.current)
    }

    pub fn last(&self) -> Option<Token> {
        if !self.started {
            None
        } else {
            self.get_index_val(self.current)
        }
    }

    pub fn peek(&self) -> Option<Token> {
        if !self.started {
            self.get_index_val(self.current)
        } else {
            self.get_index_val(self.current + 1)
        }
    }

    fn get_index_val(&self, ind: usize) -> Option<Token> {
        if ind < self.list.len() {
            Some(self.list[ind].clone())
        } else {
            None
        }
    }
}
