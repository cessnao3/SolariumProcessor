use crate::tokenizer::Token;

pub struct TokenIter
{
    list: Vec<Token>,
    current: usize,
    started: bool
}

impl TokenIter
{
    pub fn new(tokens: Vec<Token>) -> TokenIter
    {
        return Self
        {
            list: tokens,
            current: 0,
            started: false
        };
    }

    pub fn next(&mut self) -> Option<Token>
    {
        if !self.started
        {
            self.started = true;
        }
        else if self.current < self.list.len()
        {
            self.current += 1;
        }

        return self.get_index_val(self.current);
    }

    pub fn last(&self) -> Option<Token>
    {
        if !self.started
        {
            return None;
        }
        else
        {
            return self.get_index_val(self.current);
        }
    }

    pub fn peek(&self) -> Option<Token>
    {
        if !self.started
        {
            return self.get_index_val(self.current);
        }
        else
        {
            return self.get_index_val(self.current + 1);
        }
    }

    fn get_index_val(&self, ind: usize) -> Option<Token>
    {
        if ind < self.list.len()
        {
            return Some(self.list[ind].clone());
        }
        else
        {
            return None;
        }
    }
}
