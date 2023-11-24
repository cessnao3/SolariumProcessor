#[derive(Debug, Clone, Copy)]
pub struct Word {
    val: u8
}

impl Word {
    /// Creates a new memory word from the value
    pub fn new(val: u8) -> Self {
        Self { val }
    }

    /// Gets the current word value
    pub fn get(&self) -> u8 {
        self.val
    }

    /// Sets the current word value
    pub fn set(&mut self, val: u8) {
        self.val = val;
    }
}

impl Default for Word {
    fn default() -> Self {
        Self::new(0)
    }
}

impl From<u8> for Word {
    fn from(value: u8) -> Self {
        Self::new(value)
    }
}
