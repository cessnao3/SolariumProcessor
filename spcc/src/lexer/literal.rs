pub struct Literal {
    value: u16,
}

impl LoadValue for Literal {
    fn load_value_to_register(&self, register: usize) -> Vec<String> {
        let mut assembly = vec![
            format!("; Load Literal {0:} to Register {1:}", self.value, register),
            format!("ldn {0:}", register),
            format!(".load {0:}", self.value),
        ];

        return assembly;
    }
}
