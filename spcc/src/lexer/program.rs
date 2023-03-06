pub struct ProgramSection {
    assembly: Vec<String>,
    static_assembly: Vec<String>,
    allow_static: bool,
}

impl ProgramSection {
    pub fn new() -> ProgramSection {
        Self {
            assembly: Vec::new(),
            static_assembly: Vec::new(),
            allow_static: false,
        }
    }

    pub fn new_static() -> ProgramSection {
        let mut prog = Self::new();
        prog.allow_static = true;
        prog
    }

    pub fn push(&mut self, val: String) {
        self.assembly.push(val);
    }

    pub fn extend(&mut self, val: Vec<String>) {
        self.assembly.extend(val);
    }

    pub fn push_static(&mut self, val: String) {
        if !self.allow_static {
            panic!("static not allowed!");
        }
        self.static_assembly.push(val);
    }

    pub fn extend_static(&mut self, val: Vec<String>) {
        if !self.allow_static {
            panic!("static not allowed!");
        }
        self.static_assembly.extend(val);
    }

    pub fn append(&mut self, other: ProgramSection) {
        self.assembly.extend(other.assembly);

        if !self.allow_static && !other.static_assembly.is_empty() {
            panic!("static not allowed");
        }

        self.static_assembly.extend(other.static_assembly);
    }

    pub fn get_primary_assembly(&self) -> &[String] {
        &self.assembly
    }

    pub fn get_static_assembly(&self) -> &[String] {
        &self.static_assembly
    }
}
