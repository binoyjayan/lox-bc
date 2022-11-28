use std::fmt;

#[derive(Debug)]
pub struct Class {
    name: String,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)
    }
}
