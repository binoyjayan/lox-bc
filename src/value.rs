/*
 * A constant pool is an array of values. A LOAD instruction to load
 * a constant looks up the value by index in that array.
 */
pub type Value = f64;

pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn free(&mut self) {
        self.values = Vec::new();
    }
    pub fn print(&self, idx: u8) {
        print!("{}", self.values[idx as usize]);
    }
}
