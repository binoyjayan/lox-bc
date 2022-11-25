use crate::value::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::SystemTime;

pub struct NativeClock {}

impl NativeFunction for NativeClock {
    fn call(&self, _arg_count: usize, _args: &[Rc<RefCell<Value>>]) -> Value {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(n) => Value::Number(n.as_millis() as f64),
            Err(e) => panic!("Failed to get system time {:?}", e),
        }
    }
}
