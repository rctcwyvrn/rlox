use crate::value::Value;

pub type NativeFn = fn(usize, Vec<Value>) -> Value;

pub fn clock(_arg_count: usize, _args: Vec<Value>) -> Value {
    Value::Double(1.0)
}
