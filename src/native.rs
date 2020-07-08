use crate::value::Value;

pub type NativeFn = fn(usize, Vec<Value>) -> Value;

pub fn test_native(_arg_count: usize, _args: Vec<Value>) -> Value {
    println!("Clock() was impossible to get working because rust only has Instant, which keeps track of time since the Instant was created, so I would need some kind of mutable static >:C");
    Value::Nil
}