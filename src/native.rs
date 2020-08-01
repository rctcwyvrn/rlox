use crate::value::Value;

pub type NativeFn = fn(usize, Vec<Value>) -> Value;

pub fn test_native(arg_count: usize, args: Vec<Value>) -> Value {
    println!("Clock() was impossible to get working because rust only has Instant, which keeps track of time since the Instant was created, so I would need some kind of mutable static >:C");
    println!("Given {} args => {:?}", arg_count, args);
    Value::Nil
}
