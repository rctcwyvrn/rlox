use crate::vm::VM;
use crate::native::NativeFn;


/// Runtime representation of the closure, ie what variables are in scope
#[derive(Debug, Clone, PartialEq)]
pub struct ObjClosure {
    pub function: usize, 
    pub values: Vec<Value>, // Will be filled at runtime with captured upvalues
}

impl ObjClosure {
    pub fn new(function: usize) -> ObjClosure {
        ObjClosure { function, values: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
    LoxString(String), 
    LoxFunction(usize), // Index of the function in the functions Vec in VM
    LoxClosure(ObjClosure),
    NativeFunction(NativeFn),
}

impl Value {
    /// Used for print statements, use {:?} debug formatting for trace and stack examining
    pub fn to_string(&self, vm: &VM) -> String {
        match self {
            Value::Double(x) => format!("{}",x),
            Value::Bool(x) => format!("{}",x),
            Value::LoxString(x) => format!("{}",x),
            Value::Nil => String::from("Nil"),
            Value::LoxFunction(x) => format!("<fn {}>", vm.functions.get(*x).unwrap().name.as_ref().unwrap()),
            Value::NativeFunction(x) => format!("<native_fn {:?}>", x),
            Value::LoxClosure(closure) => format!("<fn {} | {:?}>", vm.functions.get(closure.function).unwrap().name.as_ref().unwrap(), closure),
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        if let Value::Double(val) = self {
            Some(val.clone())
        } else {
            None
        }
    }
}

pub fn is_falsey(val: &Value) -> bool {
    match val {
        Value::Bool(false) => true,
        Value::Nil => true,
        _ => false
    }
}

pub fn values_equal(t: (Value, Value)) -> bool {
    if let (Value::Double(x), Value::Double(y)) = t {
        return x == y;
    } else if let (Value::Bool(x), Value::Bool(y)) = t {
        return x == y;
    } else if t.0 == Value::Nil {
        return true;
    } else if let (Value::LoxString(x), Value::LoxString(y)) = t {
        return x.eq(&y);
    }

    return false;
}