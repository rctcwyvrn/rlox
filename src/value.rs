use crate::vm::{VM, VMState};
use crate::native::NativeFn;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
    LoxString(String), 
    LoxFunction(usize), // Index of the function in the functions Vec in VM
    NativeFunction(NativeFn),
    LoxClass(usize),
    LoxPointer(ObjPointer),
}

impl Value {
    /// Used for print statements, use {:?} debug formatting for trace and stack examining
    pub fn to_string(&self, vm: &VM, state: &VMState) -> String {
        match self {
            Value::Double(x)                => format!("{}",x),
            Value::Bool(x)                  => format!("{}",x),
            Value::LoxString(x)             => format!("{}",x),
            Value::Nil                      => String::from("Nil"),
            Value::LoxFunction(x)           => format!("<fn {}>", vm.functions.get(*x).unwrap().name.as_ref().unwrap()),
            Value::NativeFunction(x)        => format!("<native_fn {:?}>", x),
            Value::LoxClass(class)          => format!("<class {}>", class),
            Value::LoxPointer(pointer)      => format!("<pointer {}> to {}", pointer.obj, state.deref(*pointer).to_string(vm)),
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


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjPointer {
    pub obj: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeapObjType {
    LoxInstance,
    LoxClosure,
}

#[derive(Debug, PartialEq)]
pub struct HeapObj {
    pub obj: HeapObjVal,
    pub obj_type: HeapObjType,
    //pub type: ObjType,
    //pub is_marked: bool,
}

impl HeapObj {
    fn to_string(&self, vm: &VM) -> String {
        self.obj.to_string(vm)
    }

    pub fn new_instance(val: ObjInstance) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LoxInstance(val),
            obj_type: HeapObjType::LoxInstance,
        }
    }

    pub fn new_closure(val: ObjClosure) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LoxClosure(val),
            obj_type: HeapObjType::LoxClosure,
        }
    }
}

// I swear i really tried to not have this be duplicate with HeapObjType, but couldn't figure out a way to do it
#[derive(Debug, PartialEq)]
pub enum HeapObjVal {
    LoxInstance(ObjInstance),
    LoxClosure(ObjClosure),
    // LoxString(String), // Maybe...
}

impl HeapObjVal {
    fn to_string(&self, vm: &VM) -> String {
        match self {
            HeapObjVal::LoxClosure(closure)   => format!("<fn {} | {:?}>", vm.functions.get(closure.function).unwrap().name.as_ref().unwrap(), closure),
            HeapObjVal::LoxInstance(instance) => format!("<instance {}>", instance.class),
        }
    }

    pub fn as_closure(&self) -> &ObjClosure {
        if let HeapObjVal::LoxClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut ObjClosure {
        if let HeapObjVal::LoxClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance(&self) -> &ObjInstance {
        if let HeapObjVal::LoxInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut ObjInstance {
        if let HeapObjVal::LoxInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }
}

/// Runtime instantiation of class definitions
#[derive(Debug, PartialEq)]
pub struct ObjInstance {
    pub class: usize,
    pub fields: HashMap<String,Value>, // Possible improvement: Resolve all the field references at compile time and replace this with just a Vec
}

impl ObjInstance {
    pub fn new(class: usize) -> ObjInstance {
        ObjInstance { class, fields: HashMap::new() }
    }
}

/// Runtime representation of the closure, ie what variables are in scope
#[derive(Debug, PartialEq)]
pub struct ObjClosure {
    pub function: usize, 
    pub values: Vec<Value>, // Will be filled at runtime

    // pub values: Vec<usize>, // Will be filled at runtime with indexes of the captured upvalues in the VM upvalues Vec
}

impl ObjClosure {
    pub fn new(function: usize) -> ObjClosure {
        ObjClosure { function, values: Vec::new() }
    }
}