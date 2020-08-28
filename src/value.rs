use crate::native::NativeFn;
use crate::vm::{VMState, VM};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
    LoxString(String),
    LoxFunction(usize), // Index of the function in the functions Vec in VM // Fixme: Is this even reachable? Can this be completely removed and the parameter put in OpClosure?
    NativeFunction(NativeFn),
    LoxClass(usize),
    LoxPointer(usize),
    LoxBoundMethod(ObjBoundMethod),
}

impl Value {
    /// Used for print statements, use {:?} debug formatting for trace and stack examining
    pub fn to_string(&self, vm: &VM, state: &VMState) -> String {
        match self {
            Value::Double(x) => format!("{}", x),
            Value::Bool(x) => format!("{}", x),
            Value::LoxString(x) => format!("{}", x),
            Value::Nil => String::from("nil"),
            Value::LoxFunction(x) => format!(
                "<fn {}>",
                vm.functions.get(*x).unwrap().name.as_ref().unwrap()
            ),
            Value::NativeFunction(_x) => format!("<native_fn>"),
            Value::LoxClass(class) => format!("<class {}>", class),
            Value::LoxPointer(pointer) => format!(
                "<pointer {}> to {}",
                pointer,
                state.deref(*pointer).to_string(vm)
            ), // Suggestion: Don't reveal to the user the internals?
            Value::LoxBoundMethod(method) => format!(
                "<method {} from {}",
                vm.functions
                    .get(method.method)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                state.deref(method.pointer).to_string(vm)
            ),
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        if let Value::Double(val) = self {
            Some(val.clone())
        } else {
            None
        }
    }

    /// Hard cast to a ObjPointer. Panics if this value is not a LoxPointer
    pub fn as_pointer(&self) -> usize {
        if let Value::LoxPointer(ptr) = self {
            *ptr
        } else {
            panic!(
                "VM panic! Failed to cast value to a pointer. Found {:?} instead",
                self
            )
        }
    }
}

pub fn is_falsey(val: &Value) -> bool {
    matches!(val, Value::Bool(false) | Value::Nil)
}

pub fn values_equal(t: (&Value, &Value)) -> bool {
    match t {
        (Value::Double(x), Value::Double(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::LoxString(x), Value::LoxString(y)) => x.eq(y),
        (Value::LoxPointer(x), Value::LoxPointer(y)) => x == y,
        (Value::LoxClass(x), Value::LoxClass(y)) => x == y,
        (Value::LoxFunction(x), Value::LoxFunction(y)) => x == y,
        (Value::NativeFunction(x), Value::NativeFunction(y)) => x == y,
        (Value::LoxBoundMethod(x), Value::LoxBoundMethod(y)) => x == y,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjBoundMethod {
    pub method: usize,  // Index into the functions vec for which function to call
    pub pointer: usize, // Pointer to the LoxInstance that this method is bound to
}

// End of stack/implicit copy objects

// Heap Objects

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeapObjType {
    HeapPlaceholder,
    LoxInstance,
    LoxClosure,
}

#[derive(Debug, PartialEq)]
pub struct HeapObj {
    pub obj: HeapObjVal,
    pub obj_type: HeapObjType,
    pub is_marked: bool,
}

impl HeapObj {
    fn to_string(&self, vm: &VM) -> String {
        self.obj.to_string(vm)
    }

    pub fn new_instance(val: ObjInstance) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LoxInstance(val),
            obj_type: HeapObjType::LoxInstance,
            is_marked: false,
        }
    }

    pub fn new_closure(val: ObjClosure) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LoxClosure(val),
            obj_type: HeapObjType::LoxClosure,
            is_marked: false,
        }
    }

    pub fn new_placeholder() -> HeapObj {
        HeapObj {
            obj: HeapObjVal::HeapPlaceholder,
            obj_type: HeapObjType::HeapPlaceholder,
            is_marked: false,
        }
    }
}

// I swear i really tried to not have this be duplicate with HeapObjType, but couldn't figure out a way to do it
#[derive(Debug, PartialEq)]
pub enum HeapObjVal {
    HeapPlaceholder,
    LoxInstance(ObjInstance),
    LoxClosure(ObjClosure),
    // LoxString(String), // Maybe...
}

impl HeapObjVal {
    fn to_string(&self, vm: &VM) -> String {
        match self {
            HeapObjVal::LoxClosure(closure) => format!(
                "<fn {} | {:?}>",
                vm.functions
                    .get(closure.function)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                closure
            ),
            HeapObjVal::LoxInstance(instance) => format!(
                "<instance {}>",
                vm.classes.get(instance.class).unwrap().name
            ),
            HeapObjVal::HeapPlaceholder => {
                panic!("VM panic! How did a placeholder value get here?")
            }
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
    pub class: usize,                  // Which class was this instance made from?
    pub fields: HashMap<usize, Value>, // Stores the field values. FunctionChunks are stored in the ClassChunk, which is not ideal since it adds an extra vec lookup before getting to the function
}

impl ObjInstance {
    pub fn new(class: usize) -> ObjInstance {
        ObjInstance {
            class,
            fields: HashMap::new(),
        }
    }
}

/// Runtime representation of the closure, ie what variables are in scope
#[derive(Debug, PartialEq)]
pub struct ObjClosure {
    pub function: usize,
    pub values: Vec<Value>, // Will be filled at runtime
}

impl ObjClosure {
    pub fn new(function: usize) -> ObjClosure {
        ObjClosure {
            function,
            values: Vec::new(),
        }
    }
}
