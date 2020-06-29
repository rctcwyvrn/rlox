#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
    LoxString(String), 
    // I have no idea if this does what I want it to do, does this go on the stack or on the heap? String objects must be on the heap right? 
    // Does rust do all the hard work for me?
}

impl Value {
    pub fn as_bool(&self) -> Option<bool> {
        if let Value::Bool(val) = self {
            Some(val.clone())
        } else {
            None
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

pub fn is_falsey(val: Value) -> bool {
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