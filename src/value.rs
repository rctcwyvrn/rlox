#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Bool(bool),
    Nil,
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

// Using a tagged union for this would be a dumb idea when we can use ADTs