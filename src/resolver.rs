pub struct Resolver {
    enclosing_locals: Option<Vec<Local>>,
    upvalues: Vec<UpValue>,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl Resolver {
    pub fn begin_scope(&mut self) {
        self.scope_depth+=1;
    }

    /// MUST BE CALLED BY Compiler::end_scope()
    /// 
    /// Decrements the scope depth and pops off the values that went out of scope
    /// Todo: 
    /// *  Make this less uggo
    /// *  Use a trait or something to limit the visibility somehow?
    pub fn end_scope(&mut self) -> usize {
        self.scope_depth-=1;
        let mut pops = 0;
        for local in self.locals.iter().rev() {
            if let Some(x) = local.depth {
                if x > self.scope_depth {
                    pops+=1;
                } else {
                    break;
                }
            }
        }
        for _ in 0..pops {
            self.locals.pop();
        }
        pops
    }

    pub fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    pub fn add_local(&mut self, name: String) {
        let local = Local {
            name, 
            depth: None
        };
        self.locals.push(local);
    }

    /// Marks the last local variable as initialized by giving it a depth
    /// if the current scope is not global
    pub fn mark_initialized(&mut self) {
        if self.scope_depth == 0 { return }
        self.locals.last_mut().unwrap().depth = Some(self.scope_depth);
    }

    /// Declare new local variables by pushing them onto self.locals
    /// 
    /// New locals are set to a special "uninitialized" state until define_variable() is called
    /// 
    /// If the scope is global, do nothing
    pub fn declare_variable(&mut self, str_val: String) -> bool {
        if !self.is_global() { // Must not be in the global scope in order to define local vars
            let mut found_eq = false; // Is this the idiomatic way of doing this?? I have no idea

            for local in self.locals.iter() {
                if let Some(x) = local.depth {
                    if x < self.scope_depth {
                        break;
                    }
                }
                if str_val.eq(&local.name) {
                    found_eq = true;
                    break;
                }
            }

            self.add_local(str_val);
            !found_eq
        } else {
            true
        }
    }

    /// Walk and look for a local variable with the same name, None if the var is not found (treat as global)
    pub fn resolve_local(&self, name: &String) -> Result<Option<usize>, ()> {
        let mut error = false;
        for (i, local) in self.locals.iter().enumerate() {
            if local.name.eq(name){
                if local.depth == None  {
                    error = true;
                    break;
                } else {
                    return Ok(Some(i));
                }
            }
        }

        if error {
            Err(())
        } else {
            Ok(None)
        }
    }

    pub fn resolve_upvalue(&mut self, name: &String, function_depth: usize) -> Option<usize> {
        if function_depth == 0 { return None; } // Can't have upvalues in the global script
        let mut upval_index = None;
        for (i, local) in self.enclosing_locals.as_ref().unwrap().iter().enumerate() {
            if local.name.eq(name){
                upval_index = Some(i);
                break;
            }
        }

        if let Some(index) = upval_index {
            Some(self.add_upvalue(index, true))
        }else {
            None
        }
    }

    pub fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        self.upvalues.push(UpValue {
            is_local,
            index,
        });
        // self.functions.first_mut().unwrap().upvalue_count += 1;
        // self.functions.first_mut().unwrap().upvalue_count
        0
    }

    pub fn from_old(parent: &Resolver) -> Resolver {
        let mut locals = Vec::new();
        locals.push(Local {             // Placeholder local variable for VM use -> Will be filled by the function obj
            name: String::from(""),
            depth: None,
        });
        Resolver {
            enclosing_locals: Some(parent.locals.clone()), // Fixme: get rid of this clone somehow
            upvalues: Vec::new(),
            locals,
            scope_depth: parent.scope_depth, // Child is responsible for calling begin and end scope
        }
    }

    pub fn new() -> Resolver {
        let mut locals = Vec::new();
        locals.push(Local {             // Placeholder local variable for VM use -> Will be filled by the top level function
            name: String::from(""),
            depth: None,
        });
        Resolver {
            enclosing_locals: None,
            upvalues: Vec::new(),
            locals,
            scope_depth: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    depth: Option<usize>,
}

/// Similar to local, but for upvalues
struct UpValue {
    is_local: bool,
    index: usize,
}