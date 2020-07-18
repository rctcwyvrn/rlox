
// type Link = Option<Box<Resolver>>;

/// Manages the declaration and definition of local variables
/// 
/// One resolver is created for each Compiler, ie for each function definition. Enclosing locals points to the parent resolver
/// 
/// After each function definition the upvalues must be merged into FunctionChunk
#[derive(Debug, Clone)]
pub struct Resolver {
    stack: Vec<ResolverNode>
}

/// Used by Resolver to generate simple functions that just call the same function on the ResolverNode on the top of the stack
macro_rules! delegate_to_latest {
    ($fn_name: ident, $ret: ty) => {
        pub fn $fn_name(&mut self) -> $ret {
            self.stack.last_mut().unwrap().$fn_name()
        }
    };
    ($fn_name: ident, $ret: ty, $param: ty) => {
        pub fn $fn_name(&mut self, x: $param) -> $ret {
            self.stack.last_mut().unwrap().$fn_name(x)
        }
    }
}

impl Resolver {
    delegate_to_latest!(begin_scope, ());
    delegate_to_latest!(end_scope, usize);
    delegate_to_latest!(is_global, bool);
    delegate_to_latest!(mark_initialized, ());

    delegate_to_latest!(declare_variable, bool, String);
    delegate_to_latest!(resolve_local, Result<Option<usize>, ()>, &str);

    /// Attempt to resolve the variable name by looking at the enclosing locals
    /// If found, add an upvalue to self.upvalues
    ///
    /// Returns the index of the UpValue in the upvalues Vec
    pub fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        None
        // if !self.has_parent() { return None; } // Can't have upvalues in the global script
        // let mut upval_index = None;
        // for (i, local) in self.parent().locals.iter().enumerate() {
        //     if local.name.eq(name){
        //         upval_index = Some(i);
        //         break;
        //     }
        // }

        // if let Some(index) = upval_index {
        //     Some(self.add_upvalue(index, true))
        // } else {
        //     None
        // }
    }

    /// Push a new ResolverNode for the new function scope
    pub fn push(&mut self) {
        let mut locals = Vec::new();
        locals.push(Local {             // Placeholder local variable for VM use -> Will be filled by the function obj
            name: String::from(""),
            depth: None,
        });

        let new = ResolverNode {
            upvalues: Vec::new(),
            locals,
            scope_depth: self.stack.last().unwrap().scope_depth, // Child is responsible for calling begin and end scope
        };

        self.stack.push(new);
    }

    /// Yeet off the old ResolverNodes
    /// I'm sure I'll need to change this later to actually keep the values and have an usize struct var to keep track of the current ResolverNode
    /// But whatever I want to see if this shit works
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn new() -> Resolver {
        let mut locals = Vec::new();
        locals.push(Local {             // Placeholder local variable for VM use -> Will be filled by the top level function
            name: String::from(""),
            depth: None,
        });

        let top = ResolverNode {
            upvalues: Vec::new(),
            locals,
            scope_depth: 0,
        };

        let mut stack = Vec::new();
        stack.push(top);

        Resolver { stack }
    }
}

#[derive(Debug, Clone)]
struct ResolverNode {
    upvalues: Vec<UpValue>,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl ResolverNode {
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

    fn add_local(&mut self, name: String) {
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

    /// MUST BE CALLED BY Compiler::declare_variable()
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
    /// 
    /// *  Err => Syntax error detected, throw an error
    /// *  Ok(None) => Resolution failed
    /// *  Ok(index) => found
    pub fn resolve_local(&self, name: &str) -> Result<Option<usize>, ()> {
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

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        for (i, existing_upvalue) in self.upvalues.iter().enumerate() {
            if existing_upvalue.index == index { return i }
        }

        self.upvalues.push(UpValue {
            is_local,
            index,
        });
        self.upvalues.len() - 1
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    depth: Option<usize>,
}

/// Similar to local, but for upvalues
#[derive(Debug, Clone, Copy)]
struct UpValue {
    is_local: bool,
    index: usize,
}