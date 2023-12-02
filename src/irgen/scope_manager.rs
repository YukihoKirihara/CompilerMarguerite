use super::function::FunctionInfo;
use super::value::Value;
use koopa::ir::Function;
use std::collections::HashMap;

/// ScopeManager is a singleton to manage the scope of the CompUnit.
pub struct ScopeManager<'ast> {
    /// Stack of values of different scopes
    vals_stack: Vec<HashMap<&'ast str, Value>>,
    /// Functions in the CompUnit
    funcs: HashMap<&'ast str, Function>,
    /// FunctionInfo of the current function
    curr_func: Option<FunctionInfo>,
}

impl<'ast> ScopeManager<'ast> {
    /// Create a new Scope
    pub fn new() -> Self {
        Self {
            vals_stack: vec![HashMap::new()],
            funcs: HashMap::new(),
            curr_func: None,
        }
    }

    /// Add a new function to current Scope
    pub fn new_func(&mut self, id: &'ast str, func: Function) -> Result<(), ()> {
        self.funcs.insert(id, func);
        Ok(())
    }

    /// Return a reference to the current function
    pub fn ref_curr_func(&self) -> Option<&FunctionInfo> {
        self.curr_func.as_ref()
    }

    /// Return a mutable reference to the current function
    pub fn mut_ref_curr_func(&mut self) -> Option<&mut FunctionInfo> {
        self.curr_func.as_mut()
    }

    /// Edit the current function
    pub fn set_curr_func(&mut self, info: FunctionInfo) -> () {
        self.curr_func = Some(info);
    }

    /// Open a new scope
    pub fn open(&mut self) -> () {
        self.vals_stack.push(HashMap::new());
    }

    /// Close the current scope
    pub fn close(&mut self) -> () {
        self.vals_stack.pop();
    }
}
