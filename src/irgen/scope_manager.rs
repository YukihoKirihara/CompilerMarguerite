use super::error::IRGenError;
use super::function::FunctionInfo;
use super::variable::Variable;
use koopa::ir::Function;
use std::collections::HashMap;

/// ScopeManager is a singleton to manage the scope of the CompUnit.
pub struct ScopeManager<'ast> {
    /// Stack of variables of different scopes
    vals_stack: Vec<HashMap<&'ast str, Variable>>,
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

    /// Checks whether the current Scope is global
    pub fn is_global(&self) -> bool {
        self.curr_func.is_none()
    }

    /// Add a new function to current Scope
    pub fn new_func(&mut self, id: &'ast str, func: Function) -> Result<(), IRGenError> {
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

    /// Create a new (constant or mutable) variable in current scope
    pub fn create_new_variable(
        &mut self,
        ident: &'ast str,
        var: Variable,
    ) -> Result<(), IRGenError> {
        let is_global = self.is_global();
        let curr_vals_stack = self.vals_stack.last_mut().unwrap();
        if curr_vals_stack.contains_key(ident) || (is_global && self.funcs.contains_key(ident)) {
            Err(IRGenError::DupIdent(ident.to_string()))
        } else {
            curr_vals_stack.insert(ident, var);
            Ok(())
        }
    }

    /// Return the value of a variable
    pub fn load_variable(&self, ident: &'ast str) -> Result<&Variable, IRGenError> {
        for vals in self.vals_stack.iter().rev() {
            if let Some(var) = vals.get(ident) {
                return Ok(var);
            }
        }
        Err(IRGenError::IdentNotFound(ident.to_string()))
    }
}
