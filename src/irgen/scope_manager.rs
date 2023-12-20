use super::error::IRGenError;
use super::function::FunctionInfo;
use super::variable::Variable;
use koopa::ir::{BasicBlock, Function, Program, Type, Value};
use std::collections::HashMap;

/// ScopeManager is a singleton to manage the scope of the CompUnit.
pub struct ScopeManager<'ast> {
    /// Stack of variables of different scopes
    vals_stack: Vec<HashMap<&'ast str, Variable>>,
    /// Functions in the CompUnit
    funcs: HashMap<&'ast str, Function>,
    /// FunctionInfo of the current function
    curr_func: Option<FunctionInfo>,
    /// Stack of loops with the condition and final bblock
    /// The condition bblock recorded for "continue" and the final bblock for "break", respectively.
    loops_stack: Vec<(BasicBlock, BasicBlock)>,
}

impl<'ast> ScopeManager<'ast> {
    /// Create a new Scope
    pub fn new() -> Self {
        Self {
            vals_stack: vec![HashMap::new()],
            funcs: HashMap::new(),
            curr_func: None,
            loops_stack: Vec::new(),
        }
    }

    /// Checks whether the current Scope is global
    pub fn is_global(&self) -> bool {
        self.curr_func.is_none()
    }

    /// Add a new function to current Scope
    pub fn new_func(&mut self, id: &'ast str, func: Function) -> Result<(), IRGenError> {
        if self.funcs.contains_key(id) || self.vals_stack.first().unwrap().contains_key(id) {
            Err(IRGenError::DupIdent(id.to_string()))
        } else {
            self.funcs.insert(id, func);
            Ok(())
        }
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

    /// Return a function according to its name
    pub fn load_function(&self, ident: &'ast str) -> Result<Function, IRGenError> {
        match self.funcs.get(ident).copied() {
            Some(func) => Ok(func),
            None => Err(IRGenError::IdentNotFound(ident.to_string())),
        }
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

    /// Return the type of a value
    pub fn get_value_type(&self, program: &Program, value: Value) -> Type {
        if value.is_global() {
            program.borrow_value(value).ty().clone()
        } else {
            program
                .func(self.ref_curr_func().unwrap().func())
                .dfg()
                .value(value)
                .ty()
                .clone()
        }
    }

    /// Enter a new loop
    pub fn enter_loop(&mut self, cond_bblock: BasicBlock, final_bblock: BasicBlock) {
        self.loops_stack.push((cond_bblock, final_bblock));
    }

    /// Exit the current loop
    pub fn exit_loop(&mut self) {
        self.loops_stack.pop();
    }

    /// Return the condition bblock of the current loop
    pub fn get_curr_loop_cond_bblock(&self) -> Result<&BasicBlock, IRGenError> {
        match self.loops_stack.last() {
            Some((cond_bblock, _)) => Ok(cond_bblock),
            None => Err(IRGenError::NotInALoop),
        }
    }

    /// Return the final bblock of the current loop
    pub fn get_curr_loop_final_bblock(&self) -> Result<&BasicBlock, IRGenError> {
        match self.loops_stack.last() {
            Some((_, final_bblock)) => Ok(final_bblock),
            None => Err(IRGenError::NotInALoop),
        }
    }
}
