use super::function_info::FunctionInfo;
use koopa::ir::entities::{Program, Value};
use std::collections::HashMap;

/// ProgramInfo is a struct to record information of the program
pub struct ProgramInfo<'p> {
    /// A reference to the Program
    program: &'p Program,
    /// A map from the global value to its identifier
    global_values: HashMap<Value, String>,
    /// The current funcion
    curr_func: Option<FunctionInfo>,
    /// A counter to identify basic blocks
    label_count: usize,
}

impl<'p> ProgramInfo<'p> {
    /// Create a new ProgramInfo
    pub fn new(program: &'p Program) -> Self {
        Self {
            program: program,
            global_values: HashMap::new(),
            curr_func: None,
            label_count: 0 as usize,
        }
    }

    /// Return a reference to the program
    pub fn ref_program(&self) -> &'p Program {
        self.program
    }

    /// Record a global value and its identifier
    pub fn record_global_value(&mut self, value: Value, name: String) {
        self.global_values.insert(value, name);
    }

    /// Return name of a global value
    pub fn get_global_value_name(&mut self, value: Value) -> &str {
        self.global_values.get(&value).unwrap()
    }

    /// Return a reference to the current function
    pub fn ref_curr_func(&self) -> Option<&FunctionInfo> {
        self.curr_func.as_ref()
    }

    /// Return a mutable reference to the current function
    pub fn mut_ref_curr_func(&mut self) -> Option<&mut FunctionInfo> {
        self.curr_func.as_mut()
    }

    /// Set the current function
    pub fn set_curr_func(&mut self, func_info: FunctionInfo) -> () {
        self.curr_func = Some(func_info)
    }

    /// Remove the current function
    pub fn remove_curr_func(&mut self) -> () {
        self.curr_func = None
    }

    /// Return a incremented label count
    pub fn get_new_label_count(&mut self) -> usize {
        self.label_count += 1;
        self.label_count - 1
    }
}
