use super::error::IRGenError;
use super::scope_manager::ScopeManager;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::{Program, Value};

/// Expression value of different types
pub enum ExpValue {
    Void,
    Int(Value),
    IntPtr(Value),
}

impl ExpValue {
    pub fn get_int_value(
        self,
        program: &mut Program,
        scopes: &mut ScopeManager,
    ) -> Result<Value, IRGenError> {
        match self {
            Self::Void => Err(IRGenError::VoidValue),
            Self::Int(value) => Ok(value),
            Self::IntPtr(ptr) => {
                let info = scopes.mut_ref_curr_func().unwrap();
                let load = info.create_new_value(program).load(ptr);
                info.push_inst_curr_bblock(program, load);
                Ok(load)
            }
        }
    }
    pub fn get_int_ptr(self) -> Result<Value, IRGenError> {
        match self {
            Self::Void => Err(IRGenError::VoidValue),
            Self::Int(_) => Err(IRGenError::NotAPointer),
            Self::IntPtr(ptr) => Ok(ptr),
        }
    }
}
