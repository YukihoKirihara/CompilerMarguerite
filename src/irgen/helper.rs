use super::error::IRGenError;
use super::rval_calculator::RValCalculator;
use super::scope_manager::ScopeManager;
use crate::ast::ConstExp;
use koopa::ir::Type;

/// Generate the type of an array
/// dims = vec![] -> A single integer value
/// dism = vec![2, 3] i.e. arr[2][3]-> [[i32, 3], 2]
pub fn get_array_type(dims: &Vec<ConstExp>, scopes: &ScopeManager) -> Result<Type, IRGenError> {
    let mut array_type = Type::get_i32();
    for d in dims.iter().rev() {
        let len = d.rval_calc(scopes).unwrap();
        array_type = Type::get_array(array_type, len as usize);
    }
    Ok(array_type)
}
