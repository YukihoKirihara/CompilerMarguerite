use self::error::IRGenError;
use self::generator::IRGenerator;
use self::scope_manager::ScopeManager;
use crate::ast::CompUnit;
use koopa::ir::Program;

mod error;
mod exp_value;
mod function;
mod generator;
mod rval_calculator;
mod scope_manager;
mod variable;

pub fn generate(comp_unit: &CompUnit) -> Result<Program, IRGenError> {
    let mut program = Program::new();
    let mut scopes = ScopeManager::new();
    comp_unit.generate(&mut program, &mut scopes)?;
    Ok(program)
}
