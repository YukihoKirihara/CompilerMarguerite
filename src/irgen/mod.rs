use self::{generator::IRGenerator, scope_manager::ScopeManager};
use crate::ast::CompUnit;
use koopa::ir::Program;

mod function;
mod generator;
mod scope_manager;
mod value;

pub fn generate(comp_unit: &CompUnit) -> Result<Program, ()> {
    let mut program = Program::new();
    let mut scopes = ScopeManager::new();
    comp_unit.generate(&mut program, &mut scopes);
    Ok(program)
}
