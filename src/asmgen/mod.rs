use self::error::ASMGenError;
use self::generator::ASMGenerator;
use self::program_info::ProgramInfo;
use koopa::ir::entities::Program;
use koopa::ir::Type;
use std::fs::File;

mod asm_value;
mod error;
mod function_info;
mod generator;
mod instruction_printer;
mod program_info;
mod register;

pub fn generate(program: &Program, f: &mut File) -> Result<(), ASMGenError> {
    // Adapt to the pointer width of riscv32
    Type::set_ptr_size(4);
    let mut program_info = ProgramInfo::new(program);
    program.generate(&mut program_info, f)
}
