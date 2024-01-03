use super::error::ASMGenError;
use super::function_info::Slot;
use super::instruction_printer::InstPrinter;
use super::register::*;
use std::fs::File;

/// Assemble values of different types.
/// Slot.offset in Local(Slot) is the real offset to the new %sp
pub enum AsmValue<'p> {
    Void,
    Const(i32),
    Global(&'p str),
    Local(Slot),
    Arg(usize),
}

impl<'p> AsmValue<'p> {
    /// Whether this assemble value is a pointer
    pub fn is_ptr(&self) -> bool {
        match self {
            Self::Local(Slot { offset: _, is_ptr }) => is_ptr.clone(),
            _ => false,
        }
    }

    /// Write the assemble value to the given register
    /// stack_offset is specially for extended arguments, for they need to be located in the former stack frame
    pub fn write_register(
        &self,
        reg: &'static str,
        stack_offset: i32,
        f: &mut File,
    ) -> Result<(), ASMGenError> {
        let mut printer = InstPrinter::new(f, reg);
        match self {
            Self::Void => Err(ASMGenError::VoidValue),
            Self::Const(num) => {
                printer.li(reg, *num);
                Ok(())
            }
            Self::Global(label) => {
                printer.la(reg, label);
                printer.lw(reg, reg, 0);
                Ok(())
            }
            Self::Local(slot) => {
                printer.lw(SP!(), reg, slot.offset as i32);
                Ok(())
            }
            Self::Arg(idx) => {
                // If idx < 8, the argument is stored in registers a0-a7, otherwise in stack slots.
                // Note that arguments are placed in CALLER's stack frame!
                if *idx < 8 {
                    printer.mv(&Ai!(*idx), reg);
                } else {
                    let imm = ((*idx as i32) - 8) * 4 + stack_offset;
                    printer.lw(SP!(), reg, imm);
                }
                Ok(())
            }
        }
    }

    /// Read the given register to the assemble value
    pub fn read_register(
        &self,
        reg: &'static str,
        tmp: &'static str,
        stack_offset: i32,
        f: &mut File,
    ) -> Result<(), ASMGenError> {
        let mut printer = InstPrinter::new(f, tmp);
        match self {
            Self::Void => Err(ASMGenError::VoidValue),
            Self::Const(_) => Err(ASMGenError::ConstValue),
            Self::Global(label) => {
                printer.la(tmp, label);
                printer.sw(reg, tmp, 0);
                Ok(())
            }
            Self::Local(slot) => {
                printer.sw(reg, SP!(), slot.offset as i32);
                Ok(())
            }
            Self::Arg(idx) => {
                // If idx < 8, the argument is stored in registers a0-a7, otherwise in stack slots.
                // Note that arguments are placed in CALLER's stack frame!
                if *idx < 8 {
                    printer.mv(reg, &Ai!(*idx));
                } else {
                    let imm = ((*idx as i32) - 8) * 4 + stack_offset;
                    printer.sw(reg, SP!(), imm);
                }
                Ok(())
            }
        }
    }
}
