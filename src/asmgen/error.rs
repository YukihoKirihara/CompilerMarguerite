/// Custom errors for ASMGenerator.
use std::error::Error;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum ASMGenError {
    BasicBlockNotFound,
    ConstValue,
    LocalValueNotFound,
    Register,
    UnKnown,
}

impl Error for ASMGenError {}

impl Display for ASMGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::BasicBlockNotFound => write!(f, "Tried to visit an unknown basic block."),
            Self::ConstValue => write!(f, "Tried to write to a const value."),
            Self::LocalValueNotFound => write!(f, "Tried to visit an unknown local value."),
            Self::Register => write!(f, "Tried to locate a register in the memory"),
            Self::UnKnown => write!(f, "An unknown error occurs."),
        }
    }
}
