/// Custom errors for ASMGenerator.
use std::error::Error;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum ASMGenError {
    BasicBlockNotFound,
    ConstValue,
    GlobalValueNotFound,
    LocalValueNotFound,
    UnKnown,
    VoidValue,
}

impl Error for ASMGenError {}

impl Display for ASMGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::BasicBlockNotFound => write!(f, "Tried to visit an unknown basic block."),
            Self::ConstValue => write!(f, "Tried to write to a const value."),
            Self::GlobalValueNotFound => write!(f, "Tried to visit an unknown global value."),
            Self::LocalValueNotFound => write!(f, "Tried to visit an unknown local value."),
            Self::UnKnown => write!(f, "An unknown error occurs."),
            Self::VoidValue => write!(f, "Tried to read from or write to a void value."),
        }
    }
}
