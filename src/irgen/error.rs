/// Custom errors for IRGenerator.
use std::error::Error;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum IRGenError {
    DupIdent(String),
    IdentNotFound(String),
    NotAConstant(String),
    NotAPointer,
    ZeroDivider,
}

impl Error for IRGenError {}

impl Display for IRGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::DupIdent(msg) => write!(
                f,
                "Re-declaration or re-definition of the same identifier '{}'.",
                msg
            ),
            Self::IdentNotFound(msg) => write!(
                f,
                "The identifier '{}' is NOT found in current scopes.",
                msg
            ),
            Self::NotAConstant(msg) => write!(
                f,
                "The identifier '{}' does NOT match a constant variable.",
                msg
            ),
            Self::NotAPointer => write!(f, "A integer value is mistaken for a pointer."),
            Self::ZeroDivider => write!(f, "Division or Modulo by zero."),
        }
    }
}
