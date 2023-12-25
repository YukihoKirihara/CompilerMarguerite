/// Custom errors for IRGenerator.
use std::error::Error;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum IRGenError {
    DupIdent(String),
    IdentNotFound(String),
    IsAnArray,
    NotAConstant(String),
    NotAPointer,
    NotInALoop,
    UnKnown,
    UnmathedParams(String),
    VoidValue,
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
            Self::IsAnArray => write!(f, "Tried to treat an array as a base element."),
            Self::NotAConstant(msg) => write!(
                f,
                "The identifier '{}' does NOT match a constant variable.",
                msg
            ),
            Self::NotAPointer => write!(f, "A integer value is mistaken for a pointer."),
            Self::NotInALoop => write!(
                f,
                "Statement 'continue' or 'break' occurs in places where NO loop exists."
            ),
            Self::UnKnown => write!(f, "An unknown error occurs."),
            Self::UnmathedParams(msg) => write!(
                f,
                "The formal and real parameters of function {} is unmatched",
                msg
            ),
            Self::VoidValue => write!(f, "Tried to read from a void value."),
            Self::ZeroDivider => write!(f, "Division or Modulo by zero."),
        }
    }
}
