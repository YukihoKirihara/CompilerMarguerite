use super::error::IRGenError;
use super::scope_manager::ScopeManager;
use super::variable::Variable;
use crate::ast::*;

pub trait RValCalculator<'exp> {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError>;
}

impl<'exp> RValCalculator<'exp> for Exp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        self.lor_exp.rval_calc(scopes)
    }
}

impl<'exp> RValCalculator<'exp> for LVal {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        let var = scopes.load_variable(&self.ident).unwrap();
        match *var {
            Variable::Const(num) => Ok(num),
            Variable::Value(_) => Err(IRGenError::NotAConstant(self.ident.clone().to_string())),
        }
    }
}

impl<'exp> RValCalculator<'exp> for PrimaryExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::ParenExp(exp) => exp.rval_calc(scopes),
            Self::LVal(lval) => lval.rval_calc(scopes),
            Self::Number(num) => Ok(*num),
        }
    }
}

impl<'exp> RValCalculator<'exp> for UnaryExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::PrimaryExp(exp) => exp.rval_calc(scopes),
            Self::FuncExp(func_exp) => {
                Err(IRGenError::NotAConstant(func_exp.ident.clone().to_string()))
            }
            Self::UnaryOpExp(unary_op, exp) => {
                let exp_val = exp.rval_calc(scopes).unwrap();
                match unary_op {
                    UnaryOp::Neg => Ok(-exp_val),
                    UnaryOp::Not => Ok((exp_val == 0) as i32),
                }
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for MulExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::UnaryExp(exp) => exp.rval_calc(scopes),
            Self::MulUnaryExp(lexp, mul_op, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap();
                let rval = rexp.rval_calc(scopes).unwrap();
                match mul_op {
                    MulOp::Mul => Ok(lval * rval),
                    MulOp::Div => {
                        if rval == 0 {
                            Err(IRGenError::ZeroDivider)
                        } else {
                            Ok(lval / rval)
                        }
                    }
                    MulOp::Mod => {
                        if rval == 0 {
                            Err(IRGenError::ZeroDivider)
                        } else {
                            Ok(lval % rval)
                        }
                    }
                }
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for AddExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::MulExp(exp) => exp.rval_calc(scopes),
            Self::AddMulExp(lexp, add_op, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap();
                let rval = rexp.rval_calc(scopes).unwrap();
                match add_op {
                    AddOp::Add => Ok(lval + rval),
                    AddOp::Sub => Ok(lval - rval),
                }
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for RelExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::AddExp(exp) => exp.rval_calc(scopes),
            Self::RelAddExp(lexp, rel_op, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap();
                let rval = rexp.rval_calc(scopes).unwrap();
                match rel_op {
                    RelOp::LT => Ok((lval < rval) as i32),
                    RelOp::GT => Ok((lval > rval) as i32),
                    RelOp::LE => Ok((lval <= rval) as i32),
                    RelOp::GE => Ok((lval >= rval) as i32),
                }
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for EqExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::RelExp(exp) => exp.rval_calc(scopes),
            Self::EqRelExp(lexp, eq_op, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap();
                let rval = rexp.rval_calc(scopes).unwrap();
                match eq_op {
                    EqOp::Eq => Ok((lval == rval) as i32),
                    EqOp::Neq => Ok((lval != rval) as i32),
                }
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for LAndExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::EqExp(exp) => exp.rval_calc(scopes),
            Self::LAndEqExp(lexp, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap() != 0;
                let rval = rexp.rval_calc(scopes).unwrap() != 0;
                Ok((lval && rval) as i32)
            }
        }
    }
}

impl<'exp> RValCalculator<'exp> for LOrExp {
    fn rval_calc(&'exp self, scopes: &mut ScopeManager<'exp>) -> Result<i32, IRGenError> {
        match self {
            Self::LAndExp(exp) => exp.rval_calc(scopes),
            Self::LOrAndExp(lexp, rexp) => {
                let lval = lexp.rval_calc(scopes).unwrap() != 0;
                let rval = rexp.rval_calc(scopes).unwrap() != 0;
                Ok((lval || rval) as i32)
            }
        }
    }
}
