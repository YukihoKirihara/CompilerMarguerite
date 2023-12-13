use super::function::FunctionInfo;
use super::scope_manager::ScopeManager;
use super::value;
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};

pub trait IRGenerator<'ast> {
    type Ret;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()>;
}

impl<'ast> IRGenerator<'ast> for CompUnit {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        self.func_def.generate(program, scopes)?;
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for FuncDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        let ret_type = self.func_type.generate(program, scopes)?;
        let params_type: Vec<Type> = Vec::new();
        // Create the new function
        let mut data = FunctionData::new(format!("@{}", self.ident), params_type, ret_type);
        // Generate the entry block
        let entry_block = data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        // Genereate the return value
        let alloc = data.dfg_mut().new_value().alloc(Type::get_i32());
        data.dfg_mut()
            .set_value_name(alloc, Some("%ret".to_string()));
        let ret_val = Some(alloc);

        // Generate the FunctionInfo
        let func = program.new_func(data);
        let mut info = FunctionInfo::new(func, entry_block, ret_val);
        info.push_bblock(program, entry_block);
        info.push_inst_curr_bblock(program, ret_val.unwrap());

        // Maintain the scopes, and go down the AST
        scopes.open();
        scopes.new_func(&self.ident, func)?;
        scopes.set_curr_func(info);
        self.block.generate(program, scopes)?;
        scopes.close();

        // The last part
        let info = scopes.mut_ref_curr_func().take().unwrap();
        info.conclude_func(program);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for FuncType {
    type Ret = Type;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            Self::Int => Ok(Type::get_i32()),
        }
    }
}

impl<'ast> IRGenerator<'ast> for Block {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        // Open a new scope
        scopes.open();
        // Go down the AST
        self.stmt.generate(program, scopes)?;
        // Close the scope
        scopes.close();
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Stmt {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        let _ = match self {
            Self::Return(ret) => ret.generate(program, scopes),
        };
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Return {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        let value = self.exp.generate(program, scopes).unwrap();
        let info = scopes.mut_ref_curr_func().unwrap();
        let ret_val = info.ret_val().unwrap();
        let store_inst = info.create_new_value(program).store(value, ret_val);
        info.push_inst_curr_bblock(program, store_inst);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Exp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        let value = self.lor_exp.generate(program, scopes).unwrap();
        Ok(value)
    }
}

impl<'ast> IRGenerator<'ast> for LOrExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.generate(program, scopes),
            LOrExp::LOrAndExp(lor_exp, land_exp) => {
                let lhs = lor_exp.generate(program, scopes).unwrap();
                let rhs = land_exp.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let lvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, lhs);
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                let value = info
                    .create_new_value(program)
                    .binary(BinaryOp::Or, lvalue, rvalue);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for LAndExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate(program, scopes),
            LAndExp::LAndEqExp(land_exp, eq_exp) => {
                let lhs = land_exp.generate(program, scopes).unwrap();
                let rhs = eq_exp.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let lvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, lhs);
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                let value = info
                    .create_new_value(program)
                    .binary(BinaryOp::And, lvalue, rvalue);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for EqExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate(program, scopes),
            EqExp::EqRelExp(eq_exp, eq_op, rel_exp) => {
                let lhs = eq_exp.generate(program, scopes).unwrap();
                let rhs = rel_exp.generate(program, scopes).unwrap();
                let op = eq_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for EqOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            EqOp::Eq => Ok(BinaryOp::Eq),
            EqOp::Neq => Ok(BinaryOp::NotEq),
        }
    }
}

impl<'ast> IRGenerator<'ast> for RelExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate(program, scopes),
            RelExp::RelAddExp(rel_exp, rel_op, add_exp) => {
                let lhs = rel_exp.generate(program, scopes).unwrap();
                let rhs = add_exp.generate(program, scopes).unwrap();
                let op = rel_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for RelOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            RelOp::LT => Ok(BinaryOp::Lt),
            RelOp::LE => Ok(BinaryOp::Le),
            RelOp::GT => Ok(BinaryOp::Gt),
            RelOp::GE => Ok(BinaryOp::Ge),
        }
    }
}

impl<'ast> IRGenerator<'ast> for AddExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.generate(program, scopes),
            AddExp::AddMulExp(add_exp, add_op, mul_exp) => {
                let lhs = add_exp.generate(program, scopes).unwrap();
                let rhs = mul_exp.generate(program, scopes).unwrap();
                let op = add_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for AddOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            AddOp::Add => Ok(BinaryOp::Add),
            AddOp::Sub => Ok(BinaryOp::Sub),
        }
    }
}

impl<'ast> IRGenerator<'ast> for MulExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.generate(program, scopes),
            MulExp::MulUnaryExp(mul_exp, mul_op, unary_exp) => {
                let lhs = mul_exp.generate(program, scopes).unwrap();
                let rhs = unary_exp.generate(program, scopes).unwrap();
                let op = mul_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for MulOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            MulOp::Mul => Ok(BinaryOp::Mul),
            MulOp::Div => Ok(BinaryOp::Div),
            MulOp::Mod => Ok(BinaryOp::Mod),
        }
    }
}

impl<'ast> IRGenerator<'ast> for UnaryExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            UnaryExp::PrimaryExp(pri_exp) => pri_exp.generate(program, scopes),
            UnaryExp::UnaryOpExp(unary_op, pri_exp) => {
                let rhs = pri_exp.generate(program, scopes).unwrap();
                let op = unary_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let value = info.create_new_value(program).binary(op, zero, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(value)
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for UnaryOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            UnaryOp::Neg => Ok(BinaryOp::Sub),
            UnaryOp::Not => Ok(BinaryOp::Eq),
        }
    }
}

impl<'ast> IRGenerator<'ast> for PrimaryExp {
    type Ret = Value;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        match self {
            PrimaryExp::ParenExp(exp) => exp.generate(program, scopes),
            PrimaryExp::Number(num) => {
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).integer(*num);
                Ok(value)
            }
        }
    }
}
