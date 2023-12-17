use super::error::IRGenError;
use super::exp_value::ExpValue;
use super::function::FunctionInfo;
use super::rval_calculator::RValCalculator;
use super::scope_manager::ScopeManager;
use super::variable::Variable;
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{BinaryOp, FunctionData, Program, Type};

pub trait IRGenerator<'ast> {
    type Ret;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError>;
}

impl<'ast> IRGenerator<'ast> for CompUnit {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.func_def.generate(program, scopes)?;
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Decl {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::ConstDecl(const_decl) => const_decl.generate(program, scopes),
            Self::VarDecl(var_decl) => var_decl.generate(program, scopes),
        }
    }
}

impl<'ast> IRGenerator<'ast> for ConstDecl {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        for const_def in &self.const_defs {
            const_def.generate(program, scopes)?;
        }
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for BType {
    type Ret = Type;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::Int => Ok(Type::get_i32()),
        }
    }
}

impl<'ast> IRGenerator<'ast> for ConstDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Generate the initial value of the constant.
        let init_val = self.const_init_val.generate(program, scopes).unwrap();
        // Generate the variable.
        let var = Variable::Const(init_val);
        // Add the constant to current scope. Duplicate Identifier Error may occur.
        scopes.create_new_variable(self.ident.as_str(), var)?;
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for ConstInitVal {
    type Ret = i32;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.const_exp.generate(program, scopes)
    }
}

impl<'ast> IRGenerator<'ast> for VarDecl {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        for var_def in &self.var_defs {
            var_def.generate(program, scopes)?;
        }
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for VarDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Generate the initial value
        let rvalue = if let Some(init_value) = &self.init_val {
            Some(
                init_value
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap(),
            )
        } else {
            None
        };
        // Generate the variable.
        let info = scopes.mut_ref_curr_func().unwrap();
        let alloc = info.create_new_value(program).alloc(Type::get_i32());
        program
            .func_mut(info.func())
            .dfg_mut()
            .set_value_name(alloc, Some(format!("@{}", self.ident)));
        info.push_inst_curr_bblock(program, alloc);
        if rvalue.is_some() {
            let store = info.create_new_value(program).store(rvalue.unwrap(), alloc);
            info.push_inst_curr_bblock(program, store);
        }
        let var = Variable::Value(alloc);
        scopes.create_new_variable(&self.ident, var)?;
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for InitVal {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.exp.generate(program, scopes)
    }
}

impl<'ast> IRGenerator<'ast> for FuncDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let ret_type = self.func_type.generate(program, scopes)?;
        let params_type: Vec<Type> = Vec::new();
        // Create the new function
        let mut data = FunctionData::new(format!("@{}", self.ident), params_type, ret_type);
        // Generate the entry, exit and current blocks
        let entry_block = data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let curr_block = data.dfg_mut().new_bb().basic_block(None);
        let exit_block = data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%end".to_string()));
        // Genereate the return value
        let alloc = data.dfg_mut().new_value().alloc(Type::get_i32());
        data.dfg_mut()
            .set_value_name(alloc, Some("%ret".to_string()));
        let ret_val = Some(alloc);

        // Generate the FunctionInfo
        let func = program.new_func(data);
        let mut info = FunctionInfo::new(func, entry_block, exit_block, ret_val);
        info.push_bblock(program, entry_block);
        info.push_inst_curr_bblock(program, ret_val.unwrap());
        info.push_bblock(program, curr_block);
        let jump = info.create_new_value(program).jump(curr_block);
        info.push_inst(program, entry_block, jump);

        // Maintain the scopes, and go down the AST
        scopes.open();
        scopes.new_func(&self.ident, func)?;
        scopes.set_curr_func(info);
        self.block.generate(program, scopes)?;
        scopes.close();

        // The last part
        let info = scopes.mut_ref_curr_func().unwrap();
        info.conclude_func(program);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for FuncType {
    type Ret = Type;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
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
    ) -> Result<Self::Ret, IRGenError> {
        // Open a new scope
        scopes.open();
        // Go down the AST
        for block_item in &self.block_items {
            block_item.generate(program, scopes)?;
        }
        // Close the scope
        scopes.close();
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for BlockItem {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::Decl(decl) => decl.generate(program, scopes),
            Self::Stmt(stmt) => stmt.generate(program, scopes),
        }
    }
}

impl<'ast> IRGenerator<'ast> for Stmt {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let _ = match self {
            Self::Assign(assign) => assign.generate(program, scopes),
            Self::Return(ret) => ret.generate(program, scopes),
        };
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Assign {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let exp = self
            .exp
            .generate(program, scopes)
            .unwrap()
            .get_int_value(program, scopes)
            .unwrap();
        let lval = self
            .lval
            .generate(program, scopes)
            .unwrap()
            .get_int_ptr()
            .unwrap();
        let info = scopes.mut_ref_curr_func().unwrap();
        let store = info.create_new_value(program).store(exp, lval);
        info.push_inst_curr_bblock(program, store);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Return {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let value = self
            .exp
            .generate(program, scopes)
            .unwrap()
            .get_int_value(program, scopes)
            .unwrap();
        let info = scopes.mut_ref_curr_func().unwrap();
        let ret_val = info.ret_val().unwrap();
        let store = info.create_new_value(program).store(value, ret_val);
        info.push_inst_curr_bblock(program, store);
        let jump = info.create_new_value(program).jump(info.exit());
        info.push_inst_curr_bblock(program, jump);
        let new_bblock = info.create_new_bblock(program, None);
        info.push_bblock(program, new_bblock);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Exp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.lor_exp.generate(program, scopes)
    }
}

impl<'ast> IRGenerator<'ast> for LVal {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let value = match scopes.load_variable(self.ident.as_str()).unwrap() {
            Variable::Const(num) => {
                let info = scopes.ref_curr_func().unwrap();
                let val = info.create_new_value(program).integer(*num);
                ExpValue::Int(val)
            }
            Variable::Value(value) => ExpValue::IntPtr(*value),
        };
        Ok(value)
    }
}

impl<'ast> IRGenerator<'ast> for PrimaryExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            PrimaryExp::ParenExp(exp) => exp.generate(program, scopes),
            PrimaryExp::LVal(lval) => lval.generate(program, scopes),
            PrimaryExp::Number(num) => {
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).integer(*num);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for UnaryExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            UnaryExp::PrimaryExp(pri_exp) => pri_exp.generate(program, scopes),
            UnaryExp::UnaryOpExp(unary_op, pri_exp) => {
                let rhs = pri_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let op = unary_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let value = info.create_new_value(program).binary(op, zero, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for UnaryOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            UnaryOp::Neg => Ok(BinaryOp::Sub),
            UnaryOp::Not => Ok(BinaryOp::Eq),
        }
    }
}

impl<'ast> IRGenerator<'ast> for MulExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.generate(program, scopes),
            MulExp::MulUnaryExp(mul_exp, mul_op, unary_exp) => {
                let lhs = mul_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = unary_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let op = mul_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for MulOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            MulOp::Mul => Ok(BinaryOp::Mul),
            MulOp::Div => Ok(BinaryOp::Div),
            MulOp::Mod => Ok(BinaryOp::Mod),
        }
    }
}

impl<'ast> IRGenerator<'ast> for AddExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.generate(program, scopes),
            AddExp::AddMulExp(add_exp, add_op, mul_exp) => {
                let lhs = add_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = mul_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let op = add_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for AddOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            AddOp::Add => Ok(BinaryOp::Add),
            AddOp::Sub => Ok(BinaryOp::Sub),
        }
    }
}

impl<'ast> IRGenerator<'ast> for RelExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate(program, scopes),
            RelExp::RelAddExp(rel_exp, rel_op, add_exp) => {
                let lhs = rel_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = add_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let op = rel_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for RelOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            RelOp::LT => Ok(BinaryOp::Lt),
            RelOp::LE => Ok(BinaryOp::Le),
            RelOp::GT => Ok(BinaryOp::Gt),
            RelOp::GE => Ok(BinaryOp::Ge),
        }
    }
}

impl<'ast> IRGenerator<'ast> for EqExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate(program, scopes),
            EqExp::EqRelExp(eq_exp, eq_op, rel_exp) => {
                let lhs = eq_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = rel_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let op = eq_op.generate(program, scopes).unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let value = info.create_new_value(program).binary(op, lhs, rhs);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for EqOp {
    type Ret = BinaryOp;
    fn generate(
        &'ast self,
        _program: &mut Program,
        _scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            EqOp::Eq => Ok(BinaryOp::Eq),
            EqOp::Neq => Ok(BinaryOp::NotEq),
        }
    }
}

impl<'ast> IRGenerator<'ast> for LAndExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate(program, scopes),
            LAndExp::LAndEqExp(land_exp, eq_exp) => {
                let lhs = land_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = eq_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let lvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, lhs);
                info.push_inst_curr_bblock(program, lvalue);
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                info.push_inst_curr_bblock(program, rvalue);
                let value = info
                    .create_new_value(program)
                    .binary(BinaryOp::And, lvalue, rvalue);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for LOrExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.generate(program, scopes),
            LOrExp::LOrAndExp(lor_exp, land_exp) => {
                let lhs = lor_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let rhs = land_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let zero = info.create_new_value(program).integer(0);
                let lvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, lhs);
                info.push_inst_curr_bblock(program, lvalue);
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                info.push_inst_curr_bblock(program, rvalue);
                let value = info
                    .create_new_value(program)
                    .binary(BinaryOp::Or, lvalue, rvalue);
                info.push_inst_curr_bblock(program, value);
                Ok(ExpValue::Int(value))
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for ConstExp {
    type Ret = i32;
    fn generate(
        &'ast self,
        _program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.exp.rval_calc(scopes)
    }
}
