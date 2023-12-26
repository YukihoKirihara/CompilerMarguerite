use std::vec;

use super::array::ArrayType;
use super::error::IRGenError;
use super::exp_value::ExpValue;
use super::function_info::FunctionInfo;
use super::helper::get_array_type;
use super::rval_calculator::RValCalculator;
use super::scope_manager::ScopeManager;
use super::variable::Variable;
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{BinaryOp, FunctionData, Program, Type, TypeKind};

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
        // Pre-declare all SysY library functions:
        // decl @getint(): i32
        // decl @getch(): i32
        // decl @getarray(*i32): i32
        // decl @putint(i32)
        // decl @putch(i32)
        // decl @putarray(i32, *i32)
        // decl @starttime()
        // decl @stoptime()
        scopes.new_func(
            "getint",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "getint"),
                vec![],
                Type::get_i32(),
            )),
        )?;
        scopes.new_func(
            "getch",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "getch"),
                vec![],
                Type::get_i32(),
            )),
        )?;
        scopes.new_func(
            "getarray",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "getarray"),
                vec![Type::get_pointer(Type::get_i32())],
                Type::get_i32(),
            )),
        )?;
        scopes.new_func(
            "putint",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "putint"),
                vec![Type::get_i32()],
                Type::get_unit(),
            )),
        )?;
        scopes.new_func(
            "putch",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "putch"),
                vec![Type::get_i32()],
                Type::get_unit(),
            )),
        )?;
        scopes.new_func(
            "putarray",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "putarray"),
                vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
                Type::get_unit(),
            )),
        )?;
        scopes.new_func(
            "starttime",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "starttime"),
                vec![],
                Type::get_unit(),
            )),
        )?;
        scopes.new_func(
            "stoptime",
            program.new_func(FunctionData::new_decl(
                format!("@{}", "stoptime"),
                vec![],
                Type::get_unit(),
            )),
        )?;
        for items in &self.global_items {
            items.generate(program, scopes)?;
        }
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for GlobalItem {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::Decl(decl) => decl.generate(program, scopes),
            Self::FuncDef(func_def) => func_def.generate(program, scopes),
        }
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

impl<'ast> IRGenerator<'ast> for ConstDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Deduce the type of the constant, which can be a single integer or a nested array.
        let const_type = get_array_type(&self.dims, scopes).unwrap();
        // Generate the initial value of the constant.
        let init_val = self.const_init_val.generate(program, scopes).unwrap();
        // Reshape the initial value.
        let reshaped_init_val = init_val.reshape(&const_type).unwrap();
        if const_type.is_i32() {
            // A single constant integer.
            // Generate the variable.
            let var = Variable::Const(reshaped_init_val.get_const_i32().unwrap());
            // Add the constant to current scope. Duplicate Identifier Error may occur.
            scopes.create_new_variable(self.ident.as_str(), var)?;
        } else {
            // A constant array.
            let value = if scopes.is_global() {
                let init = reshaped_init_val.get_const_value(program, scopes).unwrap();
                let alloc = program.new_value().global_alloc(init);
                program.set_value_name(alloc, Some(format!("@{}", self.ident)));
                alloc
            } else {
                let info = scopes.mut_ref_curr_func().unwrap();
                let alloc = info
                    .create_new_allocation(program, const_type, Some(&self.ident))
                    .unwrap();
                reshaped_init_val.push_store_inst(program, scopes, alloc)?;
                alloc
            };
            scopes.create_new_variable(&self.ident, Variable::Value(value))?;
        }
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for ConstInitVal {
    type Ret = ArrayType;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::ConstArrayInitVal(init_vals) => Ok(ArrayType::Array(
                init_vals
                    .into_iter()
                    .map(|init_val| init_val.generate(program, scopes))
                    .collect::<Result<_, _>>()
                    .unwrap(),
            )),
            Self::ConstExp(exp) => Ok(ArrayType::BaseConst(exp.generate(program, scopes).unwrap())),
        }
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
        // Deduce the type, which can be a single integer or a nested array.
        let _type = get_array_type(&self.dims, scopes).unwrap();
        // Generate the initial value
        let reshaped_init_val = match self.init_val.as_ref() {
            Some(_init_val) => Some(
                _init_val
                    .generate(program, scopes)
                    .unwrap()
                    .reshape(&_type)
                    .unwrap(),
            ),
            None => None,
        };
        let value = if scopes.is_global() {
            let global_init = match reshaped_init_val {
                Some(_init_val) => _init_val.get_const_value(program, scopes).unwrap(),
                None => program.new_value().zero_init(_type),
            };
            let value = program.new_value().global_alloc(global_init);
            program.set_value_name(value, Some(format!("@{}", self.ident)));
            value
        } else {
            let info = scopes.mut_ref_curr_func().unwrap();
            let alloc = info
                .create_new_allocation(program, _type, Some(&self.ident))
                .unwrap();
            if let Some(_init_val) = reshaped_init_val {
                _init_val.push_store_inst(program, scopes, alloc)?;
            }
            alloc
        };
        scopes.create_new_variable(&self.ident, Variable::Value(value))?;
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for InitVal {
    type Ret = ArrayType;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match self {
            Self::ArrayInitVal(init_vals) => Ok(ArrayType::Array(
                init_vals
                    .into_iter()
                    .map(|init_val| init_val.generate(program, scopes))
                    .collect::<Result<_, _>>()
                    .unwrap(),
            )),
            Self::Exp(exp) => {
                if scopes.is_global() {
                    Ok(ArrayType::BaseConst(exp.rval_calc(scopes).unwrap()))
                } else {
                    Ok(ArrayType::BaseValue(
                        exp.generate(program, scopes)
                            .unwrap()
                            .get_int_value(program, scopes)
                            .unwrap(),
                    ))
                }
            }
        }
    }
}

impl<'ast> IRGenerator<'ast> for FuncDef {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Generate the types of formal parameters and return value.
        let mut params_type = vec![];
        for func_fparam in &self.func_fparams {
            params_type.push(func_fparam.generate(program, scopes).unwrap());
        }
        let ret_type = self.func_type.generate(program, scopes)?;
        // Create the new function
        let mut data = FunctionData::new(format!("@{}", self.ident), params_type, ret_type);
        let params = data.params().to_owned();
        // Generate the entry, exit and current blocks
        let entry_block = data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let curr_block = data.dfg_mut().new_bb().basic_block(None);
        let exit_block = data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%exit".to_string()));
        // Genereate the return value
        let ret_val = match self.func_type {
            FuncType::Void => None,
            FuncType::Int => {
                let alloc = data.dfg_mut().new_value().alloc(Type::get_i32());
                data.dfg_mut()
                    .set_value_name(alloc, Some("%ret".to_string()));
                Some(alloc)
            }
        };
        // Generate the FunctionInfo
        let func = program.new_func(data);
        let mut info = FunctionInfo::new(func, entry_block, exit_block, ret_val);
        info.push_bblock(program, entry_block);
        if ret_val.is_some() {
            info.push_inst_curr_bblock(program, ret_val.unwrap());
        }
        info.push_bblock(program, curr_block);

        // Maintain the scopes, and go down the AST
        scopes.open();
        // Allocate and store the input parameters
        for (fparam, rparam) in self.func_fparams.iter().zip(params) {
            let _type = program.func(func).dfg().value(rparam).ty().clone();
            let alloc = info
                .create_new_allocation(program, _type, Some(&fparam.ident))
                .unwrap();
            let store = info.create_new_value(program).store(rparam, alloc);
            info.push_inst_curr_bblock(program, store);
            scopes.create_new_variable(&fparam.ident, Variable::Value(alloc))?;
        }
        // Record the current function in scopes
        scopes.new_func(&self.ident, func)?;
        scopes.set_curr_func(info);
        // Go down the block
        self.block.generate(program, scopes)?;
        scopes.close();

        // Conclude the entry block
        let info = scopes.mut_ref_curr_func().unwrap();
        let jump = info.create_new_value(program).jump(curr_block);
        info.push_inst(program, entry_block, jump);
        // The last part
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
            Self::Void => Ok(Type::get_unit()),
            Self::Int => Ok(Type::get_i32()),
        }
    }
}

impl<'ast> IRGenerator<'ast> for FuncFParam {
    type Ret = Type;
    fn generate(
        &'ast self,
        _program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        match &self.sub_dims {
            Some(dims) => Ok(Type::get_pointer(get_array_type(&dims, scopes).unwrap())),
            None => Ok(Type::get_i32()),
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
            Self::IdleExp(exp) => exp.generate(program, scopes),
            Self::Block(block) => block.generate(program, scopes),
            Self::IfClause(if_clause) => if_clause.generate(program, scopes),
            Self::WhileClause(while_clause) => while_clause.generate(program, scopes),
            Self::Break(break_stmt) => break_stmt.generate(program, scopes),
            Self::Continue(ctn_stmt) => ctn_stmt.generate(program, scopes),
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

impl<'ast> IRGenerator<'ast> for IdleExp {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        if let Some(exp) = &self.exp {
            exp.generate(program, scopes)?;
        }
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for IfClause {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let cond = self
            .cond
            .generate(program, scopes)
            .unwrap()
            .get_int_value(program, scopes)
            .unwrap();
        let info = scopes.mut_ref_curr_func().unwrap();
        // Generate the true and false bblock.
        let true_bblock = info.create_new_bblock(program, Some("%if_true"));
        let false_bblock = info.create_new_bblock(program, Some("%if_false"));
        let final_bblock = info.create_new_bblock(program, Some("%if_final"));
        // Generate the branch instruction.
        let branch = info
            .create_new_value(program)
            .branch(cond, true_bblock, false_bblock);
        info.push_inst_curr_bblock(program, branch);
        // Go down the true bblock.
        info.push_bblock(program, true_bblock);
        self.true_stmt.generate(program, scopes)?;
        let info = scopes.mut_ref_curr_func().unwrap();
        let true_jump = info.create_new_value(program).jump(final_bblock);
        info.push_inst_curr_bblock(program, true_jump);
        // Go down the false bblock.
        info.push_bblock(program, false_bblock);
        if let Some(false_stmt) = &self.false_stmt {
            false_stmt.generate(program, scopes)?;
        }
        let info = scopes.mut_ref_curr_func().unwrap();
        let false_jump = info.create_new_value(program).jump(final_bblock);
        info.push_inst_curr_bblock(program, false_jump);
        info.push_bblock(program, final_bblock);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for WhileClause {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Generate the bblock for while condition.
        let info = scopes.mut_ref_curr_func().unwrap();
        let while_cond_bblock = info.create_new_bblock(program, Some("%while_cond"));
        let start_jump = info.create_new_value(program).jump(while_cond_bblock);
        info.push_inst_curr_bblock(program, start_jump);
        info.push_bblock(program, while_cond_bblock);
        // Go down the while condition expression.
        let cond = self
            .cond
            .generate(program, scopes)
            .unwrap()
            .get_int_value(program, scopes)
            .unwrap();
        // Generate the bblocks for while body and while end.
        let info = scopes.mut_ref_curr_func().unwrap();
        let while_body_bblock = info.create_new_bblock(program, Some("%while_body"));
        let while_final_bblock = info.create_new_bblock(program, Some("%while_final"));
        // Generate the branch instructions.
        let branch =
            info.create_new_value(program)
                .branch(cond, while_body_bblock, while_final_bblock);
        info.push_inst_curr_bblock(program, branch);
        // Enter the while body.
        info.push_bblock(program, while_body_bblock);
        scopes.enter_loop(while_cond_bblock, while_final_bblock);
        // Go down the statement of the body.
        self.loop_stmt.generate(program, scopes)?;
        // Exit the while body.
        scopes.exit_loop();
        let info = scopes.mut_ref_curr_func().unwrap();
        let back_jump = info.create_new_value(program).jump(while_cond_bblock);
        info.push_inst_curr_bblock(program, back_jump);
        // Place the final bblock.
        info.push_bblock(program, while_final_bblock);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Break {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let final_bblock = scopes.get_curr_loop_final_bblock().unwrap();
        let info = scopes.ref_curr_func().unwrap();
        let break_jump = info.create_new_value(program).jump(*final_bblock);
        let info = scopes.mut_ref_curr_func().unwrap();
        info.push_inst_curr_bblock(program, break_jump);
        // Push a new bblock
        let new_bblock = info.create_new_bblock(program, None);
        info.push_bblock(program, new_bblock);
        Ok(())
    }
}

impl<'ast> IRGenerator<'ast> for Continue {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        let cond_bblock = scopes.get_curr_loop_cond_bblock().unwrap();
        let info = scopes.ref_curr_func().unwrap();
        let continue_jump = info.create_new_value(program).jump(*cond_bblock);
        let info = scopes.mut_ref_curr_func().unwrap();
        info.push_inst_curr_bblock(program, continue_jump);
        // Push a new bblock
        let new_bblock = info.create_new_bblock(program, None);
        info.push_bblock(program, new_bblock);
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
        if let Some(ret_exp) = &self.exp {
            let value = ret_exp
                .generate(program, scopes)
                .unwrap()
                .get_int_value(program, scopes)
                .unwrap();
            let info = scopes.mut_ref_curr_func().unwrap();
            let ret_val = info.ret_val().unwrap();
            let store = info.create_new_value(program).store(value, ret_val);
            info.push_inst_curr_bblock(program, store);
        }
        let info = scopes.mut_ref_curr_func().unwrap();
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
        match scopes.load_variable(self.ident.as_str()).unwrap() {
            Variable::Const(num) => {
                let info = scopes.ref_curr_func().unwrap();
                let val = info.create_new_value(program).integer(*num);
                return Ok(ExpValue::Int(val));
            }
            Variable::Value(value) => {
                // Generate the type and dimension of the left value
                let mut value = *value;
                let mut is_ptr_ptr = false;
                let mut dims = match scopes.get_value_type(program, value).kind() {
                    TypeKind::Pointer(base) => {
                        let mut _type = base;
                        let mut _dims = 0;
                        loop {
                            _type = match _type.kind() {
                                TypeKind::Pointer(base) => {
                                    is_ptr_ptr = true;
                                    base
                                }
                                TypeKind::Array(base, _) => base,
                                _ => break _dims,
                            };
                            _dims += 1;
                        }
                    }
                    _ => 0,
                };
                // Load the value if it is a pointer to a pointer
                /*
                    int (*arr) [3];
                    arr[1][2];

                    @arr = alloc *[i32, 3]        // @arr   **[i32, 3]
                    %ptr1 = load @arr             // %ptr1  *[i32, 3]
                    %ptr2 = getptr %ptr1, 1       // %ptr2  *[i32, 3]
                    %ptr3 = getelemptr %ptr2, 2   // %ptr3  *i32
                    %value = load %ptr3           // %value i32
                */
                if is_ptr_ptr {
                    let info = scopes.ref_curr_func().unwrap();
                    value = info.create_new_value(program).load(value);
                    info.push_inst_curr_bblock(program, value);
                }
                // Go down each dimension and load the value
                for (i, idx) in self.idxs.iter().enumerate() {
                    dims -= 1;
                    let index = idx
                        .generate(program, scopes)
                        .unwrap()
                        .get_int_value(program, scopes)
                        .unwrap();
                    let info = scopes.ref_curr_func().unwrap();
                    value = if is_ptr_ptr && i == 0 {
                        info.create_new_value(program).get_ptr(value, index)
                    } else {
                        info.create_new_value(program).get_elem_ptr(value, index)
                    };
                    info.push_inst_curr_bblock(program, value);
                }
                if dims == 0 {
                    Ok(ExpValue::IntPtr(value))
                } else {
                    // Fill the rest dimensions with 0
                    if !is_ptr_ptr || !self.idxs.is_empty() {
                        let info = scopes.ref_curr_func().unwrap();
                        let zero = info.create_new_value(program).integer(0);
                        value = info.create_new_value(program).get_elem_ptr(value, zero);
                        info.push_inst_curr_bblock(program, value);
                    }
                    Ok(ExpValue::ArrPtr(value))
                }
            }
        }
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
            UnaryExp::FuncExp(func_exp) => func_exp.generate(program, scopes),
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

impl<'ast> IRGenerator<'ast> for FuncExp {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        // Generate the real parameters
        let mut rparams = vec![];
        for rparam in &self.func_rparams {
            rparams.push(
                rparam
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap(),
            );
        }
        // Load the function, its formal parameters and its return value
        let func = scopes.load_function(&self.ident).unwrap();
        let (fparams_type, ret_val) = match program.func(func).ty().kind() {
            TypeKind::Function(params, ret_val) => (params.clone(), ret_val.clone()),
            _ => unreachable!(),
        };
        // Checks that the formal and real parameters are correspondent
        if fparams_type.len() != rparams.len() {
            return Err(IRGenError::UnmathedParams(self.ident.to_string()));
        }
        for (fparam_type, rparam) in fparams_type.iter().zip(&rparams) {
            if *fparam_type != scopes.get_value_type(program, *rparam) {
                return Err(IRGenError::UnmathedParams(self.ident.to_string()));
            }
        }
        // Generate the call instruction
        let info = scopes.mut_ref_curr_func().unwrap();
        let call = info.create_new_value(program).call(func, rparams);
        info.push_inst_curr_bblock(program, call);
        if ret_val.is_i32() {
            Ok(ExpValue::Int(call))
        } else {
            Ok(ExpValue::Void)
        }
    }
}

impl<'ast> IRGenerator<'ast> for FuncRParam {
    type Ret = ExpValue;
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, IRGenError> {
        self.exp.generate(program, scopes)
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
                // A local allocation for the result value
                let info = scopes.mut_ref_curr_func().unwrap();
                let alloc = info.create_new_value(program).alloc(Type::get_i32());
                info.push_inst_curr_bblock(program, alloc);
                // Generate the basic blocks for right argument and the final
                let right_bblock = info.create_new_bblock(program, Some("%land_right"));
                let final_bblock = info.create_new_bblock(program, Some("%land_final"));
                // Go down the left argument and convert it into logical value
                let lhs = land_exp
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
                // Store the value of left argument into result
                let lstore = info.create_new_value(program).store(lvalue, alloc);
                info.push_inst_curr_bblock(program, lstore);
                // If the left argument is False, skip the right bblock
                let cond = info
                    .create_new_value(program)
                    .binary(BinaryOp::Eq, lvalue, zero);
                info.push_inst_curr_bblock(program, cond);
                let branch =
                    info.create_new_value(program)
                        .branch(cond, final_bblock, right_bblock);
                info.push_inst_curr_bblock(program, branch);
                // Go down the right argument and convert it into logical value
                info.push_bblock(program, right_bblock);
                let rhs = eq_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                info.push_inst_curr_bblock(program, rvalue);
                // Store the value of right argument into result.
                // Implemented iff the left argument is True. (a == True) -> ((a && b) == b)
                let rstore = info.create_new_value(program).store(rvalue, alloc);
                info.push_inst_curr_bblock(program, rstore);
                // Jump to the final bblock
                let jump = info.create_new_value(program).jump(final_bblock);
                info.push_inst_curr_bblock(program, jump);
                info.push_bblock(program, final_bblock);
                // Return the result value
                let load = info.create_new_value(program).load(alloc);
                info.push_inst_curr_bblock(program, load);
                Ok(ExpValue::Int(load))
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
                // A local allocation for the result value
                let info = scopes.mut_ref_curr_func().unwrap();
                let alloc = info.create_new_value(program).alloc(Type::get_i32());
                info.push_inst_curr_bblock(program, alloc);
                // Generate the basic blocks for right argument and the final
                let right_bblock = info.create_new_bblock(program, Some("%lor_right"));
                let final_bblock = info.create_new_bblock(program, Some("%lor_final"));
                // Go down the left argument and convert it into logical value
                let lhs = lor_exp
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
                // Store the value of left argument into result
                let lstore = info.create_new_value(program).store(lvalue, alloc);
                info.push_inst_curr_bblock(program, lstore);
                // If the left argument is True, skip the right bblock
                let cond = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, lvalue, zero);
                info.push_inst_curr_bblock(program, cond);
                let branch =
                    info.create_new_value(program)
                        .branch(cond, final_bblock, right_bblock);
                info.push_inst_curr_bblock(program, branch);
                // Go down the right argument and convert it into logical value
                info.push_bblock(program, right_bblock);
                let rhs = land_exp
                    .generate(program, scopes)
                    .unwrap()
                    .get_int_value(program, scopes)
                    .unwrap();
                let info = scopes.mut_ref_curr_func().unwrap();
                let rvalue = info
                    .create_new_value(program)
                    .binary(BinaryOp::NotEq, zero, rhs);
                info.push_inst_curr_bblock(program, rvalue);
                // Store the value of right argument into result.
                // Implemented iff the left argument is False. (a == False) -> ((a || b) == b)
                let rstore = info.create_new_value(program).store(rvalue, alloc);
                info.push_inst_curr_bblock(program, rstore);
                // Jump to the final bblock
                let jump = info.create_new_value(program).jump(final_bblock);
                info.push_inst_curr_bblock(program, jump);
                info.push_bblock(program, final_bblock);
                // Return the result value
                let load = info.create_new_value(program).load(alloc);
                info.push_inst_curr_bblock(program, load);
                Ok(ExpValue::Int(load))
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
