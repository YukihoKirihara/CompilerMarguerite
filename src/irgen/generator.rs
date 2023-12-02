use super::function::FunctionInfo;
use super::scope_manager::ScopeManager;
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{FunctionData, Program, Type};

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
        /*
            int main() {
                return 0;
            }
            CompUnit {
                func_def: FuncDef {
                    func_type: Int,
                    ident: "main",
                    block: Block {
                        stmt: Stmt {
                            num: 0,
                        },
                    },
                },
            }
        */
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
        info.push_inst(program, entry_block, ret_val.unwrap());

        // Maintain the scopes, and go down the AST
        scopes.open();
        scopes.new_func(&self.ident, func);
        scopes.set_curr_func(info);
        self.block.generate(program, scopes);
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
        Ok(Type::get_i32())
    }
}

impl<'ast> IRGenerator<'ast> for Block {
    type Ret = ();
    fn generate(
        &'ast self,
        program: &mut Program,
        scopes: &mut ScopeManager<'ast>,
    ) -> Result<Self::Ret, ()> {
        scopes.open();
        self.stmt.generate(program, scopes);
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
        Ok(())
    }
}
