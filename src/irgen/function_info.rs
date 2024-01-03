use super::error::IRGenError;
use koopa::ir::builder::LocalBuilder;
use koopa::ir::builder_traits::*;
use koopa::ir::Value;
use koopa::ir::{BasicBlock, Function, Program, Type};

/// FunctionInfo is a struct to record information of a function
pub struct FunctionInfo {
    func: Function,
    entry: BasicBlock,
    exit: BasicBlock,
    curr: BasicBlock,
    ret_val: Option<Value>,
}

impl FunctionInfo {
    /// Create a new FunctionInfo
    pub fn new(
        func: Function,
        entry: BasicBlock,
        exit: BasicBlock,
        ret_val: Option<Value>,
    ) -> Self {
        Self {
            func,
            entry,
            exit,
            curr: entry,
            ret_val,
        }
    }

    /// Return the Function structure
    pub fn func(&self) -> Function {
        self.func
    }

    /// Return the exit block
    pub fn exit(&self) -> BasicBlock {
        self.exit
    }

    /// Return the current block
    pub fn curr_bblock(&self) -> BasicBlock {
        self.curr
    }

    /// Return the return value
    pub fn ret_val(&self) -> Option<Value> {
        self.ret_val
    }

    /// Create a new basic block
    pub fn create_new_bblock(&self, program: &mut Program, name: Option<&str>) -> BasicBlock {
        program
            .func_mut(self.func)
            .dfg_mut()
            .new_bb()
            .basic_block(name.map(|s| s.to_string()))
    }

    /// Add a basic block as the current one
    pub fn push_bblock(&mut self, program: &mut Program, bb: BasicBlock) -> () {
        program
            .func_mut(self.func)
            .layout_mut()
            .bbs_mut()
            .push_key_back(bb)
            .unwrap();
        self.curr = bb;
    }

    /// Create a new value
    pub fn create_new_value<'a>(&self, program: &'a mut Program) -> LocalBuilder<'a> {
        program.func_mut(self.func).dfg_mut().new_value()
    }

    /// Create a new allocation.
    pub fn create_new_allocation(
        &self,
        program: &mut Program,
        ty: Type,
        bald_name: Option<&String>,
    ) -> Result<Value, IRGenError> {
        let alloc = self.create_new_value(program).alloc(ty);
        let name = match bald_name {
            Some(s) => Some(format!("@{}", s)),
            None => None,
        };
        program
            .func_mut(self.func)
            .dfg_mut()
            .set_value_name(alloc, name);
        self.push_inst(program, self.entry, alloc);
        Ok(alloc)
    }

    /// Add an instruction to the block
    pub fn push_inst(&self, program: &mut Program, bb: BasicBlock, inst: Value) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    /// Add an instruction to the current block. A wrapped function just for convenience.
    pub fn push_inst_curr_bblock(&self, program: &mut Program, inst: Value) {
        let bb = self.curr_bblock();
        self.push_inst(program, bb, inst);
    }

    /// Construct the end of the function
    pub fn conclude_func(&mut self, program: &mut Program) -> () {
        // Jump to the exit bblock
        let jump = self.create_new_value(program).jump(self.exit);
        self.push_inst_curr_bblock(program, jump);
        // Push the exit bblock
        self.push_bblock(program, self.exit);
        // Generate return instructions
        let ret_val_opt = match self.ret_val() {
            Some(value) => {
                let load = self.create_new_value(program).load(value);
                self.push_inst_curr_bblock(program, load);
                Some(load)
            }
            None => None,
        };
        let ret = self.create_new_value(program).ret(ret_val_opt);
        self.push_inst_curr_bblock(program, ret);
    }
}
