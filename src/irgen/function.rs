use koopa::ir::Value;
use koopa::ir::{builder::LocalBuilder, builder_traits::*};
use koopa::ir::{BasicBlock, Function, Program};

pub struct FunctionInfo {
    func: Function,
    entry: BasicBlock,
    curr: BasicBlock,
    ret_val: Option<Value>,
}

impl FunctionInfo {
    /// Create a new FunctionInfo
    pub fn new(func: Function, entry: BasicBlock, ret_val: Option<Value>) -> Self {
        Self {
            func,
            entry,
            curr: entry,
            ret_val,
        }
    }

    /// Return the current function
    pub fn func(&self) -> Function {
        self.func
    }

    /// Return the entry block
    pub fn entry(&self) -> BasicBlock {
        self.entry
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

    /// Add an instruction to the block
    pub fn push_inst(&mut self, program: &mut Program, bb: BasicBlock, inst: Value) {
        program
            .func_mut(self.func)
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }

    // Construct the end of the function
    pub fn conclude_func(&mut self, program: &mut Program) -> () {
        let value = self.create_new_value(program).load(self.ret_val.unwrap());
        self.push_inst(program, self.entry, value);
        let ret = self.create_new_value(program).ret(Some(value));
        self.push_inst(program, self.entry, ret)
    }
}
