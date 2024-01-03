use super::error::ASMGenError;
use koopa::ir::{
    entities::{BasicBlock, Function, ValueData},
    TypeKind, ValueKind,
};
use std::cmp::max;
use std::collections::HashMap;

pub struct Slot {
    /// The offset from the stack pointer to the allocated space
    pub offset: usize,
    /// Whether the slot stores a pointer value
    pub is_ptr: bool,
}

impl Slot {
    pub fn new(offset: usize, is_ptr: bool) -> Self {
        Self {
            offset: offset,
            is_ptr: is_ptr,
        }
    }
}

/// FunctionInfo is a struct to record information of the function
pub struct FunctionInfo {
    /// The current function
    func: Function,
    /// A map from a value to its stack slot
    /// Slot.offset here is relative to the beginning of local values segments (S)
    value_to_stack_slot: HashMap<*const ValueData, Slot>,
    /// The total size of the allocated stack space
    alloc_total: usize,
    /// A map from the basic block to the name of its corresponding label
    bblock_to_label: HashMap<BasicBlock, String>,
    /// The maximum number of parameters of all function calls. Set None iff no function call occurs.
    max_arg_num: Option<usize>,
    /// The total offset of the stack frame
    stack_offset: Option<usize>,
}

impl FunctionInfo {
    /// Create a new FunctionInfo
    pub fn new(func: Function) -> Self {
        Self {
            func,
            value_to_stack_slot: HashMap::new(),
            alloc_total: 0 as usize,
            bblock_to_label: HashMap::new(),
            max_arg_num: None,
            stack_offset: None,
        }
    }

    /// Return the current function
    pub fn get_function(&self) -> Function {
        self.func
    }

    /// Record the map from a basic block to its label
    pub fn record_bblock_to_label(&mut self, bblock: BasicBlock, name: String) {
        self.bblock_to_label.insert(bblock, name);
    }

    /// Return the label name of a basic block
    pub fn get_bblock_label_name(&self, bblock: &BasicBlock) -> Result<String, ASMGenError> {
        match self.bblock_to_label.get(bblock) {
            Some(s) => Ok(s.clone()),
            None => Err(ASMGenError::BasicBlockNotFound),
        }
    }

    /// Allocate a stack slot for a value
    pub fn allocate_slot_for_value(&mut self, value_data: &ValueData) {
        match value_data.kind() {
            ValueKind::Alloc(_) => {
                let slot = Slot::new(self.alloc_total, false);
                self.value_to_stack_slot.insert(value_data, slot);
                self.alloc_total += match value_data.ty().kind() {
                    TypeKind::Pointer(base) => base.size(),
                    _ => unreachable!(),
                }
            }
            _ => {
                let is_ptr = match value_data.ty().kind() {
                    TypeKind::Pointer(_) => true,
                    _ => false,
                };
                let slot = Slot::new(self.alloc_total, is_ptr);
                self.value_to_stack_slot.insert(value_data, slot);
                self.alloc_total += value_data.ty().size();
            }
        }
    }

    /// Update the maximum number of parameters
    pub fn update_max_args_num(&mut self, args_num: usize) {
        let tmp = match self.max_arg_num {
            Some(curr_num) => Some(max(curr_num, args_num)),
            None => Some(args_num),
        };
        self.max_arg_num = tmp;
    }

    /// Whether the function is a leaf function
    pub fn is_leaf(&self) -> bool {
        match self.max_arg_num {
            Some(_) => false,
            _ => true,
        }
    }

    /// Calculate the total offset of the stack frame
    /// S' = ((R + S + A) + 15) / 16 * 16, where
    ///     R is the stack space for register %ra,
    ///     S is the stack space for local values, and
    ///     A is the stack space for parameters of function calls.
    /// R = if exists "call" then 4 else 0.  
    /// A = max{{the maximum number of parameters} - 8, 0} * 4,
    pub fn calculate_stack_offset(&mut self) -> usize {
        let s = self.alloc_total;
        let r = if self.is_leaf() { 0 } else { 4 };
        let a = match self.max_arg_num {
            Some(num) => (if num < 8 { 0 } else { num - 8 }) * 4,
            _ => 0,
        };
        let offset = (s + r + a + 15) / 16 * 16;
        self.stack_offset = Some(offset);
        offset
    }

    /// Get the total offset of the stack frame, if failed, calculate it.
    pub fn get_stack_offset(&mut self) -> usize {
        if self.stack_offset.is_none() {
            self.calculate_stack_offset();
        }
        self.stack_offset.unwrap()
    }

    /// Get the offset of the stack slot of a value to the current %sp.
    pub fn get_value_slot_sp_offset(
        &mut self,
        value_data: &ValueData,
    ) -> Result<Slot, ASMGenError> {
        let tmp = self
            .value_to_stack_slot
            .get(&(value_data as *const ValueData));
        match tmp {
            Some(slot) => {
                let offset = slot.offset;
                let is_ptr = slot.is_ptr;
                let stack_offset = self.get_stack_offset();
                Ok(Slot::new(
                    if self.is_leaf() {
                        stack_offset - self.alloc_total + offset
                    } else {
                        stack_offset - self.alloc_total - 4 + offset
                    },
                    is_ptr.clone(),
                ))
            }
            None => Err(ASMGenError::LocalValueNotFound),
        }
    }
}
