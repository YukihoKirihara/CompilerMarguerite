use std::iter::repeat_with;

use super::error::IRGenError;
use super::scope_manager::ScopeManager;
use koopa::ir::builder_traits::*;
use koopa::ir::{Program, Type, TypeKind, Value};

pub enum ArrayType {
    Array(Vec<ArrayType>),
    BaseValue(Value),
    BaseConst(i32),
}

impl ArrayType {
    /// Return the i32 in a base constant.
    pub fn get_const_i32(&self) -> Result<i32, IRGenError> {
        match self {
            Self::BaseConst(num) => Ok(num.clone()),
            _ => Err(IRGenError::UnKnown),
        }
    }

    /// Reshape the initial value from irregular tree-like to linear, and fill in the blanks with 0.
    /// eg. a[2][4] = {1, 2, 3, 4, {5}} -> {{1, 2, 3, 4}, {5, 0, 0, 0}}.
    pub fn reshape(self, mut _type: &Type) -> Result<Self, IRGenError> {
        match self {
            Self::Array(arr) => {
                // Generate the shapes of each dimension.
                // Vec<(the number of columns in reverse order, the total of leaf elements in the subtree)>
                // eg. a[2][4] -> [(4, 4), (2, 8)]
                let mut rev_dims: Vec<usize> = Vec::new();
                let mut shapes: Vec<(usize, usize)> = Vec::new();
                let mut total: usize = 1;
                loop {
                    match _type.kind() {
                        TypeKind::Int32 => break,
                        TypeKind::Array(sub_type, column_num) => {
                            rev_dims.push(*column_num);
                            _type = sub_type;
                        }
                        _ => unreachable!(),
                    }
                }
                for d in rev_dims.iter().rev() {
                    total *= d;
                    shapes.push((*d, total));
                }
                Self::reshape_array(arr, &shapes)
            }
            ret_val @ (Self::BaseConst(_) | Self::BaseValue(_)) => Ok(ret_val),
        }
    }

    /// Reshape an array by DFS.
    /// Reshape the initial value from irregular tree-like to linear, and fill in the blanks with 0.
    /// eg. a[2][4] = {1, 2, 3, 4, {5}} -> {{1, 2, 3, 4}, {5, 0, 0, 0}}.
    pub fn reshape_array(arr: Vec<Self>, shapes: &[(usize, usize)]) -> Result<Self, IRGenError> {
        // A place to store the processed initial value of different dimensions from lower to higher.
        // eg. a[2][4] = {1, 2, 3, 4, {5}}, shapes = [(4, 4), (2, 8)].
        // When {1, 2, 3, 4} are processed and stored in reshaped_arr[0]: reshaped_arr = [[1, 2, 3, 4], [], []],
        //      they will get wrapped to the higher dimension: reshaped_arr = [[], [[1, 2, 3, 4]], []].
        // And then after a recursion: reshaped_arr = [[], [[1, 2, 3, 4], [5, 0, 0, 0]], []].
        // Finally: reshaped_arr = [[], [], [[[1, 2, 3, 4], [5, 0, 0, 0]]]].
        let mut reshaped_arr: Vec<Vec<Self>> =
            repeat_with(Vec::new).take(shapes.len() + 1).collect();
        // A counter of leaf elements in the current sub-tree.
        let mut elem_cnt: usize = 0;
        for item in arr {
            match item {
                Self::Array(sub_arr) => {
                    // Get the slice of shapes for a recursion.
                    let sub_shapes = match reshaped_arr.iter().position(|v| !v.is_empty()) {
                        Some(i) => &shapes[..i],
                        None => &shapes[..shapes.len() - 1],
                    };
                    // Recursion
                    reshaped_arr[sub_shapes.len()]
                        .push(Self::reshape_array(sub_arr, sub_shapes).unwrap());
                    // If the number of elements is aligned to the shape of the subtree, push the slice to the next dimension.
                    for (dim, &(column_num, _)) in shapes.iter().enumerate() {
                        if reshaped_arr[dim].len() == column_num {
                            let tmp = Self::Array(reshaped_arr[dim].drain(..).collect());
                            reshaped_arr[dim + 1].push(tmp);
                        }
                    }
                    elem_cnt += sub_shapes.last().unwrap().1;
                }
                _ => {
                    // Base elements are pushed to the lowest dimension.
                    reshaped_arr[0].push(item);
                    // If the number of elements is aligned to the shape of the subtree, push the slice to the next dimension.
                    for (dim, &(column_num, _)) in shapes.iter().enumerate() {
                        if reshaped_arr[dim].len() == column_num {
                            let tmp = Self::Array(reshaped_arr[dim].drain(..).collect());
                            reshaped_arr[dim + 1].push(tmp);
                        }
                    }
                    elem_cnt += 1;
                }
            }
        }
        // Fill in the blanks with zeros.
        while elem_cnt < shapes.last().unwrap().1 {
            reshaped_arr[0].push(Self::BaseConst(0));
            // If the number of elements is aligned to the shape of the subtree, push the slice to the next dimension.
            for (dim, &(column_num, _)) in shapes.iter().enumerate() {
                if reshaped_arr[dim].len() == column_num {
                    let tmp = Self::Array(reshaped_arr[dim].drain(..).collect());
                    reshaped_arr[dim + 1].push(tmp);
                }
            }
            elem_cnt += 1;
        }
        // Return with the highest dimension
        // eg. reshaped_arr = [[], [], [[[1, 2, 3, 4], [5, 0, 0, 0]]]] -> [[1, 2, 3, 4], [5, 0, 0, 0]]
        Ok(reshaped_arr.pop().unwrap().pop().unwrap())
    }

    /// Convert the reshaped ArrayType to a constant Value
    pub fn get_const_value(
        self,
        program: &mut Program,
        scopes: &ScopeManager,
    ) -> Result<Value, IRGenError> {
        match self {
            Self::Array(arr) => {
                let value = arr
                    .into_iter()
                    .map(|i| i.get_const_value(program, scopes))
                    .collect::<Result<_, _>>()
                    .unwrap();
                let aggregate = if scopes.is_global() {
                    program.new_value().aggregate(value)
                } else {
                    let info = scopes.ref_curr_func().unwrap();
                    info.create_new_value(program).aggregate(value)
                };
                Ok(aggregate)
            }
            Self::BaseConst(num) => {
                let value = if scopes.is_global() {
                    program.new_value().integer(num)
                } else {
                    let info = scopes.ref_curr_func().unwrap();
                    info.create_new_value(program).integer(num)
                };
                Ok(value)
            }
            Self::BaseValue(_) => Err(IRGenError::NotAConstant("".to_string())),
        }
    }

    /// Convert the reshaped ArrayType to a store instruction
    pub fn push_store_inst(
        self,
        program: &mut Program,
        scopes: &ScopeManager,
        ptr: Value,
    ) -> Result<(), IRGenError> {
        let info = scopes.ref_curr_func().unwrap();
        match self {
            Self::Array(arr) => {
                for (i, item) in arr.into_iter().enumerate() {
                    let index = info.create_new_value(program).integer(i as i32);
                    let elem_ptr = info.create_new_value(program).get_elem_ptr(ptr, index);
                    info.push_inst_curr_bblock(program, elem_ptr);
                    // Recursion
                    item.push_store_inst(program, scopes, elem_ptr)?;
                }
            }
            Self::BaseConst(num) => {
                let value = info.create_new_value(program).integer(num);
                let store = info.create_new_value(program).store(value, ptr);
                info.push_inst_curr_bblock(program, store);
            }
            Self::BaseValue(value) => {
                let store = info.create_new_value(program).store(value, ptr);
                info.push_inst_curr_bblock(program, store);
            }
        }
        Ok(())
    }
}
