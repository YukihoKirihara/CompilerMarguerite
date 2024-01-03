use super::asm_value::AsmValue;
use super::error::ASMGenError;
use super::function_info::FunctionInfo;
use super::instruction_printer::InstPrinter;
use super::program_info::ProgramInfo;
use super::register::*;
use koopa::ir::entities::{
    BasicBlock, Function, FunctionData, Program, Value, ValueData, ValueKind,
};
use koopa::ir::values::*;
use koopa::ir::{BinaryOp, TypeKind};
use std::fs::File;
use std::io::Write;

pub trait ASMGenerator<'p> {
    type Ret;
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError>;
}

pub trait ASMGenerator4Values<'p> {
    type Ret;
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError>;
}

impl<'p> ASMGenerator<'p> for Program {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        let program = pinfo.ref_program();
        // Iterate over the global values
        // .rodata, .data and .bss are simplified to one single .data
        for &value in self.inst_layout() {
            writeln!(f, "  .data").unwrap();
            let data = self.borrow_value(value);
            let name = &data.name().as_ref().unwrap()[1..];
            pinfo.record_global_value(value, name.to_string());
            writeln!(f, "  .globl {}", name).unwrap();
            writeln!(f, "{}:", name).unwrap();
            data.generate(pinfo, f)?;
        }
        // Iterate over the list of basic blocks
        for &func in program.func_layout() {
            // Visit the function
            let func_data = program.func(func);
            let func_info = FunctionInfo::new(func);
            pinfo.set_curr_func(func_info);
            func_data.generate(pinfo, f)?;
            pinfo.remove_curr_func();
        }
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for Function {
    type Ret = String;
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        _f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        let func_name = &pinfo.ref_program().func(*self).name()[1..];
        Ok(func_name.to_string())
    }
}

impl<'p> ASMGenerator<'p> for FunctionData {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        // SysY library function has no entry basic block
        if self.layout().entry_bb().is_none() {
            return Ok(());
        }
        // Iterate over the local values
        let info = pinfo.mut_ref_curr_func().unwrap();
        for value_data in self.dfg().values().values() {
            // Temporarily allocate all local values on the stack (instead of registers with some algorithm).
            if value_data.kind().is_local_inst() && !value_data.used_by().is_empty() {
                info.allocate_slot_for_value(value_data);
            }
            // Update the maximum number of parameters of all function calls.
            if let ValueKind::Call(func_call) = value_data.kind() {
                info.update_max_args_num(func_call.args().len());
            }
        }
        // Generate Prologue
        writeln!(f, "  .text").unwrap();
        let func_name = &self.name()[1..];
        writeln!(f, "  .globl {}", func_name).unwrap();
        writeln!(f, "{}:", func_name).unwrap();
        // Calculate the total offset of the stack frame
        let offset = info.calculate_stack_offset() as i32;
        let mut printer = InstPrinter::new(f, T0!());
        if offset > 0 {
            // Subtract the stack pointer register
            printer.addi(SP!(), SP!(), -offset);
            if !info.is_leaf() {
                // Store %ra on the top of the stack frame
                printer.sw("ra", SP!(), offset - 4);
            }
        }
        // Iterate over the list of basic blocks
        for (&bblock, bblock_data) in self.dfg().bbs() {
            // Generate the label corresponding to the basic block
            let label_id = pinfo.get_new_label_count();
            let label_name = match bblock_data.name() {
                Some(name) => {
                    format!(".L_{}__{}", &name[1..], label_id)
                }
                None => {
                    format!(".L__{}", label_id)
                }
            };
            // Record it in the function info
            let info = pinfo.mut_ref_curr_func().unwrap();
            info.record_bblock_to_label(bblock, label_name);
        }
        // Go down the the basic blocks
        for (&bblock, node) in self.layout().bbs() {
            // Print the label of current basic block
            let label_name = bblock.generate(pinfo, f).unwrap();
            writeln!(f, "{label_name}:").unwrap();
            // Iterate over the list of instructions
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                // Visit the instruction
                value_data.generate(pinfo, f)?;
            }
        }

        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for BasicBlock {
    type Ret = String;
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        _f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        let info = pinfo.ref_curr_func().unwrap();
        info.get_bblock_label_name(self)
    }
}

impl<'p> ASMGenerator<'p> for ValueData {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        match self.kind() {
            ValueKind::Aggregate(v) => v.generate(pinfo, f),
            ValueKind::Binary(v) => v.generate(pinfo, self, f),
            ValueKind::Branch(v) => v.generate(pinfo, f),
            ValueKind::Call(v) => v.generate(pinfo, self, f),
            ValueKind::GetElemPtr(v) => v.generate(pinfo, self, f),
            ValueKind::GetPtr(v) => v.generate(pinfo, self, f),
            ValueKind::GlobalAlloc(v) => v.generate(pinfo, f),
            ValueKind::Integer(v) => v.generate(pinfo, f),
            ValueKind::Jump(v) => v.generate(pinfo, f),
            ValueKind::Load(v) => v.generate(pinfo, self, f),
            ValueKind::Return(v) => v.generate(pinfo, f),
            ValueKind::Store(v) => v.generate(pinfo, f),
            ValueKind::ZeroInit(v) => v.generate(pinfo, self, f),
            _ => Ok(()),
        }
    }
}

impl<'p> ASMGenerator<'p> for Value {
    type Ret = AsmValue<'p>;
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        _f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        if self.is_global() {
            let value_name = pinfo.get_global_value_name(*self);
            Ok(AsmValue::Global(value_name))
        } else {
            let func = pinfo.ref_curr_func().unwrap().get_function();
            let value_data = pinfo.ref_program().func(func).dfg().value(*self);
            match value_data.kind() {
                ValueKind::Integer(num) => Ok(AsmValue::Const(num.value())),
                ValueKind::FuncArgRef(arg) => Ok(AsmValue::Arg(arg.index())),
                _ => {
                    let info = pinfo.mut_ref_curr_func().unwrap();
                    Ok(AsmValue::Local(
                        info.get_value_slot_sp_offset(value_data).unwrap(),
                    ))
                }
            }
        }
    }
}

impl<'p> ASMGenerator<'p> for Aggregate {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        for &elem in self.elems() {
            pinfo
                .ref_program()
                .borrow_value(elem)
                .generate(pinfo, f)
                .unwrap();
        }
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for Binary {
    type Ret = ();
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        // Generate the left factor and store into %t0
        let lhs = self.lhs().generate(pinfo, f).unwrap();
        lhs.write_register(T0!(), 0, f).unwrap();
        // Generate the right factor and store into %t1
        let rhs = self.rhs().generate(pinfo, f).unwrap();
        rhs.write_register(T1!(), 0, f).unwrap();
        // %t0 op %t1 -> %t0
        let mut printer = InstPrinter::new(f, T2!());
        match self.op() {
            BinaryOp::Add => printer.add(T0!(), T1!(), T0!()),
            BinaryOp::And => printer.and(T0!(), T1!(), T0!()),
            BinaryOp::Div => printer.div(T0!(), T1!(), T0!()),
            BinaryOp::Eq => {
                /*  # %0 = eq 6, 0
                    li    t0, 6
                    xor   t0, t0, x0
                    seqz  t0, t0
                */
                printer.xor(T0!(), T1!(), T0!());
                printer.seqz(T0!(), T0!());
            }
            BinaryOp::Ge => {
                printer.slt(T0!(), T1!(), T0!());
                printer.seqz(T0!(), T0!());
            }
            BinaryOp::Gt => {
                printer.sgt(T0!(), T1!(), T0!());
            }
            BinaryOp::Le => {
                /*  # %0 = le 1 2
                    li    t0, 1
                    li    t1, 2
                    sgt   t1, t0, t1
                    seqz  t1, t1
                */
                printer.sgt(T0!(), T1!(), T0!());
                printer.seqz(T0!(), T0!());
            }
            BinaryOp::Lt => {
                printer.slt(T0!(), T1!(), T0!());
            }
            BinaryOp::Mod => printer.rem(T0!(), T1!(), T0!()),
            BinaryOp::Mul => printer.mul(T0!(), T1!(), T0!()),
            BinaryOp::NotEq => {
                printer.xor(T0!(), T1!(), T0!());
                printer.snez(T0!(), T0!());
            }
            BinaryOp::Or => printer.or(T0!(), T1!(), T0!()),
            BinaryOp::Sar => printer.sra(T0!(), T1!(), T0!()),
            BinaryOp::Shl => printer.sll(T0!(), T1!(), T0!()),
            BinaryOp::Shr => printer.srl(T0!(), T1!(), T0!()),
            BinaryOp::Sub => printer.sub(T0!(), T1!(), T0!()),
            BinaryOp::Xor => printer.xor(T0!(), T1!(), T0!()),
        }
        // Load the value from %t0
        let info = pinfo.mut_ref_curr_func().unwrap();
        let asm_value = AsmValue::Local(info.get_value_slot_sp_offset(v).unwrap());
        asm_value.read_register(T0!(), T1!(), 0, f).unwrap();
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for Branch {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        /*  # %0 = ...
            # br %0, %then, %else
            lw t0, 4(sp)
            bnez t0, then
            j else
        */
        // Generate and store the condition value in %t0
        self.cond()
            .generate(pinfo, f)
            .unwrap()
            .write_register(T0!(), 0, f)
            .unwrap();
        // Generate the label names of true and false basic blocks.
        let true_bb_label_name = self.true_bb().generate(pinfo, f).unwrap();
        let false_bb_label_name = self.false_bb().generate(pinfo, f).unwrap();
        let mut printer = InstPrinter::new(f, T1!());
        printer.bnez(T0!(), &true_bb_label_name);
        printer.j(&false_bb_label_name);
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for Call {
    type Ret = ();
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        // Read out the values of all its parameter variables,
        //      store them in the parameter register or stack frame,
        //      and then generate a call for RISC-V.
        let stack_offset = pinfo.mut_ref_curr_func().unwrap().get_stack_offset() as i32;
        for (i, arg) in self.args().iter().enumerate() {
            let asm_value = arg.generate(pinfo, f).unwrap();
            asm_value.write_register(T0!(), stack_offset, f).unwrap();
            // Note this stack offset is 0 because here is the caller's stack frame.
            AsmValue::Arg(i).read_register(T0!(), T1!(), 0, f).unwrap();
        }
        // Generate the callee function
        let callee_label = self.callee().generate(pinfo, f).unwrap();
        InstPrinter::new(f, T0!()).call(&callee_label);
        // Load the return value from %a0
        if !v.used_by().is_empty() {
            let info = pinfo.mut_ref_curr_func().unwrap();
            let asm_value = AsmValue::Local(info.get_value_slot_sp_offset(v).unwrap());
            asm_value.read_register(A0!(), T1!(), 0, f).unwrap();
        }
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for GetElemPtr {
    type Ret = ();
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        /*  # @arr = alloc [i32, 2]
            # %ptr = getelemptr @arr, 1
            # Get the address of @arr
            addi t0, sp, 4
            # Calculate the offset
            li t1, 1
            li t2, 4
            mul t1, t1, t2
            # Calculate the final address
            add t0, t0, t1
            # Store the result
            ...
        */
        // Generate the address of the source array and store it into %t0
        let stack_offset = pinfo.mut_ref_curr_func().unwrap().get_stack_offset() as i32;
        let src = self.src().generate(pinfo, f).unwrap();
        if src.is_ptr() {
            src.write_register(T0!(), 0, f).unwrap();
        } else {
            src.write_address_to_register(T0!(), stack_offset, f)
                .unwrap();
        }
        // Calculate the offset and store it into %t1
        let index = self.index().generate(pinfo, f).unwrap();
        index.write_register(T1!(), stack_offset, f).unwrap();
        // Get the size of the array element
        let size = match v.ty().kind() {
            TypeKind::Pointer(base) => base.size(),
            _ => 0,
        };
        let mut printer = InstPrinter::new(f, T2!());
        printer.muli(T1!(), T1!(), size as i32);
        // Calculate the final address
        printer.add(T0!(), T1!(), T0!());
        // Store the result
        let info = pinfo.mut_ref_curr_func().unwrap();
        let asm_value = AsmValue::Local(info.get_value_slot_sp_offset(v).unwrap());
        asm_value.read_register(T0!(), T1!(), 0, f).unwrap();
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for GetPtr {
    type Ret = ();
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        // Identical to GetElemPtr
        // Generate the address of the source array and store it into %t0
        let stack_offset = pinfo.mut_ref_curr_func().unwrap().get_stack_offset() as i32;
        let src = self.src().generate(pinfo, f).unwrap();
        if src.is_ptr() {
            src.write_register(T0!(), 0, f).unwrap();
        } else {
            src.write_address_to_register(T0!(), stack_offset, f)
                .unwrap();
        }
        // Calculate the offset and store it into %t1
        let index = self.index().generate(pinfo, f).unwrap();
        index.write_register(T1!(), stack_offset, f).unwrap();
        // Get the size of the array element
        let size = match v.ty().kind() {
            TypeKind::Pointer(base) => base.size(),
            _ => 0,
        };
        let mut printer = InstPrinter::new(f, T2!());
        printer.muli(T1!(), T1!(), size as i32);
        // Calculate the final address
        printer.add(T0!(), T1!(), T0!());
        // Store the result
        let info = pinfo.mut_ref_curr_func().unwrap();
        let asm_value = AsmValue::Local(info.get_value_slot_sp_offset(v).unwrap());
        asm_value.read_register(T0!(), T1!(), 0, f).unwrap();
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for GlobalAlloc {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        pinfo
            .ref_program()
            .borrow_value(self.init())
            .generate(pinfo, f)
    }
}

impl<'p> ASMGenerator<'p> for Integer {
    type Ret = ();
    fn generate(
        &self,
        _pinfo: &'p mut ProgramInfo,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        writeln!(f, "   .word {}", self.value()).unwrap();
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for Jump {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        let label_name = self.target().generate(pinfo, f).unwrap();
        InstPrinter::new(f, T0!()).j(label_name.as_str());
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for Load {
    type Ret = ();
    fn generate(
        &self,
        pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        // Generate the source value and put it into %t0
        let src = self.src().generate(pinfo, f).unwrap();
        src.write_register(T0!(), 0, f).unwrap();
        // If the value is a pointer, load its content
        if src.is_ptr() {
            InstPrinter::new(f, T1!()).lw(T0!(), T0!(), 0);
        }
        // Load the value from %t0
        let info = pinfo.mut_ref_curr_func().unwrap();
        let asm_value = AsmValue::Local(info.get_value_slot_sp_offset(v).unwrap());
        asm_value.read_register(T0!(), T1!(), 0, f).unwrap();
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for Return {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        // Put the return value to %a0
        if let Some(ret_val) = self.value() {
            ret_val
                .generate(pinfo, f)
                .unwrap()
                .write_register(A0!(), 0, f)
                .unwrap();
        }
        // Generate Epilogue
        let info = pinfo.mut_ref_curr_func().unwrap();
        let offset = info.get_stack_offset() as i32;
        let mut printer = InstPrinter::new(f, T0!());
        if offset > 0 {
            if !info.is_leaf() {
                // Restore %ra from the stack frame
                printer.lw(SP!(), "ra", offset - 4);
            }
            // Restore the stack pointer register
            printer.addi(SP!(), SP!(), offset);
        }
        printer.ret();
        Ok(())
    }
}

impl<'p> ASMGenerator<'p> for Store {
    type Ret = ();
    fn generate(&self, pinfo: &'p mut ProgramInfo, f: &mut File) -> Result<Self::Ret, ASMGenError> {
        // Generate the value and put it into %t0
        let info = pinfo.mut_ref_curr_func().unwrap();
        let stack_offset = info.get_stack_offset() as i32;
        let value = self.value().generate(pinfo, f).unwrap();
        value.write_register(T0!(), stack_offset, f).unwrap();
        let dest = self.dest().generate(pinfo, f).unwrap();
        if dest.is_ptr() {
            // Store the addresss to %t1
            dest.write_register(T1!(), 0, f).unwrap();
            // Store the value in %t0 to the address in %t1
            InstPrinter::new(f, T2!()).sw(T0!(), T1!(), 0);
        } else {
            dest.read_register(T0!(), T1!(), 0, f).unwrap();
        }
        Ok(())
    }
}

impl<'p> ASMGenerator4Values<'p> for ZeroInit {
    type Ret = ();
    fn generate(
        &self,
        _pinfo: &'p mut ProgramInfo,
        v: &ValueData,
        f: &mut File,
    ) -> Result<Self::Ret, ASMGenError> {
        writeln!(f, "  .zero {}", v.ty().size()).unwrap();
        Ok(())
    }
}
