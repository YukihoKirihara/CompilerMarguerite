use std::fs::File;
use std::io::Write;

const IMM12_MIN: i32 = -2048;
const IMM12_MAX: i32 = 2047;

/// Instruction Printer for RISC-V language.
/// Some instructions take several lines to generate, as it benefits to summarize them to the struct.
pub struct InstPrinter<'f> {
    /// The file to write into
    f: &'f mut File,
    /// The name of a temporary register required by some certain instructions
    tmp: &'static str,
}

impl<'f> InstPrinter<'f> {
    /// Create a new Instruction Printer
    pub fn new(f: &'f mut File, reg: &'static str) -> Self {
        Self { f: f, tmp: reg }
    }

    /// Print the beqz instruction
    /// beqz rs, label  : if %rs == 0, jump to .label.
    pub fn beqz(&mut self, cond: &str, label: &str) {
        writeln!(self.f, "  beqz {}, {}", cond, label).unwrap();
    }

    /// Print the bnez instruction
    /// bnez rs, label  : if %rs != 0, jump to .label.
    pub fn bnez(&mut self, cond: &str, label: &str) {
        writeln!(self.f, "  bnez {}, {}", cond, label).unwrap();
    }

    /// Print the j instruction
    /// j label     : jump to .label.
    pub fn j(&mut self, label: &str) {
        writeln!(self.f, "  j {}", label).unwrap();
    }

    /// Print the call instruction
    /// call label      : store the address of the next instruction into %ra, and jump to label.
    pub fn call(&mut self, label: &str) {
        writeln!(self.f, "  call {}", label).unwrap();
    }

    /// Print the ret instruction
    /// ret     : jump to the address in %ra.
    pub fn ret(&mut self) {
        writeln!(self.f, "  ret").unwrap();
    }

    /// Print the lw instruction
    /// lw rd, imm(rs)    : add up %rs and imm to the address, read the 32-bit data at that address, and store into %rd.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn lw(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  lw {}, {}({})", dst, imm, src).unwrap();
        } else {
            self.addi(src, self.tmp, imm);
            writeln!(self.f, "  lw {}, {}({})", dst, 0, self.tmp).unwrap();
        }
    }

    /// Print the sw instruction
    /// sw rs, imm(rd)  : add up %rd and imm to the address, and store %rs to that address.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn sw(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  sw {}, {}({})", src, imm, dst).unwrap();
        } else {
            self.addi(dst, self.tmp, imm);
            writeln!(self.f, "  sw {}, {}({})", src, 0, self.tmp).unwrap();
        }
    }

    /// Print the add instruction
    /// add rd, rs1, rs2    : add up %rs1 and %rs2, and store the result to %rd.
    pub fn add(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  add {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the addi instruction
    /// addi rd, rs, imm    : add up %rs and imm, and store the result to %rd.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn addi(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  addi {}, {}, {}", dst, src, imm).unwrap();
        } else {
            self.li(self.tmp, imm);
            writeln!(self.f, "  add {}, {}, {}", dst, src, self.tmp).unwrap();
        }
    }

    /// Print the sub instruction
    /// sub rd, rs1, rs2   : subtract %rs1 by %rs2, and store the result to %rd.
    pub fn sub(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  sub {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the slt instruction
    /// slt rd, rs1, rs2   : if %rs1 < %rs2, store 1 to %rd, else 0.
    pub fn slt(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  slt {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the sgt instruction
    /// sgt rd, rs1, rs2   : if %rs1 > %rs2, store 1 to %rd, else 0.
    pub fn sgt(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  sgt {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the seqz instruction
    /// seqz rd, rs     : if %rs == 0, store 1 to %rd, else 0.
    pub fn seqz(&mut self, src: &str, dst: &str) {
        writeln!(self.f, "  seqz {}, {}", dst, src).unwrap();
    }

    /// Print the snez instruction
    /// snez rd, rs     : if %rs != 0, store 1 to %rd, else 0.
    pub fn snez(&mut self, src: &str, dst: &str) {
        writeln!(self.f, "  snez {}, {}", dst, src).unwrap();
    }

    /// Print the xor instruction
    /// xor rd, rs1, rs2   : calculate %rs1 xor %rs2, and store the result to %rd.
    pub fn xor(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  xor {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the xori instruction
    /// xori rd, rs, imm    : calculate %rs xor imm, and store the result to %rd.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn xori(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  xori {}, {}, {}", dst, src, imm).unwrap();
        } else {
            self.li(self.tmp, imm);
            writeln!(self.f, "  xor {}, {}, {}", dst, src, self.tmp).unwrap();
        }
    }

    /// Print the or instruction
    /// or rd, rs1, rs2   : calculate %rs1 or %rs2, and store the result to %rd.
    pub fn or(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  or {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the ori instruction
    /// ori rd, rs, imm    : calculate %rs or imm, and store the result to %rd.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn ori(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  ori {}, {}, {}", dst, src, imm).unwrap();
        } else {
            self.li(self.tmp, imm);
            writeln!(self.f, "  or {}, {}, {}", dst, src, self.tmp).unwrap();
        }
    }

    /// Print the and instruction
    /// and rd, rs1, rs2   : calculate %rs1 and %rs2, and store the result to %rd.
    pub fn and(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  and {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the andi instruction
    /// andi rd, rs, imm    : calculate %rs and imm, and store the result to %rd.
    /// If the length of imm is greater than 12 bits, the instruction will be divided into 2 steps.
    pub fn andi(&mut self, src: &str, dst: &str, imm: i32) {
        if imm >= IMM12_MIN && imm <= IMM12_MAX {
            writeln!(self.f, "  andi {}, {}, {}", dst, src, imm).unwrap();
        } else {
            self.li(self.tmp, imm);
            writeln!(self.f, "  and {}, {}, {}", dst, src, self.tmp).unwrap();
        }
    }

    /// Print the sll instruction
    /// sll rd, rs1, rs2   : shift left logically %rs1 by %rs2 bits, and store the result to %rd.
    pub fn sll(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  sll {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the srl instruction
    /// srl rd, rs1, rs2   : shift right logically %rs1 by %rs2 bits, and store the result to %rd.
    pub fn srl(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  srl {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the sra instruction
    /// sra rd, rs1, rs2   : shift right arithmetically %rs1 by %rs2 bits, and store the result to %rd.
    pub fn sra(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  sra {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the mul instruction
    /// mul rd, rs1, rs2    : multiply %rs1 and %rs2, and store the result to %rd.
    pub fn mul(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  mul {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the div instruction
    /// div rd, rs1, rs2    : divide %rs1 by %rs2, and store the result to %rd.
    pub fn div(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  div {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the rem instruction
    /// rem rd, rs1, rs2    : calculate the remain of %rs1 divided by %rs2, and store the result to %rd.
    pub fn rem(&mut self, src1: &str, src2: &str, dst: &str) {
        writeln!(self.f, "  rem {}, {}, {}", dst, src1, src2).unwrap();
    }

    /// Print the li instruction
    /// li rd, imm  : Load imm into %rd.
    pub fn li(&mut self, dst: &str, imm: i32) {
        writeln!(self.f, "  li {}, {}", dst, imm).unwrap();
    }

    /// Print the la instruction
    /// la rd, label    : Load the abstract address of .label into %rd.
    pub fn la(&mut self, dst: &str, label: &str) {
        writeln!(self.f, "  la {}, {}", dst, label).unwrap();
    }

    /// Print the mv instruction
    /// mv rd, rs  : Copy %rs to %rd.
    pub fn mv(&mut self, src: &str, dst: &str) {
        writeln!(self.f, "  mv {}, {}", dst, src).unwrap();
    }
}
