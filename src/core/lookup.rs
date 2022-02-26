//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

use crate::core::{Operand, CPU6502};

/// Information about a given opcode
pub(crate) struct Info {
    /// A human-readable name of the opcode
    pub(crate) name: &'static str,
    /// The instruction executor method on the CPU that handles this opcode
    pub(crate) executor: fn(&mut CPU6502, Operand) -> bool,
    /// The operand method on the CPU that correctly reads the instruction's operand(s)
    pub(crate) operands: fn(&mut CPU6502) -> Operand,
    /// The number of cycles this instruction is supposed to take
    pub(crate) cycles: u16,
}

macro_rules! inst {
    ($method:ident) => {{
        fn inner(this: &mut CPU6502<'_>, operand: Operand) -> bool {
            this.$method(operand)
        }

        inner
    }};
}

macro_rules! oper_wrapper {
    ($operand:ident) => {
        pub(in crate::core) const $operand: fn(&mut CPU6502) -> Operand = |this| this.$operand();
    };
}

// I can't directly use the methods from `CPU6502` due to lifetime parameter
// issues, so I need a wrapper of some sort. I could also accomplish this with
// a macro that just wraps a lambda or anonymous function, but being able to refer
// to these functions by name helps other code in the project.
//
// Why Rust prevents me from just using the methods directly, I barely understand. I'm
// not happy about it (since it then forces a function that does nothing but immediately `jmp`
// to another one that can't ever be inlined due to the fact we're only doing indirect
// jumps to these functions), but whatever.

oper_wrapper!(abs);
oper_wrapper!(abx);
oper_wrapper!(aby);
oper_wrapper!(imm);
oper_wrapper!(imp);
oper_wrapper!(ind);
oper_wrapper!(izx);
oper_wrapper!(izy);
oper_wrapper!(rel);
oper_wrapper!(zpo);
oper_wrapper!(zpx);
oper_wrapper!(zpy);

macro_rules! info {
    ($name:expr, $inst:expr, $oper:expr, $cycles:expr) => {{
        Info {
            name: $name,
            executor: $inst,
            operands: $oper,
            cycles: $cycles,
        }
    }};
}

/// Provides a lookup table for all instructions that the emulator supports. This table is
/// how instruction/operand handlers are dispatched, and how the emulator gets cycle information
/// for a given instruction. 
#[rustfmt::skip]
const INSTR_LOOKUP: [Info; 256] = [
    //        x0                               x1                                x2                                x3                                x4                                x5                                x6                                x7                                x8                                x9                                xA                                xB                                xC                                xD                                xE                                xF
    /* 0x */ info!("brk", inst!(brk), imp, 7), info!("ora", inst!(ora), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("ora", inst!(ora), zpo, 3), info!("asl", inst!(asl), zpo, 5), info!("???", inst!(ill), imp, 1), info!("php", inst!(php), imp, 3), info!("ora", inst!(ora), imm, 2), info!("asl", inst!(asl), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("ora", inst!(ora), abs, 4), info!("asl", inst!(asl), abs, 6), info!("???", inst!(ill), imp, 1),
    /* 1x */ info!("bpl", inst!(bpl), rel, 2), info!("ora", inst!(ora), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("ora", inst!(ora), zpx, 4), info!("asl", inst!(asl), zpx, 6), info!("???", inst!(ill), imp, 1), info!("clc", inst!(clc), imp, 2), info!("ora", inst!(ora), aby, 4), info!("???", inst!(ill), imp, 2), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("ora", inst!(ora), abx, 4), info!("asl", inst!(asl), abx, 7), info!("???", inst!(ill), imp, 1),
    /* 2x */ info!("jsr", inst!(jsr), abs, 6), info!("and", inst!(and), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("bit", inst!(bit), zpo, 3), info!("and", inst!(and), zpo, 3), info!("rol", inst!(rol), zpo, 5), info!("???", inst!(ill), imp, 1), info!("plp", inst!(plp), imp, 4), info!("and", inst!(and), imm, 2), info!("rol", inst!(rol), imp, 2), info!("???", inst!(ill), imp, 1), info!("bit", inst!(bit), abs, 4), info!("and", inst!(and), abs, 4), info!("rol", inst!(rol), abs, 6), info!("???", inst!(ill), imp, 1),
    /* 3x */ info!("bmi", inst!(bmi), rel, 2), info!("and", inst!(and), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("and", inst!(and), zpx, 4), info!("rol", inst!(rol), zpx, 6), info!("???", inst!(ill), imp, 1), info!("sec", inst!(sec), imp, 2), info!("and", inst!(and), aby, 4), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("and", inst!(and), abx, 4), info!("rol", inst!(rol), abx, 7), info!("???", inst!(ill), imp, 1),
    /* 4x */ info!("rti", inst!(rti), imp, 6), info!("eor", inst!(eor), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("eor", inst!(eor), zpo, 3), info!("lsr", inst!(lsr), zpo, 5), info!("???", inst!(ill), imp, 1), info!("pha", inst!(pha), imp, 3), info!("eor", inst!(eor), imm, 2), info!("lsr", inst!(lsr), imp, 2), info!("???", inst!(ill), imp, 1), info!("jmp", inst!(jmp), abs, 3), info!("eor", inst!(eor), abs, 4), info!("lsr", inst!(lsr), abs, 6), info!("???", inst!(ill), imp, 1),
    /* 5x */ info!("bvc", inst!(bvc), rel, 2), info!("eor", inst!(eor), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("eor", inst!(eor), zpx, 4), info!("lsr", inst!(lsr), zpx, 6), info!("???", inst!(ill), imp, 1), info!("cli", inst!(cli), imp, 2), info!("eor", inst!(eor), aby, 4), info!("???", inst!(ill), imp, 2), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 3), info!("eor", inst!(eor), abx, 4), info!("lsr", inst!(lsr), abx, 7), info!("???", inst!(ill), imp, 1),
    /* 6x */ info!("rts", inst!(rts), imp, 6), info!("adc", inst!(adc), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("adc", inst!(adc), zpo, 3), info!("ror", inst!(ror), zpo, 5), info!("???", inst!(ill), imp, 1), info!("pla", inst!(pla), imp, 4), info!("adc", inst!(adc), imm, 2), info!("ror", inst!(ror), imp, 2), info!("???", inst!(ill), imp, 1), info!("jmp", inst!(jmp), ind, 5), info!("adc", inst!(adc), abs, 4), info!("ror", inst!(ror), abs, 6), info!("???", inst!(ill), imp, 1),
    /* 7x */ info!("bvs", inst!(bvs), rel, 2), info!("adc", inst!(adc), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("adc", inst!(adc), zpx, 4), info!("ror", inst!(ror), zpx, 6), info!("???", inst!(ill), imp, 1), info!("sei", inst!(sei), imp, 2), info!("adc", inst!(adc), aby, 4), info!("???", inst!(ror), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("adc", inst!(adc), abx, 4), info!("ror", inst!(ror), abx, 7), info!("???", inst!(ill), imp, 1),
    /* 8x */ info!("???", inst!(ill), imp, 1), info!("sta", inst!(sta), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("sty", inst!(sty), zpo, 3), info!("sta", inst!(sta), zpo, 3), info!("stx", inst!(stx), zpo, 3), info!("???", inst!(ill), imp, 1), info!("dey", inst!(dey), imp, 2), info!("???", inst!(ill), imp, 1), info!("txa", inst!(txa), imp, 2), info!("???", inst!(ill), imp, 1), info!("sty", inst!(sty), abs, 4), info!("sta", inst!(sta), abs, 4), info!("stx", inst!(stx), abs, 4), info!("???", inst!(ill), imp, 1),
    /* 9x */ info!("bcc", inst!(bcc), rel, 2), info!("sta", inst!(sta), izy, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("sty", inst!(sty), zpx, 4), info!("sta", inst!(sta), zpx, 4), info!("stx", inst!(stx), zpy, 4), info!("???", inst!(ill), imp, 1), info!("tya", inst!(tya), imp, 2), info!("sta", inst!(sta), aby, 5), info!("txs", inst!(txa), imp, 2), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 4), info!("sta", inst!(sta), abx, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1),
    /* Ax */ info!("ldy", inst!(ldy), imm, 2), info!("lda", inst!(lda), izx, 6), info!("ldx", inst!(ldx), imm, 2), info!("???", inst!(ill), imp, 1), info!("ldy", inst!(ldy), zpo, 3), info!("lda", inst!(lda), zpo, 3), info!("ldx", inst!(ldx), zpo, 3), info!("???", inst!(ill), imp, 1), info!("tay", inst!(tay), imp, 2), info!("lda", inst!(lda), imm, 2), info!("tax", inst!(tax), imp, 2), info!("???", inst!(ill), imp, 1), info!("ldy", inst!(ldy), abs, 4), info!("lda", inst!(lda), abs, 4), info!("ldx", inst!(ldx), abs, 4), info!("???", inst!(ill), imp, 1),
    /* Bx */ info!("bcs", inst!(bcs), rel, 2), info!("lda", inst!(lda), izy, 5), info!("???", inst!(ill), imm, 2), info!("???", inst!(ill), imp, 1), info!("ldy", inst!(ldy), zpx, 4), info!("lda", inst!(lda), zpx, 4), info!("ldx", inst!(ldx), zpy, 4), info!("???", inst!(ill), imp, 1), info!("clv", inst!(clv), imp, 2), info!("lda", inst!(lda), aby, 4), info!("tsx", inst!(tsx), imp, 2), info!("???", inst!(ill), imp, 1), info!("ldy", inst!(ldy), abx, 4), info!("lda", inst!(lda), abx, 4), info!("ldx", inst!(ldx), aby, 4), info!("???", inst!(ill), imp, 1),
    /* Cx */ info!("cpy", inst!(cpy), imm, 2), info!("cmp", inst!(cmp), izx, 6), info!("???", inst!(ill), imm, 1), info!("???", inst!(ill), imp, 1), info!("cpy", inst!(cpy), zpo, 3), info!("cmp", inst!(cmp), zpo, 3), info!("dec", inst!(dec), zpo, 5), info!("???", inst!(ill), imp, 1), info!("iny", inst!(iny), imp, 2), info!("cmp", inst!(cmp), imm, 2), info!("dex", inst!(dex), imp, 2), info!("???", inst!(ill), imp, 1), info!("cpy", inst!(cpy), abs, 4), info!("cmp", inst!(cmp), abs, 4), info!("dec", inst!(dec), abs, 6), info!("???", inst!(ill), imp, 1),
    /* Dx */ info!("bne", inst!(bne), rel, 2), info!("cmp", inst!(cmp), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("cmp", inst!(cmp), zpx, 4), info!("dec", inst!(dec), zpx, 6), info!("???", inst!(ill), imp, 1), info!("cld", inst!(cld), imp, 2), info!("cmp", inst!(cmp), aby, 4), info!("dex", inst!(dex), imp, 2), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 4), info!("cmp", inst!(cmp), abx, 4), info!("dec", inst!(dec), abx, 7), info!("???", inst!(ill), imp, 1),
    /* Ex */ info!("cpx", inst!(cpx), imm, 2), info!("sbc", inst!(sbc), izx, 6), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("cpx", inst!(cpx), zpo, 3), info!("sbc", inst!(sbc), zpo, 3), info!("inc", inst!(inc), zpo, 5), info!("???", inst!(ill), imp, 1), info!("inx", inst!(inx), imp, 2), info!("sbc", inst!(sbc), imm, 2), info!("nop", inst!(nop), imp, 2), info!("???", inst!(ill), imp, 1), info!("cpx", inst!(cpx), abs, 4), info!("sbc", inst!(sbc), abs, 4), info!("inc", inst!(inc), abs, 6), info!("???", inst!(ill), imp, 1),
    /* Fx */ info!("beq", inst!(beq), rel, 2), info!("sbc", inst!(sbc), izy, 5), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("sbc", inst!(sbc), zpx, 4), info!("inc", inst!(inc), zpx, 6), info!("???", inst!(ill), imp, 1), info!("sed", inst!(sed), imp, 2), info!("sbc", inst!(sbc), aby, 4), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("???", inst!(ill), imp, 1), info!("sbc", inst!(sbc), abx, 4), info!("inc", inst!(inc), abx, 7), info!("???", inst!(ill), imp, 1),
];

/// Performs a lookup into the instruction table based on the opcode `opcode`,
/// and returns the instruction information for that opcode.
pub(in crate::core) fn lookup_instruction(opcode: u8) -> &'static Info {
    &INSTR_LOOKUP[opcode as usize]
}
