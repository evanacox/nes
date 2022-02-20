//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

use crate::core::CPU6502;

pub(crate) struct Info {
    pub(crate) name: &'static str,
    pub(crate) executor: fn(&mut CPU6502),
    pub(crate) operands: fn(&mut CPU6502) -> u8,
    pub(crate) cycles: u16,
}

macro_rules! inst {
    ($method:ident) => {
        {
            fn inner(this: &mut CPU6502<'_>) {
                this.$method()
            }
            
            inner
        }
    }
}

macro_rules! oper {
    ($operand:ident) => {
        {
            fn inner(this: &mut CPU6502<'_>) -> u8 {
                this.$operand()
            }
            
            inner
        }
    }
}

macro_rules! info {
    ($name:expr, $inst:expr, $oper:expr, $cycles:expr) => {
        {
            Info {
                name: $name,
                executor: $inst,
                operands: $oper,
                cycles: $cycles,
            }
        }
    }
}

#[rustfmt::skip]
pub(crate) const INSTR_LOOKUP: [Info; 256] = [
    //        x0                                      x1                                       x2                                       x3                                       x4                                       x5                                       x6                                       x7                                       x8                                       x9                                       xA                                       xB                                       xC                                       xD                                       xE                                       xF
    /* 0x */ info!("brk", inst!(brk), oper!(imp), 7), info!("ora", inst!(ora), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("ora", inst!(ora), oper!(zpo), 3), info!("asl", inst!(asl), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("php", inst!(php), oper!(imp), 3), info!("ora", inst!(ora), oper!(imm), 2), info!("asl", inst!(asl), oper!(imm), 2), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("ora", inst!(ora), oper!(abs), 4), info!("asl", inst!(asl), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* 1x */ info!("bpl", inst!(bpl), oper!(rel), 2), info!("ora", inst!(ora), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("ora", inst!(ora), oper!(zpx), 4), info!("asl", inst!(asl), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("clc", inst!(clc), oper!(imp), 2), info!("ora", inst!(ora), oper!(aby), 4), info!("???", inst!(ill), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("ora", inst!(ora), oper!(abx), 4), info!("asl", inst!(asl), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
    /* 2x */ info!("jsr", inst!(jsr), oper!(abs), 6), info!("and", inst!(and), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("bit", inst!(bit), oper!(zpo), 3), info!("and", inst!(and), oper!(zpo), 3), info!("rol", inst!(rol), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("plp", inst!(plp), oper!(imp), 4), info!("and", inst!(and), oper!(imm), 2), info!("rol", inst!(rol), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("bit", inst!(bit), oper!(abs), 4), info!("and", inst!(and), oper!(abs), 4), info!("rol", inst!(rol), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* 3x */ info!("bmi", inst!(bmi), oper!(rel), 2), info!("and", inst!(and), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("and", inst!(and), oper!(zpx), 4), info!("rol", inst!(rol), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("sec", inst!(sec), oper!(imp), 2), info!("and", inst!(and), oper!(aby), 4), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("and", inst!(and), oper!(abx), 4), info!("rol", inst!(rol), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
    /* 4x */ info!("rti", inst!(rti), oper!(imp), 6), info!("eor", inst!(eor), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("eor", inst!(eor), oper!(zpo), 3), info!("lsr", inst!(lsr), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("pha", inst!(pha), oper!(imp), 3), info!("eor", inst!(eor), oper!(imm), 2), info!("lsr", inst!(lsr), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("jmp", inst!(jmp), oper!(abs), 3), info!("eor", inst!(eor), oper!(abs), 4), info!("lsr", inst!(lsr), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* 5x */ info!("bvc", inst!(bvc), oper!(rel), 2), info!("eor", inst!(eor), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("eor", inst!(eor), oper!(zpx), 4), info!("lsr", inst!(lsr), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("cli", inst!(cli), oper!(imp), 2), info!("eor", inst!(eor), oper!(aby), 4), info!("???", inst!(ill), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 3), info!("eor", inst!(eor), oper!(abx), 4), info!("lsr", inst!(lsr), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
    /* 6x */ info!("rts", inst!(rts), oper!(imp), 6), info!("adc", inst!(adc), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("adc", inst!(adc), oper!(zpo), 3), info!("ror", inst!(ror), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("pla", inst!(pla), oper!(imp), 4), info!("adc", inst!(adc), oper!(imm), 2), info!("ror", inst!(ror), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("jmp", inst!(jmp), oper!(ind), 5), info!("adc", inst!(adc), oper!(abs), 4), info!("ror", inst!(ror), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* 7x */ info!("bvs", inst!(bvs), oper!(rel), 2), info!("adc", inst!(adc), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("adc", inst!(adc), oper!(zpx), 4), info!("ror", inst!(ror), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("sei", inst!(sei), oper!(imp), 2), info!("adc", inst!(adc), oper!(aby), 4), info!("???", inst!(ror), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("adc", inst!(adc), oper!(abx), 4), info!("ror", inst!(ror), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
    /* 8x */ info!("???", inst!(ill), oper!(imp), 1), info!("sta", inst!(sta), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("sty", inst!(sty), oper!(zpo), 3), info!("sta", inst!(sta), oper!(zpo), 3), info!("stx", inst!(stx), oper!(zpo), 3), info!("???", inst!(ill), oper!(imp), 1), info!("dey", inst!(dey), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("txa", inst!(txa), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("sty", inst!(sty), oper!(abs), 4), info!("sta", inst!(sta), oper!(abs), 4), info!("stx", inst!(stx), oper!(abs), 4), info!("???", inst!(ill), oper!(imp), 1),
    /* 9x */ info!("bcc", inst!(bcc), oper!(rel), 2), info!("sta", inst!(sta), oper!(izy), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("sty", inst!(sty), oper!(zpx), 4), info!("sta", inst!(sta), oper!(zpx), 4), info!("stx", inst!(stx), oper!(zpy), 4), info!("???", inst!(ill), oper!(imp), 1), info!("tya", inst!(tya), oper!(imp), 2), info!("sta", inst!(sta), oper!(aby), 5), info!("txs", inst!(txa), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 4), info!("sta", inst!(sta), oper!(abx), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1),
    /* Ax */ info!("ldy", inst!(ldy), oper!(imm), 2), info!("lda", inst!(lda), oper!(izx), 6), info!("ldx", inst!(ldx), oper!(imm), 2), info!("???", inst!(ill), oper!(imp), 1), info!("ldy", inst!(ldy), oper!(zpo), 3), info!("lda", inst!(lda), oper!(zpo), 3), info!("ldx", inst!(ldx), oper!(zpo), 3), info!("???", inst!(ill), oper!(imp), 1), info!("tay", inst!(tay), oper!(imp), 2), info!("lda", inst!(lda), oper!(imm), 2), info!("tax", inst!(tax), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("ldy", inst!(ldy), oper!(abs), 4), info!("lda", inst!(lda), oper!(abs), 4), info!("ldx", inst!(ldx), oper!(abs), 4), info!("???", inst!(ill), oper!(imp), 1),
    /* Bx */ info!("bcs", inst!(bcs), oper!(rel), 2), info!("lda", inst!(lda), oper!(izy), 5), info!("???", inst!(ill), oper!(imm), 2), info!("???", inst!(ill), oper!(imp), 1), info!("ldy", inst!(ldy), oper!(zpx), 4), info!("lda", inst!(lda), oper!(zpx), 4), info!("ldx", inst!(ldx), oper!(zpy), 4), info!("???", inst!(ill), oper!(imp), 1), info!("clv", inst!(clv), oper!(imp), 2), info!("lda", inst!(lda), oper!(aby), 4), info!("tsx", inst!(tsx), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("ldy", inst!(ldy), oper!(abx), 4), info!("lda", inst!(lda), oper!(abx), 4), info!("ldx", inst!(ldx), oper!(aby), 4), info!("???", inst!(ill), oper!(imp), 1),
    /* Cx */ info!("cpy", inst!(cpy), oper!(imm), 2), info!("cmp", inst!(cmp), oper!(izx), 6), info!("???", inst!(ill), oper!(imm), 1), info!("???", inst!(ill), oper!(imp), 1), info!("cpy", inst!(cpy), oper!(zpo), 3), info!("cmp", inst!(cmp), oper!(zpo), 3), info!("dec", inst!(dec), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("iny", inst!(iny), oper!(imp), 2), info!("cmp", inst!(cmp), oper!(imm), 2), info!("dex", inst!(dex), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("cpy", inst!(cpy), oper!(abs), 4), info!("cmp", inst!(cmp), oper!(abs), 4), info!("dec", inst!(dec), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* Dx */ info!("bne", inst!(bne), oper!(rel), 2), info!("cmp", inst!(cmp), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("cmp", inst!(cmp), oper!(zpx), 4), info!("dec", inst!(dec), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("cld", inst!(cld), oper!(imp), 2), info!("cmp", inst!(cmp), oper!(aby), 4), info!("dex", inst!(dex), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 4), info!("cmp", inst!(cmp), oper!(abx), 4), info!("dec", inst!(dec), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
    /* Ex */ info!("cpx", inst!(cpx), oper!(imm), 2), info!("sbc", inst!(sbc), oper!(izx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("cpx", inst!(cpx), oper!(zpo), 3), info!("sbc", inst!(sbc), oper!(zpo), 3), info!("inc", inst!(inc), oper!(zpo), 5), info!("???", inst!(ill), oper!(imp), 1), info!("inx", inst!(inx), oper!(imp), 2), info!("sbc", inst!(sbc), oper!(imm), 2), info!("nop", inst!(nop), oper!(imp), 2), info!("???", inst!(ill), oper!(imp), 1), info!("cpx", inst!(cpx), oper!(abs), 4), info!("sbc", inst!(sbc), oper!(abs), 4), info!("inc", inst!(inc), oper!(abs), 6), info!("???", inst!(ill), oper!(imp), 1),
    /* Fx */ info!("beq", inst!(beq), oper!(rel), 2), info!("sbc", inst!(sbc), oper!(izy), 5), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("sbc", inst!(sbc), oper!(zpx), 4), info!("inc", inst!(inc), oper!(zpx), 6), info!("???", inst!(ill), oper!(imp), 1), info!("sed", inst!(sed), oper!(imp), 2), info!("sbc", inst!(sbc), oper!(aby), 4), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("???", inst!(ill), oper!(imp), 1), info!("sbc", inst!(sbc), oper!(abx), 4), info!("inc", inst!(inc), oper!(abx), 7), info!("???", inst!(ill), oper!(imp), 1),
];
