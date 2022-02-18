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

impl Info {
    fn new(
        name: &'static str,
        executor: fn(&mut CPU6502),
        operands: fn(&mut CPU6502) -> u8,
        cycles: u16,
    ) -> Self {
        Self {
            name,
            executor,
            operands,
            cycles,
        }
    }
}

const IN: fn(&'static str, fn(&mut CPU6502), fn(&mut CPU6502) -> u8, u32) -> Info = IN;
type C<'a> = CPU6502<'a>;

#[rustfmt::skip]
pub(crate) const INSTR_LOOKUP: [Info; 256] = [
    //        x0                           x1                             x2                           x3                            x4                            x5                            x6                            x7                            x8                            x9                            xA                            xB                            xC                            xD                            xE                            xF
    /* 0x */ IN("brk", C::brk, C::imp, 7), IN("ora", C::ora, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("ora", C::ora, C::zpo, 3), IN("asl", C::asl, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("php", C::php, C::imp, 3), IN("ora", C::ora, C::imm, 2), IN("asl", C::asl, C::imm, 2), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("ora", C::ora, C::abs, 4), IN("asl", C::asl, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* 1x */ IN("bpl", C::bpl, C::rel, 2), IN("ora", C::ora, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("ora", C::ora, C::zpx, 4), IN("asl", C::asl, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("clc", C::clc, C::imp, 2), IN("ora", C::ora, C::aby, 4), IN("???", C::ill, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("ora", C::ora, C::abx, 4), IN("asl", C::asl, C::abx, 7), IN("???", C::ill, C::imp, 1),
    /* 2x */ IN("jsr", C::jsr, C::abs, 6), IN("and", C::and, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("bit", C::bit, C::zpo, 3), IN("and", C::and, C::zpo, 3), IN("rol", C::rol, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("plp", C::plp, C::imp, 4), IN("and", C::and, C::imm, 2), IN("rol", C::rol, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("bit", C::bit, C::abs, 4), IN("and", C::and, C::abs, 4), IN("rol", C::rol, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* 3x */ IN("bmi", C::bmi, C::rel, 2), IN("and", C::and, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("and", C::and, C::zpx, 4), IN("rol", C::rol, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("sec", C::sec, C::imp, 2), IN("and", C::and, C::aby, 4), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("and", C::and, C::abx, 4), IN("rol", C::rol, C::abx, 7), IN("???", C::ill, C::imp, 1),
    /* 4x */ IN("rti", C::rti, C::imp, 6), IN("eor", C::eor, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("eor", C::eor, C::zpo, 3), IN("lsr", C::lsr, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("pha", C::pha, C::imp, 3), IN("eor", C::eor, C::imm, 2), IN("lsr", C::lsr, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("jmp", C::jmp, C::abs, 3), IN("eor", C::eor, C::abs, 4), IN("lsr", C::lsr, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* 5x */ IN("bvc", C::bvc, C::rel, 2), IN("eor", C::eor, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("eor", C::eor, C::zpx, 4), IN("lsr", C::lsr, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("cli", C::cli, C::imp, 2), IN("eor", C::eor, C::aby, 4), IN("???", C::ill, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 3), IN("eor", C::eor, C::abx, 4), IN("lsr", C::lsr, C::abx, 7), IN("???", C::ill, C::imp, 1),
    /* 6x */ IN("rts", C::rts, C::imp, 6), IN("adc", C::adc, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("adc", C::adc, C::zpo, 3), IN("ror", C::ror, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("pla", C::pla, C::imp, 4), IN("adc", C::adc, C::imm, 2), IN("ror", C::ror, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("jmp", C::jmp, C::ind, 5), IN("adc", C::adc, C::abs, 4), IN("ror", C::ror, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* 7x */ IN("bvs", C::bvs, C::rel, 2), IN("adc", C::adc, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("adc", C::adc, C::zpx, 4), IN("ror", C::ror, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("sei", C::sei, C::imp, 2), IN("adc", C::adc, C::aby, 4), IN("???", C::ror, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("adc", C::adc, C::abx, 4), IN("ror", C::ror, C::abx, 7), IN("???", C::ill, C::imp, 1),
    /* 8x */ IN("???", C::ill, C::imp, 1), IN("sta", C::sta, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("sty", C::sty, C::zpo, 3), IN("sta", C::sta, C::zpo, 3), IN("stx", C::stx, C::zpo, 3), IN("???", C::ill, C::imp, 1), IN("dey", C::dey, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("txa", C::txa, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("sty", C::sty, C::abs, 4), IN("sta", C::sta, C::abs, 4), IN("stx", C::stx, C::abs, 4), IN("???", C::ill, C::imp, 1),
    /* 9x */ IN("bcc", C::bcc, C::rel, 2), IN("sta", C::sta, C::izy, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("sty", C::sty, C::zpx, 4), IN("sta", C::sta, C::zpx, 4), IN("stx", C::stx, C::zpy, 4), IN("???", C::ill, C::imp, 1), IN("tya", C::tya, C::imp, 2), IN("sta", C::sta, C::aby, 5), IN("txs", C::txa, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 4), IN("sta", C::sta, C::abx, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1),
    /* Ax */ IN("ldy", C::ldy, C::imm, 2), IN("lda", C::lda, C::izx, 6), IN("ldx", C::ldx, C::imm, 2), IN("???", C::ill, C::imp, 1), IN("ldy", C::ldy, C::zpo, 3), IN("lda", C::lda, C::zpo, 3), IN("ldx", C::ldx, C::zpo, 3), IN("???", C::ill, C::imp, 1), IN("tay", C::tay, C::imp, 2), IN("lda", C::lda, C::imm, 2), IN("tax", C::tax, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("ldy", C::ldy, C::abs, 4), IN("lda", C::lda, C::abs, 4), IN("ldx", C::ldx, C::abs, 4), IN("???", C::ill, C::imp, 1),
    /* Bx */ IN("bcs", C::bcs, C::rel, 2), IN("lda", C::lda, C::izy, 5), IN("???", C::ill, C::imm, 2), IN("???", C::ill, C::imp, 1), IN("ldy", C::ldy, C::zpx, 4), IN("lda", C::lda, C::zpx, 4), IN("ldx", C::ldx, C::zpy, 4), IN("???", C::ill, C::imp, 1), IN("clv", C::clv, C::imp, 2), IN("lda", C::lda, C::aby, 4), IN("tsx", C::tsx, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("ldy", C::ldy, C::abx, 4), IN("lda", C::lda, C::abx, 4), IN("ldx", C::ldx, C::aby, 4), IN("???", C::ill, C::imp, 1),
    /* Cx */ IN("cpy", C::cpy, C::imm, 2), IN("cmp", C::cmp, C::izx, 6), IN("???", C::ill, C::imm, 1), IN("???", C::ill, C::imp, 1), IN("cpy", C::cpy, C::zpo, 3), IN("cmp", C::cmp, C::zpo, 3), IN("dec", C::dec, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("iny", C::iny, C::imp, 2), IN("cmp", C::cmp, C::imm, 2), IN("dex", C::dex, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("cpy", C::cpy, C::abs, 4), IN("cmp", C::cmp, C::abs, 4), IN("dec", C::dec, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* Dx */ IN("bne", C::bne, C::rel, 2), IN("cmp", C::cmp, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("cmp", C::cmp, C::zpx, 4), IN("dec", C::dec, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("cld", C::cld, C::imp, 2), IN("cmp", C::cmp, C::aby, 4), IN("dex", C::dex, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 4), IN("cmp", C::cmp, C::abx, 4), IN("dec", C::dec, C::abx, 7), IN("???", C::ill, C::imp, 1),
    /* Ex */ IN("cpx", C::cpx, C::imm, 2), IN("sbc", C::sbc, C::izx, 6), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("cpx", C::cpx, C::zpo, 3), IN("sbc", C::sbc, C::zpo, 3), IN("inc", C::inc, C::zpo, 5), IN("???", C::ill, C::imp, 1), IN("inx", C::inx, C::imp, 2), IN("sbc", C::sbc, C::imm, 2), IN("nop", C::nop, C::imp, 2), IN("???", C::ill, C::imp, 1), IN("cpx", C::cpx, C::abs, 4), IN("sbc", C::sbc, C::abs, 4), IN("inc", C::inc, C::abs, 6), IN("???", C::ill, C::imp, 1),
    /* Fx */ IN("beq", C::beq, C::rel, 2), IN("sbc", C::sbc, C::izy, 5), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("sbc", C::sbc, C::zpx, 4), IN("inc", C::inc, C::zpx, 6), IN("???", C::ill, C::imp, 1), IN("sed", C::sed, C::imp, 2), IN("sbc", C::sbc, C::aby, 4), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("???", C::ill, C::imp, 1), IN("sbc", C::sbc, C::abx, 4), IN("inc", C::inc, C::abx, 7), IN("???", C::ill, C::imp, 1),
];
