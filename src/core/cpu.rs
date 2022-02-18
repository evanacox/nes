//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

use crate::core::lookup::INSTR_LOOKUP;
use crate::core::Bus;

pub struct CPU6502<'a> {
    a: u8,            // accumulator
    x: u8,            // gp register
    y: u8,            // gp register
    p: u8,            // status register
    sp: u16,          // stack pointer
    pc: u16,          // program counter
    cycles_left: u16, // counts the number of cycles we need to "eat" to maintain timing
    bus: &'a Bus,
}

impl<'a> CPU6502<'a> {
    pub fn new(bus: &'a Bus) -> Self<'a> {
        Self {
            a: 0,
            x: 0,
            y: 0,
            p: 0,
            sp: 0,
            pc: 0,
            cycles_left: 0,
            bus,
        }
    }

    pub fn cycle(&mut self) {
        if self.cycles_left == 0 {
            let info = &INSTR_LOOKUP[0x55];
            info.executor(self);
            self.cycles_left = info.cycles - 1;
        }

        self.cycles_left -= 1;
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }

    pub fn interrupt_request(&mut self) {
        unimplemented!()
    }

    pub fn interrupt_non_maskable(&mut self) {
        unimplemented!()
    }

    pub(crate) fn abs(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn abx(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn aby(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn imm(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn imp(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn izx(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn izy(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn ind(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn rel(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn zpo(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn zpx(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn zpy(&mut self) -> u8 {
        unimplemented!()
    }

    pub(crate) fn ill(&mut self) {
        unimplemented!()
    }

    pub(crate) fn adc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn and(&mut self) {
        unimplemented!()
    }

    pub(crate) fn asl(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bcc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bcs(&mut self) {
        unimplemented!()
    }

    pub(crate) fn beq(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bit(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bmi(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bne(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bpl(&mut self) {
        unimplemented!()
    }

    pub(crate) fn brk(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bvc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn bvs(&mut self) {
        unimplemented!()
    }

    pub(crate) fn clc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn cld(&mut self) {
        unimplemented!()
    }

    pub(crate) fn cli(&mut self) {
        unimplemented!()
    }

    pub(crate) fn clv(&mut self) {
        unimplemented!()
    }

    pub(crate) fn cmp(&mut self) {
        unimplemented!()
    }

    pub(crate) fn cpx(&mut self) {
        unimplemented!()
    }

    pub(crate) fn cpy(&mut self) {
        unimplemented!()
    }

    pub(crate) fn dec(&mut self) {
        unimplemented!()
    }

    pub(crate) fn dex(&mut self) {
        unimplemented!()
    }

    pub(crate) fn dey(&mut self) {
        unimplemented!()
    }

    pub(crate) fn eor(&mut self) {
        unimplemented!()
    }

    pub(crate) fn inc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn inx(&mut self) {
        unimplemented!()
    }

    pub(crate) fn iny(&mut self) {
        unimplemented!()
    }

    pub(crate) fn jmp(&mut self) {
        unimplemented!()
    }

    pub(crate) fn jsr(&mut self) {
        unimplemented!()
    }

    pub(crate) fn lda(&mut self) {
        unimplemented!()
    }

    pub(crate) fn ldx(&mut self) {
        unimplemented!()
    }

    pub(crate) fn ldy(&mut self) {
        unimplemented!()
    }

    pub(crate) fn lsr(&mut self) {
        unimplemented!()
    }

    pub(crate) fn nop(&mut self) {
        unimplemented!()
    }

    pub(crate) fn ora(&mut self) {
        unimplemented!()
    }

    pub(crate) fn pha(&mut self) {
        unimplemented!()
    }

    pub(crate) fn php(&mut self) {
        unimplemented!()
    }

    pub(crate) fn pla(&mut self) {
        unimplemented!()
    }

    pub(crate) fn plp(&mut self) {
        unimplemented!()
    }

    pub(crate) fn rol(&mut self) {
        unimplemented!()
    }

    pub(crate) fn ror(&mut self) {
        unimplemented!()
    }

    pub(crate) fn rti(&mut self) {
        unimplemented!()
    }

    pub(crate) fn rts(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sbc(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sec(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sed(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sei(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sta(&mut self) {
        unimplemented!()
    }

    pub(crate) fn stx(&mut self) {
        unimplemented!()
    }

    pub(crate) fn sty(&mut self) {
        unimplemented!()
    }

    pub(crate) fn tax(&mut self) {
        unimplemented!()
    }

    pub(crate) fn tay(&mut self) {
        unimplemented!()
    }

    pub(crate) fn tsx(&mut self) {
        unimplemented!()
    }

    pub(crate) fn txa(&mut self) {
        unimplemented!()
    }

    pub(crate) fn txs(&mut self) {
        unimplemented!()
    }

    pub(crate) fn tya(&mut self) {
        unimplemented!()
    }
}
