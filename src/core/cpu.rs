//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

use crate::core::lookup::{lookup_instruction, Info};
use crate::core::Bus;
use crate::numeric::Arithmetic6502;

macro_rules! implement_branch {
    ($self:ident, $operand:ident, $condition:expr) => {{
        if $condition {
            // branch taken => extra cycle
            $self.cycles_left += 1;

            // page boundary cross => extra cycle
            if ($operand.address() & 0xFF00) != ($self.pc & 0xFF00) {
                $self.cycles_left += 1;
            }

            $self.pc = $operand.address();
        }

        false
    }};
}

macro_rules! branch_if_set {
    ($self:ident, $operand:ident, $flag:ident) => {{
        implement_branch!($self, $operand, $self.is_set(StatusFlag::$flag))
    }};
}

macro_rules! branch_if_unset {
    ($self:ident, $operand:ident, $flag:ident) => {{
        implement_branch!($self, $operand, $self.is_unset(StatusFlag::$flag))
    }};
}

macro_rules! clear_flag {
    ($self:ident, $flag:ident) => {{
        $self.set_flag(StatusFlag::$flag, false);

        false
    }};
}

/// Gets an offset into the zero page, starting at `start`
fn zp_address(start: u8, offset: u8) -> u16 {
    ((start as u16) + (offset as u16)) % 256
}

/// An operand to an instruction
#[derive(Copy, Clone)]
pub struct Operand {
    value: u16,
    extra_cycle: bool,
    should_load: bool,
}

impl Operand {
    /// Gets the value as an address, and does nothing else.
    fn address(self) -> u16 {
        self.value
    }

    /// Gets the value in a usable byte form.
    ///
    /// If the operand is an indirect value (i.e. an address to the actual byte
    /// that the instruction wants), the byte is loaded. Otherwise, the
    /// internal value is just returned.
    ///
    /// Also returns whether or not an extra cycle is necessary from a load
    /// that caused a page transition.
    fn value(self, cpu: &mut CPU6502) -> (u8, bool) {
        let value = if !self.should_load {
            self.value as u8
        } else {
            cpu.read(self.value)
        };

        (value, self.extra_cycle)
    }
}

/// Represents the bit positions of each of the flags in the status register
#[repr(u8)]
#[derive(Clone, Copy)]
pub enum StatusFlag {
    /// Carry flag
    C = 0,
    /// Zero flag
    Z = 1,
    /// Disable Interrupts flag
    I = 2,
    /// Unused (decimal mode: unsupported by NES)
    D = 3,
    /// Break flag
    B = 4,
    /// Unused
    U = 5,
    /// Signed overflow flag
    V = 6,
    /// Negative flag
    N = 7,
}

pub struct CPU6502<'a> {
    bus: &'a mut Bus,
    a: u8,            // accumulator
    x: u8,            // gp register
    y: u8,            // gp register
    p: u8,            // status register
    sp: u16,          // stack pointer
    pc: u16,          // program counter
    cycles_left: u16, // counts the number of cycles we need to "eat" to maintain timing
    opcode: u8,       // the current opcode
}

impl<'a> CPU6502<'a> {
    /// Creates a new 6502 CPU emulator in a "reset" state.
    pub fn new(bus: &'a mut Bus) -> Self {
        Self {
            bus,
            a: 0,
            x: 0,
            y: 0,
            p: 0,
            sp: 0,
            pc: 0,
            cycles_left: 0,
            opcode: 0,
        }
    }

    /// Runs a single CPU cycle. If a previous instruction has any cycles "remaining,"
    /// this function does nothing. Otherwise, it reads the next instruction and executes it,
    /// storing how many cycles are left to be executed.
    pub fn cycle(&mut self) {
        if self.cycles_left == 0 {
            let info = self.next_instruction();
            let operand = (info.operands)(self);

            self.cycles_left = info.cycles;

            // true => extra cycle, we hit a page transition
            if (info.executor)(self, operand) {
                self.cycles_left += 1;
            }
        }

        self.cycles_left -= 1;
    }

    /// Resets the 6502 to a clean state.
    ///
    /// This is equivalent to the state created by `Self::new(bus)`
    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.p = 0;
        self.sp = 0;
        self.pc = 0;
        self.cycles_left = 0;
        self.opcode = 0;
    }

    /// Requests that an interrupt be handled, this is a non-binding request. This only runs
    /// if the `I` flag is not set.
    pub fn interrupt_request(&mut self) {
        todo!()
    }

    /// Tells the CPU that there's an interrupt happening that must be handled. Ignores the
    /// `I` flag.
    pub fn interrupt_non_maskable(&mut self) {
        todo!()
    }

    /// Sets a status flag to the specified value
    pub fn set_flag(&mut self, flag: StatusFlag, state: bool) {
        self.p &= !(1 << (flag as u8)); // toggle off the flag bit for `flag`
        self.p |= (state as u8) << (flag as u8); // set that bit to `state
    }

    /// Checks if a status flag is set
    pub fn is_set(&self, flag: StatusFlag) -> bool {
        (self.p & (1 << flag as u8)) != 0
    }

    /// Checks if a status flag is unset
    pub fn is_unset(&self, flag: StatusFlag) -> bool {
        !self.is_set(flag)
    }

    /// Moves to the next instruction, updates PC
    fn next_instruction(&mut self) -> &'static Info {
        self.opcode = self.read_pc();

        lookup_instruction(self.opcode)
    }

    /// Updates the N/Z flags based on a byte value
    fn update_nz(&mut self, value: u8) {
        self.set_flag(StatusFlag::N, (value & 0b1000_0000) != 0);
        self.set_flag(StatusFlag::Z, value == 0);
    }

    /// Reads the byte at PC, increments PC
    fn read_pc(&mut self) -> u8 {
        self.pc += 1;

        self.read(self.pc - 1)
    }

    /// Reads a byte at `address`
    fn read(&mut self, address: u16) -> u8 {
        self.bus.read(address, false)
    }

    /// Reads a little-endian address from `address`
    fn read_address(&mut self, address: u16) -> u16 {
        let low = self.read(address);
        let high = self.read(address + 1);

        u16::from_le_bytes([low, high])
    }

    /// Reads a little-endian address from PC, moves PC forward by 2
    fn read_address_pc(&mut self) -> u16 {
        self.pc += 2;

        self.read_address(self.pc - 2)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.bus.write(address, value);
    }

    fn cmp_with(&mut self, operand: Operand, reg: u8) -> bool {
        let (value, paged) = operand.value(self);
        let (result, carry) = value.sub_with_carry(reg, self.is_set(StatusFlag::C));

        self.update_nz(result);
        self.set_flag(StatusFlag::C, carry);

        paged
    }

    /// Gets an absolute address from PC, increments it by `register` and returns
    /// it as a packaged `Operand`
    fn abs_with(&mut self, register: u8) -> Operand {
        let address = self.read_address_pc();
        let incremented = address + register as u16;

        Operand {
            value: incremented,
            // if high byte differs after increment, we need to do an extra cycle.
            // 6502 does the load and increment in parallel, and checks if it loaded the right
            // address afterwards by comparing the bytes, for accuracy we "do the same."
            extra_cycle: (incremented & 0xFF00) != (address & 0xFF00),
            should_load: true,
        }
    }

    /// Models the `a` (absolute) addressing mode. Gets an absolute
    /// address encoded in the instruction.  
    pub fn abs(&mut self) -> Operand {
        // zero will get optimized away
        self.abs_with(0)
    }

    /// Models the `a,x` (absolute x-indexed) addressing mode. Gets an absolute
    /// address encoded in the address, and increments it by `x`
    pub fn abx(&mut self) -> Operand {
        self.abs_with(self.x)
    }

    /// Models the `a,y` (absolute x-indexed) addressing mode. Gets an absolute
    /// address encoded in the address, and increments it by `y`
    pub fn aby(&mut self) -> Operand {
        self.abs_with(self.y)
    }

    /// Models the `#v` (immediate) addressing mode. Gets an immediate that
    /// is directly encoded in the instruction as the operand
    pub fn imm(&mut self) -> Operand {
        Operand {
            value: self.read_pc() as u16,
            extra_cycle: false,
            should_load: false,
        }
    }

    /// Models the implicit addressing mode for instructions with no operands.
    ///
    /// This is implicitly the `a` register for some instructions, and for others
    /// they make no use of it. In either case, we give the `a` register anyway.
    pub fn imp(&mut self) -> Operand {
        Operand {
            value: self.a as u16,
            extra_cycle: false,
            should_load: false,
        }
    }

    /// Models the `(a)` (indirect) addressing mode for `jmp` instructions. This
    /// takes a 16-bit address encoded in the instruction, and uses the
    /// address stored *at* that address as the operand.
    ///
    /// This implementation correctly implements the 6502's page boundary bug with
    /// this addressing mode.
    pub fn ind(&mut self) -> Operand {
        let ptr = self.read_address_pc();

        // NES 6502 had a hardware bug here, where instead of going to next
        // page it would wrap around the same page.
        //
        // ex: reading 0x56FF would get 0x5600 and 0x56FF as the bytes, not 0x5700 and 0x56FF
        if (ptr & 0x00FF) == 0xFF {
            let low = self.read(ptr);
            let high = self.read(ptr & 0xFF00);

            return Operand {
                value: self.read_address(u16::from_le_bytes([low, high])),
                extra_cycle: false,
                should_load: true,
            };
        }

        Operand {
            value: self.read_address(ptr),
            extra_cycle: false,
            should_load: true,
        }
    }

    /// Models the `(d,x)` (indexed indirect) addressing mode. This takes a zero-page-offset
    /// encoded in the instruction, turns it into a zero-page address, increments it by `x` (while
    /// maintaining the "zero-page-ness" of the address) and then loads an address from that
    /// location to use as the operand.
    pub fn izx(&mut self) -> Operand {
        let address = zp_address(self.read_pc(), self.x);
        let low = self.read(address);
        let high = self.read((address + 1) % 256);

        Operand {
            value: u16::from_le_bytes([low, high]),
            extra_cycle: false,
            should_load: true,
        }
    }

    /// Models the `(d),y` (indirect indexed) addressing mode. Takes a zero-page offset
    /// encoded in the instruction, turns it into a zero-page address, loads an address from *that*,
    /// and then increments that address by `y`.
    pub fn izy(&mut self) -> Operand {
        let address = self.read_pc() as u16;
        let low = self.read(address);
        let high = self.read((address + 1) % 256);
        let incremented = u16::from_le_bytes([low, high]) + self.y as u16;

        Operand {
            value: incremented,
            // same as with abx/aby, if we cross a page we need an extra cycle
            extra_cycle: (incremented & 0xFF00) != ((high as u16) << 8),
            should_load: true,
        }
    }

    /// Models the `label` (relative) addressing mode. This gets a signed 8-bit offset
    /// that is relative to the current PC value.
    pub fn rel(&mut self) -> Operand {
        Operand {
            // same representation as immediate, but here we're conceptually getting
            // an i8 instead of a u8
            value: self.read_pc() as u16,
            extra_cycle: false,
            should_load: true,
        }
    }

    /// Gets a zero-page offset with some register value.
    fn zp_with(&mut self, reg: u8) -> Operand {
        Operand {
            // this add is done without carrying, page (high byte) should still be zero by the end
            // even if we "overflow" the low bit in the addition
            value: zp_address(self.read_pc(), reg),
            extra_cycle: false,
            should_load: true,
        }
    }

    /// Models the `d` (zero-page) addressing mode. Gets a byte from the instruction
    /// and uses it as an offset into the zero page.  
    pub fn zpo(&mut self) -> Operand {
        self.zp_with(0)
    }

    /// Models the `d,x` (zero-page x-indexed) addressing mode. Gets a byte from the instruction
    /// and uses it (added with X) as an offset into the zero page.  
    pub fn zpx(&mut self) -> Operand {
        self.zp_with(self.x)
    }

    /// Models the `d,y` (zero-page y-indexed) addressing mode. Gets a byte from the instruction
    /// and uses it (added with Y) as an offset into the zero page.  
    pub fn zpy(&mut self) -> Operand {
        self.zp_with(self.y)
    }

    /// Handles an illegal instruction by doing nothing. Same as NOP.
    pub fn ill(&mut self, _: Operand) -> bool {
        false
    }

    /// Add to Accumulator with Carry: Performs add-with-carry, adding 1 if the carry flag is set.
    ///
    /// Updates: `N`, `Z`, `C`
    pub fn adc(&mut self, operand: Operand) -> bool {
        let (value, paged) = operand.value(self);
        let (value, carry) = value.add_with_carry(self.a, self.is_set(StatusFlag::C));

        self.a = value;
        self.set_flag(StatusFlag::C, carry);
        self.update_nz(self.a);

        paged
    }

    /// AND with Accumulator: Performs a bitwise AND with the accumulator register.
    ///
    /// Updates: `N`, `Z`
    pub fn and(&mut self, operand: Operand) -> bool {
        let (value, paged) = operand.value(self);

        self.a &= value;
        self.update_nz(self.a);

        paged
    }

    /// Arithmetic Shift Left: Shifts the operand to the left by one bit
    ///
    /// Affects: `N`, `Z`, `C`
    pub fn asl(&mut self, operand: Operand) -> bool {
        let (value, _) = operand.value(self);
        let result = value << 1;
        let carry = value & 0b1000_0000;

        self.set_flag(StatusFlag::C, carry != 0);

        // can't shift an immediate, can only shift A or an address
        if operand.should_load {
            self.write(operand.address(), result);
        } else {
            self.a = result;
        }

        // never dependent on pages
        false
    }

    /// Branch on Carry Clear: Branches when the Carry flag is unset
    pub fn bcc(&mut self, operand: Operand) -> bool {
        branch_if_unset!(self, operand, C)
    }

    /// Branch on Carry Set: Branches when the Carry flag is set
    pub fn bcs(&mut self, operand: Operand) -> bool {
        branch_if_set!(self, operand, C)
    }

    /// Branch on Equal: Branches when the Zero flag is set
    pub fn beq(&mut self, operand: Operand) -> bool {
        branch_if_set!(self, operand, Z)
    }

    pub fn bit(&mut self, operand: Operand) -> bool {
        todo!()
    }

    /// Branch on Minus: Branches when the Negative flag is set
    pub fn bmi(&mut self, operand: Operand) -> bool {
        branch_if_set!(self, operand, N)
    }

    /// Branch on Not Equal: Branches when the Zero flag is unset
    pub fn bne(&mut self, operand: Operand) -> bool {
        branch_if_unset!(self, operand, Z)
    }

    /// Branch on Plus: Branches when the negative flag is unset
    pub fn bpl(&mut self, operand: Operand) -> bool {
        branch_if_unset!(self, operand, N)
    }

    pub fn brk(&mut self, operand: Operand) -> bool {
        todo!()
    }

    /// Branch on Overflow Clear: Branches when the Overflow flag is unset
    pub fn bvc(&mut self, operand: Operand) -> bool {
        branch_if_unset!(self, operand, V)
    }

    /// Branch on Overflow Clear: Branches when the Overflow flag is set
    pub fn bvs(&mut self, operand: Operand) -> bool {
        branch_if_set!(self, operand, V)
    }

    /// Clear Carry: Unsets the carry flag
    pub fn clc(&mut self, _: Operand) -> bool {
        clear_flag!(self, C)
    }

    /// Clear Decimal: Unsets the decimal mode flag (unused, may as well
    /// implement it though)
    pub fn cld(&mut self, _: Operand) -> bool {
        clear_flag!(self, D)
    }

    /// Clear Interrupt: Unsets the Disable Interrupts flag
    pub fn cli(&mut self, _: Operand) -> bool {
        clear_flag!(self, I)
    }

    /// Clear Overflow: Unsets the Overflow flag
    pub fn clv(&mut self, _: Operand) -> bool {
        clear_flag!(self, V)
    }

    /// Compare with Accumulator: Sets flags as-if the subtraction `A - {operand}` had
    /// been performed.
    ///
    /// Affects: `N`, `Z`, `C`
    pub fn cmp(&mut self, operand: Operand) -> bool {
        self.cmp_with(operand, self.a)
    }

    /// Compare with X: Sets flags as-if the subtraction `X - {operand}` had
    /// been performed.
    ///
    /// Affects: `N`, `Z`, `C`
    pub fn cpx(&mut self, operand: Operand) -> bool {
        self.cmp_with(operand, self.x)
    }

    /// Compare with Y: Sets flags as-if the subtraction `Y - {operand}` had
    /// been performed.
    ///
    /// Affects: `N`, `Z`, `C`
    pub fn cpy(&mut self, operand: Operand) -> bool {
        self.cmp_with(operand, self.y)
    }

    /// Decrement Memory: Decrements the memory at the address in the operand
    ///
    /// Affects flags: `N`, `Z`
    pub fn dec(&mut self, operand: Operand) -> bool {
        let address = operand.address();
        let value = self.read(address).wrapping_sub(1);

        // while it's technically not a all-in-one read-write-modify,
        // it's not like you can observe the result in the meantime
        self.write(address, value);
        self.update_nz(value);

        false
    }

    /// Decrement X: Decrements the X register
    ///
    /// Affects flags: `N`, `Z`
    pub fn dex(&mut self, _: Operand) -> bool {
        self.x = self.x.wrapping_sub(1);
        self.update_nz(self.x);

        false
    }

    /// Decrement Y: Decrements the Y register
    ///
    /// Affects flags: `N`, `Z`
    pub fn dey(&mut self, _: Operand) -> bool {
        self.y = self.y.wrapping_sub(1);
        self.update_nz(self.y);

        false
    }

    /// XOR with Accumulator: XORs the operand with A, puts the result in A
    ///
    /// Affects flags: `N`, `Z`
    pub fn eor(&mut self, operand: Operand) -> bool {
        let (value, paged) = operand.value(self);

        self.a ^= value;
        self.update_nz(self.a);

        paged
    }

    /// Increment Memory: Increments the value at the memory given by the operand
    ///
    /// Affects flags: `N`, `Z`
    pub fn inc(&mut self, operand: Operand) -> bool {
        let address = operand.address();
        let value = self.read(address).wrapping_add(1);

        // while it's technically not a all-in-one read-write-modify,
        // it's not like you can observe the result in the meantime
        self.write(address, value);
        self.update_nz(value);

        false
    }

    /// Increment X: Increments the X register
    ///
    /// Affects flags: `N`, `Z`
    pub fn inx(&mut self, operand: Operand) -> bool {
        self.x = self.x.wrapping_add(1);
        self.update_nz(self.x);

        false
    }

    /// Increment Y: Increments the Y register
    ///
    /// Affects flags: `N`, `Z`
    pub fn iny(&mut self, operand: Operand) -> bool {
        self.y = self.y.wrapping_add(1);
        self.update_nz(self.y);

        false
    }

    /// Jumps to an address given by the operand
    pub fn jmp(&mut self, operand: Operand) -> bool {
        self.pc = operand.address();

        false
    }

    pub fn jsr(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn lda(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn ldx(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn ldy(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn lsr(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn nop(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn ora(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn pha(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn php(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn pla(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn plp(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn rol(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn ror(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn rti(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn rts(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sbc(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sec(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sed(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sei(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sta(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn stx(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn sty(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn tax(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn tay(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn tsx(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn txa(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn txs(&mut self, operand: Operand) -> bool {
        todo!()
    }

    pub fn tya(&mut self, operand: Operand) -> bool {
        todo!()
    }
}
