//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

use crate::core::lookup;
use crate::core::lookup::{lookup_instruction, Info};
use crate::core::CPU6502;

pub struct CPUState {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub pc: u16,
    pub sp: u16,
    pub nearby_ops: Vec<String>,
    pub relative_pc: usize, // the offset of the current instruction in `nearby_ops`
}

fn disassemble_single(cpu: &CPU6502, pc: u16) -> (&'static Info, String) {
    let opcode = cpu.bus.read_nocycle(pc);
    let info = lookup_instruction(opcode);

    if info.name == "???" {
        return (info, format!("??? ({:02X})", opcode));
    }

    let s = match info.operands {
        lookup::abs => {
            let low = cpu.bus.read_nocycle(pc + 1);
            let high = cpu.bus.read_nocycle(pc + 2);

            format!("{} ${:04X}", info.name, u16::from_le_bytes([low, high]))
        }
        lookup::abx => {
            let low = cpu.bus.read_nocycle(pc + 1);
            let high = cpu.bus.read_nocycle(pc + 2);

            format!("{} ${:04X},X", info.name, u16::from_le_bytes([low, high]))
        }
        lookup::aby => {
            let low = cpu.bus.read_nocycle(pc + 1);
            let high = cpu.bus.read_nocycle(pc + 2);

            format!("{} ${:04X},Y", info.name, u16::from_le_bytes([low, high]))
        }
        lookup::imm => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} #${:02X}", info.name, byte)
        }
        lookup::imp => {
            if ["asl", "shr", "rol", "ror"].contains(&info.name) {
                format!("{} A", info.name)
            } else {
                format!("{}", info.name)
            }
        }
        lookup::ind => {
            let low = cpu.bus.read_nocycle(pc + 1);
            let high = cpu.bus.read_nocycle(pc + 2);

            format!("{} (${:04X})", info.name, u16::from_le_bytes([low, high]))
        }
        lookup::izx => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} (${:02X},X)", info.name, byte)
        }
        lookup::izy => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} (${:02X}),Y", info.name, byte)
        }
        lookup::rel => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} ${:02X}", info.name, byte)
        }
        lookup::zpo => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} ${:02X}", info.name, byte)
        }
        lookup::zpx => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} ${:02X},X", info.name, byte)
        }
        lookup::zpy => {
            let byte = cpu.bus.read_nocycle(pc + 1);

            format!("{} ${:02X},Y", info.name, byte)
        }
        _ => panic!("should be unreachable"),
    };

    (info, s)
}

fn disassemble_nearby(cpu: &CPU6502, prev: usize, next: usize) -> (Vec<String>, usize) {
    assert!(0 <= prev && prev <= 8);

    let mut instructions = Vec::<String>::new();
    let mut offset_of_current = 0usize;

    let mut pc = {
        let mut decrement_by = 0;

        // for `prev` 2-bit chunks (starting at LSB), get the value by itself (0-3) and
        // add that to `decrement_by` so we can go backwards from PC as far as needed
        for i in 0..prev {
            let bit = i * 2;
            let chunk = (cpu.prev_len & (0b11 << bit)) >> bit;

            decrement_by += chunk;
        }

        cpu.pc - decrement_by
    };

    for i in 0..(prev + next + 1) {
        let (info, instruction) = disassemble_single(cpu, pc);

        if pc == cpu.pc {
            offset_of_current = i;
        }

        instructions.push(instruction);
        pc += instruction_length(info) as u16;
    }

    (instructions, offset_of_current)
}

/// Gets the length in bytes of a given instruction based off of
/// the info obtained from its opcode
pub(crate) fn instruction_length(info: &'static Info) -> usize {
    match info.operands {
        lookup::abs => 3,
        lookup::abx => 3,
        lookup::aby => 3,
        lookup::imm => 2,
        lookup::imp => 1,
        lookup::ind => 3,
        lookup::izx => 2,
        lookup::izy => 2,
        lookup::rel => 2,
        lookup::zpo => 2,
        lookup::zpx => 2,
        lookup::zpy => 2,
        _ => panic!("should be unreachable"),
    }
}

/// Gets the CPU state starting at `prev` instructions before the current, to `next`
/// instructions after the current instruction.
///
/// # Panics
/// `prev` must be in the range `0 <= prev <= 8`
pub fn cpu_state_with(cpu: &CPU6502, prev: usize, next: usize) -> CPUState {
    let (nearby, relative) = disassemble_nearby(cpu, prev, next);

    CPUState {
        a: cpu.a,
        x: cpu.x,
        y: cpu.y,
        p: cpu.p,
        pc: cpu.pc,
        sp: cpu.sp,
        nearby_ops: nearby,
        relative_pc: relative,
    }
}

/// Gets the CPU state with a default number of preceding/following bytes
///
/// Equivalent to `cpu_state_with(cpu, N, M)`
pub fn cpu_state(cpu: &CPU6502) -> CPUState {
    cpu_state_with(cpu, 4, 4)
}
