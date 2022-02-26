//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

pub mod core;
pub mod numeric;

use crate::core::{Bus, CPU6502};

fn main() {
    let mut bus = Bus::new();

    bus.write(CPU6502::STARTUP_HANDLER, 0x10); // *(STARTUP_HANDLER) == 0x0010
    bus.write(CPU6502::STARTUP_HANDLER + 1, 0x00);
    bus.write(0x0010, 0xAD); // LDA $0030
    bus.write(0x0011, 0x30);
    bus.write(0x0012, 0x00);
    bus.write(0x0013, 0x0A); // ASL A
    bus.write(0x0014, 0x8D); // STA $0034
    bus.write(0x0015, 0x34);
    bus.write(0x0016, 0x00);
    bus.write(0x0017, 0x00); // BRK

    bus.write(CPU6502::INTERRUPT_HANDLER, 0x10); // *(INTERRUPT_HANDLER) == 0x0010
    bus.write(CPU6502::INTERRUPT_HANDLER + 1, 0x00);

    {
        let mut cpu = CPU6502::new(&mut bus);

        while cpu.read_pc_nocycle() != 0x00 {
            let data = core::cpu_state(&cpu);

            println!("CPU State");
            println!("  a: {:02X}", data.a);
            println!("  x: {:02X}", data.x);
            println!("  y: {:02X}", data.y);
            println!("  pc: {:04X}", data.pc);
            println!("  sp: {:04X}", data.sp);
            println!("Instructions: ");

            for (i, inst) in data.nearby_ops.iter().enumerate() {
                if i == data.relative_pc {
                    print!("> ");
                } else {
                    print!("  ");
                }

                println!("{}", inst);
            }

            cpu.cycle();
        }
    }
}
