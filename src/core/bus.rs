//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

pub trait Device {
    const START: u16;

    const END: u16;

    fn read(&mut self, address: u16, bus_data: u8) -> u8;

    fn read_nocycle(&self, address: u16, bus_data: u8) -> u8;

    fn write(&mut self, address: u16, data: u8);
}

struct RAM {
    data: Box<[u8; (u16::MAX as usize) + 1]>,
}

impl RAM {
    pub fn new() -> Self {
        Self {
            data: Box::new([0 as u8; (u16::MAX as usize) + 1]),
        }
    }
}

impl Device for RAM {
    const START: u16 = 0;
    const END: u16 = u16::MAX;

    fn read(&mut self, address: u16, _bus_data: u8) -> u8 {
        self.data[address as usize]
    }

    fn read_nocycle(&self, address: u16, _bus_data: u8) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.data[address as usize] = data;
    }
}

pub struct Bus {
    address: u16, // last address read/written to
    data: u8,     // last data written
    ram: RAM,
}

impl Bus {
    pub fn new() -> Self {
        Self {
            address: 0,
            data: 0,
            ram: RAM::new(),
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        self.address = address;

        self.read_nocycle(self.address)
    }

    pub fn read_nocycle(&self, address: u16) -> u8 {
        if RAM::START <= address && address <= RAM::END {
            return self.ram.read_nocycle(address, self.data);
        }

        0
    }

    pub fn write(&mut self, address: u16, data: u8) {
        self.address = address;
        self.data = data;

        println!("writing {:02X} to {:04X}", data, address);

        if RAM::START <= self.address && self.address <= RAM::END {
            self.ram.write(self.address, self.data);
        }
    }
}
