//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

pub enum RW {
    Read,
    Write,
}

pub struct Bus {
    /// The last address used by any instruction
    address: u16,
    /// The last data written to the bus
    data: u8,
}

impl Bus {
    pub fn read(&mut self, address: u16) -> u8 {
        0
    }

    pub fn write(&mut self, address: u16, data: u8) {
        //
    }
}
