//======---------------------------------------------------------------======//
//                                                                           //
// Copyright 2022 Evan Cox <evanacox00@gmail.com>. All rights reserved.      //
//                                                                           //
// Use of this source code is governed by a BSD-style license that can be    //
// found in the LICENSE.txt file at the root of this project, or at the      //
// following link: https://opensource.org/licenses/BSD-3-Clause              //
//                                                                           //
//======---------------------------------------------------------------======//

pub trait Arithmetic6502
where
    Self: Sized,
{
    fn add_with_carry(self, rhs: Self, carry: bool) -> (Self, bool);

    fn sub_with_borrow(self, rhs: Self, borrow: bool) -> (Self, bool);

    fn sub_with_carry(self, rhs: Self, carry: bool) -> (Self, bool) {
        self.sub_with_borrow(rhs, !carry)
    }
}

impl Arithmetic6502 for u8 {
    fn add_with_carry(self, rhs: Self, carry: bool) -> (Self, bool) {
        let (a, b) = self.overflowing_add(rhs);
        let (c, d) = a.overflowing_add(carry as Self);

        (c, b != d)
    }

    fn sub_with_borrow(self, rhs: Self, borrow: bool) -> (Self, bool) {
        let (a, b) = self.overflowing_sub(rhs);
        let (c, d) = a.overflowing_sub(borrow as Self);

        (c, b != d)
    }
}

#[cfg(test)]
mod tests {
    use crate::numeric::Arithmetic6502;

    #[test]
    fn carrying_add() {
        assert_eq!(5u8.add_with_carry(2, false), (7, false));
        assert_eq!(5u8.add_with_carry(2, true), (8, false));
        assert_eq!(u8::MAX.add_with_carry(1, false), (0, true));
        assert_eq!(u8::MAX.add_with_carry(0, true), (0, true));
        assert_eq!(u8::MAX.add_with_carry(1, true), (1, true));
        assert_eq!(u8::MAX.add_with_carry(u8::MAX, true), (u8::MAX, true));
        assert_eq!(5_u8.add_with_carry(2, false), 5_u8.overflowing_add(2));
        assert_eq!(u8::MAX.add_with_carry(1, false), u8::MAX.overflowing_add(1));
    }

    #[test]
    fn borrowing_sub() {
        assert_eq!(5u8.sub_with_borrow(2, false), (3, false));
        assert_eq!(5u8.sub_with_borrow(2, true), (2, false));
        assert_eq!(0u8.sub_with_borrow(1, false), (u8::MAX, true));
        assert_eq!(0u8.sub_with_borrow(1, true), (u8::MAX - 1, true));
    }
}
