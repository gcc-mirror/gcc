# _ashrsi3.S for Lattice Mico32 
# Contributed by Jon Beniston <jon@beniston.com> and Richard Henderson.
#
# Copyright (C) 2009-2016 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.
# 

/* Arithmetic right shift.  */

        .global __ashrsi3
        .type __ashrsi3,@function
                                                  
__ashrsi3:
        /* Only use 5 LSBs, as that's all the h/w shifter uses.  */
        andi    r2, r2, 0x1f
        /* Get address of offset into unrolled shift loop to jump to.  */
#ifdef __PIC__
        lw      r3, (gp+got(__ashrsi3_0))
#else
        mvhi    r3, hi(__ashrsi3_0)
        ori     r3, r3, lo(__ashrsi3_0)
#endif
        add     r2, r2, r2
        add     r2, r2, r2
        sub     r3, r3, r2
        b       r3        
        
__ashrsi3_31:
        sri     r1, r1, 1
__ashrsi3_30:
        sri     r1, r1, 1
__ashrsi3_29:
        sri     r1, r1, 1
__ashrsi3_28:
        sri     r1, r1, 1
__ashrsi3_27:
        sri     r1, r1, 1
__ashrsi3_26:
        sri     r1, r1, 1
__ashrsi3_25:
        sri     r1, r1, 1
__ashrsi3_24:
        sri     r1, r1, 1
__ashrsi3_23:
        sri     r1, r1, 1
__ashrsi3_22:
        sri     r1, r1, 1
__ashrsi3_21:
        sri     r1, r1, 1
__ashrsi3_20:
        sri     r1, r1, 1
__ashrsi3_19:
        sri     r1, r1, 1
__ashrsi3_18:
        sri     r1, r1, 1
__ashrsi3_17:
        sri     r1, r1, 1
__ashrsi3_16:
        sri     r1, r1, 1
__ashrsi3_15:
        sri     r1, r1, 1
__ashrsi3_14:
        sri     r1, r1, 1
__ashrsi3_13:
        sri     r1, r1, 1
__ashrsi3_12:
        sri     r1, r1, 1
__ashrsi3_11:
        sri     r1, r1, 1
__ashrsi3_10:
        sri     r1, r1, 1
__ashrsi3_9:
        sri     r1, r1, 1
__ashrsi3_8:
        sri     r1, r1, 1
__ashrsi3_7:
        sri     r1, r1, 1
__ashrsi3_6:
        sri     r1, r1, 1
__ashrsi3_5:
        sri     r1, r1, 1
__ashrsi3_4:
        sri     r1, r1, 1
__ashrsi3_3:
        sri     r1, r1, 1
__ashrsi3_2:
        sri     r1, r1, 1
__ashrsi3_1:
        sri     r1, r1, 1
__ashrsi3_0:
        ret
        