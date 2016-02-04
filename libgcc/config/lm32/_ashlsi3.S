# _ashlsi3.S for Lattice Mico32 
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

/* Arithmetic left shift.  */
        
        .text

        .global __ashlsi3
        .type __ashlsi3,@function

        .align 4        
__ashlsi3:
        /* Only use 5 LSBs, as that's all the h/w shifter uses.  */
        andi    r2, r2, 0x1f
        /* Get address of offset into unrolled shift loop to jump to.  */
#ifdef __PIC__
        lw      r3, (gp+got(__ashlsi3_0))
#else
        mvhi    r3, hi(__ashlsi3_0)
        ori     r3, r3, lo(__ashlsi3_0)
#endif
        add     r2, r2, r2
        add     r2, r2, r2
        sub     r3, r3, r2
        b       r3        
        
__ashlsi3_31:
        add     r1, r1, r1
__ashlsi3_30:
        add     r1, r1, r1
__ashlsi3_29:
        add     r1, r1, r1
__ashlsi3_28:
        add     r1, r1, r1
__ashlsi3_27:
        add     r1, r1, r1
__ashlsi3_26:
        add     r1, r1, r1
__ashlsi3_25:
        add     r1, r1, r1
__ashlsi3_24:
        add     r1, r1, r1
__ashlsi3_23:
        add     r1, r1, r1
__ashlsi3_22:
        add     r1, r1, r1
__ashlsi3_21:
        add     r1, r1, r1
__ashlsi3_20:
        add     r1, r1, r1
__ashlsi3_19:
        add     r1, r1, r1
__ashlsi3_18:
        add     r1, r1, r1
__ashlsi3_17:
        add     r1, r1, r1
__ashlsi3_16:
        add     r1, r1, r1
__ashlsi3_15:
        add     r1, r1, r1
__ashlsi3_14:
        add     r1, r1, r1
__ashlsi3_13:
        add     r1, r1, r1
__ashlsi3_12:
        add     r1, r1, r1
__ashlsi3_11:
        add     r1, r1, r1
__ashlsi3_10:
        add     r1, r1, r1
__ashlsi3_9:
        add     r1, r1, r1
__ashlsi3_8:
        add     r1, r1, r1
__ashlsi3_7:
        add     r1, r1, r1
__ashlsi3_6:
        add     r1, r1, r1
__ashlsi3_5:
        add     r1, r1, r1
__ashlsi3_4:
        add     r1, r1, r1
__ashlsi3_3:
        add     r1, r1, r1
__ashlsi3_2:
        add     r1, r1, r1
__ashlsi3_1:
        add     r1, r1, r1
__ashlsi3_0:
        ret
                
        