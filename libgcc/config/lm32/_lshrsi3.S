# _lshrsi3.S for Lattice Mico32 
# Contributed by Jon Beniston <jon@beniston.com> and Richard Henderson.
#
# Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

/* Logical right shift.  */
        
        .global __lshrsi3
        .type __lshrsi3,@function

__lshrsi3:
        /* Only use 5 LSBs, as that's all the h/w shifter uses.  */
        andi    r2, r2, 0x1f
        /* Get address of offset into unrolled shift loop to jump to.  */
#ifdef __PIC__
        lw      r3, (gp+got(__lshrsi3_0))
#else
        mvhi    r3, hi(__lshrsi3_0)
        ori     r3, r3, lo(__lshrsi3_0)
#endif
        add     r2, r2, r2
        add     r2, r2, r2
        sub     r3, r3, r2
        b       r3        
        
__lshrsi3_31:        
        srui    r1, r1, 1
__lshrsi3_30:        
        srui    r1, r1, 1
__lshrsi3_29:        
        srui    r1, r1, 1
__lshrsi3_28:        
        srui    r1, r1, 1
__lshrsi3_27:        
        srui    r1, r1, 1
__lshrsi3_26:        
        srui    r1, r1, 1
__lshrsi3_25:        
        srui    r1, r1, 1
__lshrsi3_24:        
        srui    r1, r1, 1
__lshrsi3_23:        
        srui    r1, r1, 1
__lshrsi3_22:        
        srui    r1, r1, 1
__lshrsi3_21:        
        srui    r1, r1, 1
__lshrsi3_20:        
        srui    r1, r1, 1
__lshrsi3_19:        
        srui    r1, r1, 1
__lshrsi3_18:        
        srui    r1, r1, 1
__lshrsi3_17:        
        srui    r1, r1, 1
__lshrsi3_16:        
        srui    r1, r1, 1
__lshrsi3_15:        
        srui    r1, r1, 1
__lshrsi3_14:        
        srui    r1, r1, 1
__lshrsi3_13:        
        srui    r1, r1, 1
__lshrsi3_12:        
        srui    r1, r1, 1
__lshrsi3_11:        
        srui    r1, r1, 1
__lshrsi3_10:        
        srui    r1, r1, 1
__lshrsi3_9:        
        srui    r1, r1, 1
__lshrsi3_8:        
        srui    r1, r1, 1
__lshrsi3_7:        
        srui    r1, r1, 1
__lshrsi3_6:        
        srui    r1, r1, 1
__lshrsi3_5:        
        srui    r1, r1, 1
__lshrsi3_4:        
        srui    r1, r1, 1
__lshrsi3_3:        
        srui    r1, r1, 1
__lshrsi3_2:        
        srui    r1, r1, 1
__lshrsi3_1:        
        srui    r1, r1, 1
__lshrsi3_0:        
        ret
