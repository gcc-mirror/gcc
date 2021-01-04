/* crti.s for __init, __fini
   This file supplies the prologue for __init and __fini routines 

   Copyright (C) 2009-2021 Free Software Foundation, Inc.

   Contributed by Michael Eager <eager@eagercon.com>.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.
 
   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* An executable stack is *not* required for these functions.  */
#ifdef __linux__
.section .note.GNU-stack,"",%progbits
.previous
#endif

    .section .init, "ax"
    .global __init

    .weak _stack
    .set  _stack, 0xffffffff
    .weak _stack_end
    .set  _stack_end, 0

    .align 2
__init: 
    addik   r1, r1, -8
    sw      r15, r0, r1
    la      r11, r0, _stack
    mts     rshr, r11
    la      r11, r0, _stack_end
    mts     rslr, r11

    .section .fini, "ax"
    .global __fini
    .align 2
__fini: 
    addik   r1, r1, -8
    sw      r15, r0, r1
