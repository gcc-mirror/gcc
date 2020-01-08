# crti.S for Sunplus S+CORE
#
#   Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

# This file makes a stack frame for the contents of the .init and
# .fini sections.
.extern _stack

#ifndef __pic__
.section .init, "ax", @progbits
        .weak   _start
        .ent    _start
        .frame  r0, 0, r3, 0
        .mask   0x00000000, 0
_start:
        la      r28, _gp
        la      r8, __bss_start
        la      r9, __bss_end__
        sub!    r9, r8
        srli!   r9, 2
        addi    r9, -1
        mtsr    r9, sr0
        li      r9, 0
1:
        sw      r9, [r8]+, 4
        bcnz    1b
        la      r0, _stack
        jl      _init
        la      r4, _end
        jl      _init_argv
        jl      exit
        .end    _start

        .weak   _init_argv
        .ent
        .frame  r0, 0, r3, 0
        .mask   0x00000000, 0
_init_argv:
        ldiu!   r4, 0
        ldiu!   r5, 0
        j       main
        .end    _init_argv

        .globl  _init
        .type   _init, %function
_init:
        addi    r0, -32
        sw      r3, [r0, 20]

        .section .fini, "ax", @progbits
        .globl  _fini
        .type   _fini, %function
_fini:
        addi    r0, -32
        sw      r3, [r0, 20]
#else
.section .init, "ax", @progbits
        .set    pic
        .weak   _start
        .ent    _start
        .frame  r0, 0, r3, 0
        .mask   0x00000000, 0
_start:
        mv      r29, r3
        bl      0f
0:
        .cpload r3
        mv      r3, r29
        la      r8, __bss_start
        la      r9, __bss_end__
        sub!    r9, r8
        srli!   r9, 2
        addi    r9, -1
        mtsr    r9, sr0
        li      r9, 0
1:
        sw      r9, [r8]+, 4
        bcnz    1b
        la      r0, _stack
        bl      _init
        la      r4, _end
        la      r29, _init_argv
        brl     r29
        la      r29, exit
        brl     r29
        .end    _start

        .weak   _init_argv
        .ent _init_argv
        .frame  r0, 0, r3, 0
        .mask   0x00000000, 0
_init_argv:
        ldiu!   r4, 0
        ldiu!   r5, 0
        la      r29, main
        brl     r29
        .end    _init_argv

        .globl  _init
        .type   _init, %function
_init:
        addi    r0, -32
        sw      r3, [r0, 20]

        .section .fini, "ax", @progbits
        .globl  _fini
        .type   _fini, %function
_fini:
        addi    r0, -32
        sw      r3, [r0, 20]

#endif
