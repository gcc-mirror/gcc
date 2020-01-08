/* PowerPC64 support code for fibers and multithreading.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "../common/threadasm.S"

#if defined(_CALL_ELF) && _CALL_ELF == 2
#define USE_ABI_2
#define LINKAGE_SZ   32
#define LR_OFS       16
#define TOC_OFS      24
#define GPR_OFS      32
#define STACK_SZ     (LINKAGE_SZ + 26*8)
#define OFS_R3_R10   GPR_OFS
#define OFS_R14_R31  (GPR_OFS+8*8)
#else
#define LINKAGE_SZ   48
#define LR_OFS       16
#define TOC_OFS      40
#define GPR_OFS      112
#define STACK_SZ     (LINKAGE_SZ + 8*8 + 18*8)
#define OFS_R3_R10   (STACK_SZ+LINKAGE_SZ)
#define OFS_R14_R31  GPR_OFS
#endif

    .text
#if defined( USE_ABI_2 )
    .abiversion 2
#endif
    .globl  _D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .align  2
    .type   _D4core6thread18callWithStackShellFNbMDFNbPvZvZv,@function
#if defined( USE_ABI_2 )
    .section .text._D4core6thread18callWithStackShellFNbMDFNbPvZvZv,"a",@progbits
#else
    .section .opd,"aw",@progbits
#endif
_D4core6thread18callWithStackShellFNbMDFNbPvZvZv:
#if !defined( USE_ABI_2 )
    .align  3
    .quad .L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .quad .TOC.@tocbase
    .quad 0
#endif
    .text
/*
 * Called with:
 * r3: pointer context
 * r4: pointer to function
 */
.L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv:
    .cfi_startproc
    stdu    1, -STACK_SZ(1)
    mflr    0
    std     0, LR_OFS(1)
    .cfi_def_cfa_offset 256
    .cfi_offset lr, 16

    /* Save r14-r31 in general register save area */
    std     14, (OFS_R14_R31 + 0 * 8)(1)
    std     15, (OFS_R14_R31 + 1 * 8)(1)
    std     16, (OFS_R14_R31 + 2 * 8)(1)
    std     17, (OFS_R14_R31 + 3 * 8)(1)
    std     18, (OFS_R14_R31 + 4 * 8)(1)
    std     19, (OFS_R14_R31 + 5 * 8)(1)
    std     20, (OFS_R14_R31 + 6 * 8)(1)
    std     21, (OFS_R14_R31 + 7 * 8)(1)
    std     22, (OFS_R14_R31 + 8 * 8)(1)
    std     23, (OFS_R14_R31 + 9 * 8)(1)
    std     24, (OFS_R14_R31 + 10 * 8)(1)
    std     25, (OFS_R14_R31 + 11 * 8)(1)
    std     26, (OFS_R14_R31 + 12 * 8)(1)
    std     27, (OFS_R14_R31 + 13 * 8)(1)
    std     28, (OFS_R14_R31 + 14 * 8)(1)
    std     29, (OFS_R14_R31 + 15 * 8)(1)
    std     30, (OFS_R14_R31 + 16 * 8)(1)
    std     31, (OFS_R14_R31 + 17 * 8)(1)

    /* Save r3-r10 in parameter save area of caller */
    std     3, (OFS_R3_R10 + 0 * 8)(1)
    std     4, (OFS_R3_R10 + 1 * 8)(1)
    std     5, (OFS_R3_R10 + 2 * 8)(1)
    std     6, (OFS_R3_R10 + 3 * 8)(1)
    std     7, (OFS_R3_R10 + 4 * 8)(1)
    std     8, (OFS_R3_R10 + 5 * 8)(1)
    std     9, (OFS_R3_R10 + 6 * 8)(1)
    std     10, (OFS_R3_R10 + 7 * 8)(1)

    /* Save r2 in TOC save area */
    std     2, TOC_OFS(1)

    /* Do not save r11, r12 and r13. */

    /* Call delegate:
     * r3: pointer to context
     * r4: pointer to stack
     */
    mr      5, 4
    mr      4, 1
    ld      6, 0(5)
    ld      11, 16(5)
    ld      2, 8(5)
    mtctr   6
    bctrl
    nop

    /* Restore r2 from TOC save area */
    ld      2, TOC_OFS(1)

    /* Restore r3-r10 from local variable space */
    ld      3, (OFS_R3_R10 + 0 * 8)(1)
    ld      4, (OFS_R3_R10 + 1 * 8)(1)
    ld      5, (OFS_R3_R10 + 2 * 8)(1)
    ld      6, (OFS_R3_R10 + 3 * 8)(1)
    ld      7, (OFS_R3_R10 + 4 * 8)(1)
    ld      8, (OFS_R3_R10 + 5 * 8)(1)
    ld      9, (OFS_R3_R10 + 6 * 8)(1)
    ld      10, (OFS_R3_R10 + 7 * 8)(1)

    /* Restore r14-r31 from general register save area */
    ld      14, (OFS_R14_R31 + 0 * 8)(1)
    ld      15, (OFS_R14_R31 + 1 * 8)(1)
    ld      16, (OFS_R14_R31 + 2 * 8)(1)
    ld      17, (OFS_R14_R31 + 3 * 8)(1)
    ld      18, (OFS_R14_R31 + 4 * 8)(1)
    ld      19, (OFS_R14_R31 + 5 * 8)(1)
    ld      20, (OFS_R14_R31 + 6 * 8)(1)
    ld      21, (OFS_R14_R31 + 7 * 8)(1)
    ld      22, (OFS_R14_R31 + 8 * 8)(1)
    ld      23, (OFS_R14_R31 + 9 * 8)(1)
    ld      24, (OFS_R14_R31 + 10 * 8)(1)
    ld      25, (OFS_R14_R31 + 11 * 8)(1)
    ld      26, (OFS_R14_R31 + 12 * 8)(1)
    ld      27, (OFS_R14_R31 + 13 * 8)(1)
    ld      28, (OFS_R14_R31 + 14 * 8)(1)
    ld      29, (OFS_R14_R31 + 15 * 8)(1)
    ld      30, (OFS_R14_R31 + 16 * 8)(1)
    ld      31, (OFS_R14_R31 + 17 * 8)(1)

    ld      0, LR_OFS(1)
    mtlr    0
    addi    1, 1, STACK_SZ
    blr
    .long 0
    .quad 0
.Lend:
    .size _D4core6thread18callWithStackShellFNbMDFNbPvZvZv, .Lend-.L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .cfi_endproc
