/* LoongArch support code for fibers and multithreading.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

/**
 * Performs a context switch.
 *
 * $a0 - void** - ptr to old stack pointer
 * $a1 - void*  - new stack pointer
 *
 */

#if defined(__loongarch_lp64)
#  define GPR_L ld.d
#  define GPR_S st.d
#  define SZ_GPR 8
#  define ADDSP(si)   addi.d  $sp, $sp, si
#elif defined(__loongarch64_ilp32)
#  define GPR_L ld.w
#  define GPR_S st.w
#  define SZ_GPR 4
#  define ADDSP(si)   addi.w  $sp, $sp, si
#else
#  error Unsupported GPR size (must be 64-bit or 32-bit).
#endif

#if defined(__loongarch_double_float)
#  define FPR_L fld.d
#  define FPR_S fst.d
#  define SZ_FPR 8
#elif defined(__loongarch_single_float)
#  define FPR_L fld.s
#  define FPR_S fst.s
#  define SZ_FPR 4
#else
#  define SZ_FPR 0
#endif

    .text
    .align 2
    .global fiber_switchContext
    .type   fiber_switchContext, @function
fiber_switchContext:
    .cfi_startproc
    ADDSP(-11 * SZ_GPR)

    // fp regs and return address are stored below the stack
    // because we don't want the GC to scan them.

    // return address (r1)
    GPR_S  $r1, $sp, -SZ_GPR

#if SZ_FPR != 0
    // callee-saved scratch FPRs (f24-f31)
    FPR_S  $f24, $sp, -SZ_GPR-1*SZ_FPR
    FPR_S  $f25, $sp, -SZ_GPR-2*SZ_FPR
    FPR_S  $f26, $sp, -SZ_GPR-3*SZ_FPR
    FPR_S  $f27, $sp, -SZ_GPR-4*SZ_FPR
    FPR_S  $f28, $sp, -SZ_GPR-5*SZ_FPR
    FPR_S  $f29, $sp, -SZ_GPR-6*SZ_FPR
    FPR_S  $f30, $sp, -SZ_GPR-7*SZ_FPR
    FPR_S  $f31, $sp, -SZ_GPR-8*SZ_FPR
#endif

    // callee-saved GPRs (r21, fp (r22), r23-r31)
    GPR_S $r21, $sp, 0*SZ_GPR
    GPR_S  $fp, $sp, 1*SZ_GPR
    GPR_S  $s0, $sp, 2*SZ_GPR
    GPR_S  $s1, $sp, 3*SZ_GPR
    GPR_S  $s2, $sp, 4*SZ_GPR
    GPR_S  $s3, $sp, 5*SZ_GPR
    GPR_S  $s4, $sp, 6*SZ_GPR
    GPR_S  $s5, $sp, 7*SZ_GPR
    GPR_S  $s6, $sp, 8*SZ_GPR
    GPR_S  $s7, $sp, 9*SZ_GPR
    GPR_S  $s8, $sp, 10*SZ_GPR

    // swap stack pointer
    GPR_S $sp, $a0, 0
    move $sp, $a1

    GPR_L  $r1, $sp, -SZ_GPR

#if SZ_FPR != 0
    FPR_L  $f24, $sp, -SZ_GPR-1*SZ_FPR
    FPR_L  $f25, $sp, -SZ_GPR-2*SZ_FPR
    FPR_L  $f26, $sp, -SZ_GPR-3*SZ_FPR
    FPR_L  $f27, $sp, -SZ_GPR-4*SZ_FPR
    FPR_L  $f28, $sp, -SZ_GPR-5*SZ_FPR
    FPR_L  $f29, $sp, -SZ_GPR-6*SZ_FPR
    FPR_L  $f30, $sp, -SZ_GPR-7*SZ_FPR
    FPR_L  $f31, $sp, -SZ_GPR-8*SZ_FPR
#endif

    GPR_L $r21, $sp, 0*SZ_GPR
    GPR_L  $fp, $sp, 1*SZ_GPR
    GPR_L  $s0, $sp, 2*SZ_GPR
    GPR_L  $s1, $sp, 3*SZ_GPR
    GPR_L  $s2, $sp, 4*SZ_GPR
    GPR_L  $s3, $sp, 5*SZ_GPR
    GPR_L  $s4, $sp, 6*SZ_GPR
    GPR_L  $s5, $sp, 7*SZ_GPR
    GPR_L  $s6, $sp, 8*SZ_GPR
    GPR_L  $s7, $sp, 9*SZ_GPR
    GPR_L  $s8, $sp, 10*SZ_GPR

    ADDSP(11 * SZ_GPR)

    jr     $r1 // return
    .cfi_endproc
    .size fiber_switchContext,.-fiber_switchContext
