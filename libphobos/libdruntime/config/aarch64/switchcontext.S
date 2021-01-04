/* AArch64 support code for fibers and multithreading.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.

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
 * preserve/restore AAPCS64 registers
 *   x19-x28 5.1.1 64-bit callee saved
 *   x29 fp, or possibly callee saved reg - depends on platform choice 5.2.3)
 *   x30 lr
 *   d8-d15  5.1.2 says callee only must save bottom 64-bits (the "d" regs)
 *
 * saved regs on stack will look like:
 *   19: x19
 *   18: x20
 *   ...
 *   10: x28
 *    9: x29 (fp)  <-- oldp / *newp save stack top
 *    8: x30 (lr)
 *    7: d8
 *   ...
 *    0: d15       <-- sp
 */
    .text
    .global CSYM(fiber_switchContext)
    .type CSYM(fiber_switchContext), %function
    .align 4
CSYM(fiber_switchContext):
    .cfi_startproc
    stp     d15, d14, [sp, #-20*8]!
    stp     d13, d12, [sp, #2*8]
    stp     d11, d10, [sp, #4*8]
    stp     d9, d8,   [sp, #6*8]
    stp     x30, x29, [sp, #8*8] // lr, fp
    stp     x28, x27, [sp, #10*8]
    stp     x26, x25, [sp, #12*8]
    stp     x24, x23, [sp, #14*8]
    stp     x22, x21, [sp, #16*8]
    stp     x20, x19, [sp, #18*8]

    // oldp is set above saved lr (x30) to hide it and float regs
    // from GC
    add     x19, sp, #9*8
    str     x19, [x0]       // *oldp tstack
    sub     sp, x1, #9*8    // switch to newp sp

    ldp     x20, x19, [sp, #18*8]
    ldp     x22, x21, [sp, #16*8]
    ldp     x24, x23, [sp, #14*8]
    ldp     x26, x25, [sp, #12*8]
    ldp     x28, x27, [sp, #10*8]
    ldp     x30, x29, [sp, #8*8] // lr, fp
    ldp     d9, d8,   [sp, #6*8]
    ldp     d11, d10, [sp, #4*8]
    ldp     d13, d12, [sp, #2*8]
    ldp     d15, d14, [sp], #20*8
    ret
    .cfi_endproc
    .size CSYM(fiber_switchContext),.-CSYM(fiber_switchContext)

/**
 * When generating any kind of backtrace (gdb, exception handling) for
 * a function called in a Fiber, we need to tell the unwinder to stop
 * at our Fiber main entry point, i.e. we need to mark the bottom of
 * the call stack. This can be done by clearing the link register lr
 * prior to calling fiber_entryPoint (i.e. in fiber_switchContext) or
 * using a .cfi_undefined directive for the link register in the
 * Fiber entry point. cfi_undefined seems to yield better results in gdb.
 * Unfortunately we can't place it into fiber_entryPoint using inline
 * asm, so we use this trampoline instead.
 */
    .text
    .global CSYM(fiber_trampoline)
    .p2align  2
    .type CSYM(fiber_trampoline), %function
CSYM(fiber_trampoline):
    .cfi_startproc
    .cfi_undefined x30
    // fiber_entryPoint never returns
    bl CSYM(fiber_entryPoint)
    .cfi_endproc
    .size CSYM(fiber_trampoline),.-CSYM(fiber_trampoline)
