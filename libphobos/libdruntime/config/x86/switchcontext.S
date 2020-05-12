/* i386 support code for fibers and multithreading.
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

#ifdef __CET__
# include <cet.h>
#else
# define _CET_ENDBR
#endif

#if defined(__i386__)

    .text
    .globl CSYM(fiber_switchContext)
    .type CSYM(fiber_switchContext), @function
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
    _CET_ENDBR
    // save current stack state
    push %ebp
    mov  %esp, %ebp
    push %edi
    push %esi
    push %ebx
    push %eax

    // store oldp again with more accurate address
    mov 8(%ebp), %eax
    mov %esp, (%eax)
    // load newp to begin context switch
    mov 12(%ebp), %esp

    // load saved state from new stack
    pop %eax
    pop %ebx
    pop %esi
    pop %edi
    pop %ebp

    // 'return' to complete switch
    ret
    .cfi_endproc
    .size CSYM(fiber_switchContext),.-CSYM(fiber_switchContext)

#elif defined(__x86_64__) && !defined(__ILP32__)

    .text
    .globl CSYM(fiber_switchContext)
    .type CSYM(fiber_switchContext), @function
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
    _CET_ENDBR
    // Save current stack state.save current stack state
    push %rbp
    mov  %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15

    // store oldp again with more accurate address
    mov %rsp, (%rdi)
    // load newp to begin context switch
    mov %rsi, %rsp

    // load saved state from new stack
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp

    // 'return' to complete switch
    ret
    .cfi_endproc
   .size CSYM(fiber_switchContext),.-CSYM(fiber_switchContext)

#endif
