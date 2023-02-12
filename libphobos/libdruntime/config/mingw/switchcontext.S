/* Windows i386 support code for fibers and multithreading.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

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

#if defined(__x86_64__)

    .text
    .globl CSYM(fiber_switchContext)
    .def CSYM(fiber_switchContext)
    .scl 2
    .type 32
    .endef
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
    pushq %RBP;
    movq %RSP, %RBP;
    pushq %RBX;
    pushq %R12;
    pushq %R13;
    pushq %R14;
    pushq %R15;
    pushq %GS:0;
    pushq %GS:8;
    pushq %GS:16;

    // store oldp
    movq %RSP, (%RCX);
    // load newp to begin context switch
    movq %RDX, %RSP;

    // load saved state from new stack
    popq %GS:16;
    popq %GS:8;
    popq %GS:0;
    popq %R15;
    popq %R14;
    popq %R13;
    popq %R12;
    popq %RBX;
    popq %RBP;

    // 'return' to complete switch
    popq %RCX;
    jmp *%RCX;
    .cfi_endproc

#elif defined(_X86_)

    .text
    .globl CSYM(fiber_switchContext)
    .def CSYM(fiber_switchContext)
    .scl 2
    .type 32
    .endef
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
    // Save current stack state.save current stack state
    // Standard CDECL prologue.
    push %EBP;
    mov  %ESP, %EBP;
    push %EDI;
    push %ESI;
    push %EBX;
    push %FS:0;
    push %FS:4;
    push %FS:8;
    push %EAX;

    // store oldp again with more accurate address
    mov 8(%EBP), %EAX;
    mov %ESP, (%EAX);
    // load newp to begin context switch
    mov 12(%EBP), %ESP;

    // load saved state from new stack
    pop %EAX;
    pop %FS:8;
    pop %FS:4;
    pop %FS:0;
    pop %EBX;
    pop %ESI;
    pop %EDI;
    pop %EBP;

    // 'return' to complete switch
    ret;
    .cfi_endproc

#endif
