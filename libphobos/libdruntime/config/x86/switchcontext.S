/* i386 support code for fibers and multithreading.
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

/* NB: Generate the CET marker for -fcf-protection.  */
#ifdef __CET__
# include <cet.h>
#endif

#if !defined(__CET__)

# if defined(__ELF__)

#  if defined(__i386__)

    .text
    .globl CSYM(fiber_switchContext)
    .type CSYM(fiber_switchContext), @function
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
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

#  endif /* defined(__ELF__) && defined(__i386__) */

#  if defined(__x86_64__) && !defined(__ILP32__)

    .text
    .globl CSYM(fiber_switchContext)
    .type CSYM(fiber_switchContext), @function
    .align 16
CSYM(fiber_switchContext):
    .cfi_startproc
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

#  endif /* defined(__ELF__) && defined(__x86_64__) && !defined(__ILP32__) */

# endif /* defined(__ELF__) */

# if defined(__MACH__)

#  if defined(__i386__)

    .text
    .globl CSYM(fiber_switchContext)
    .p2align 4
CSYM(fiber_switchContext):
LFB0:
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
LFE0:

/* CFI */
        .section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
        .set L$set$0,LECIE1-LSCIE1
        .long L$set$0   # Length of Common Information Entry
LSCIE1:
        .long   0       # CIE Identifier Tag
        .byte   0x1     # CIE Version
        .ascii "zR\0"   # CIE Augmentation
        .byte   0x1     # uleb128 0x1; CIE Code Alignment Factor
        .byte   0x7c    # sleb128 -4; CIE Data Alignment Factor
        .byte   0x8     # CIE RA Column
        .byte   0x1     # uleb128 0x1; Augmentation size
        .byte   0x10    # FDE Encoding (pcrel)
        .byte   0xc     # DW_CFA_def_cfa
        .byte   0x5     # uleb128 0x5
        .byte   0x4     # uleb128 0x4
        .byte   0x88    # DW_CFA_offset, column 0x8
        .byte   0x1     # uleb128 0x1
        .p2align 2,0
LECIE1:

/* minimal FDE - does not record the stack frame changes. */
LSFDE1:
        .set L$set$1,LEFDE1-LASFDE1
        .long L$set$1   # FDE Length
LASFDE1:
        .long   LASFDE1-EH_frame1       # FDE CIE offset
        .long   LFB0-.  # FDE initial location
        .set L$set$2,LFE0-LFB0
        .long L$set$2   # FDE address range
        .byte   0       # uleb128 0; Augmentation size
        .p2align 2,0
LEFDE1:

#  endif /* defined(__MACH__) && defined(__i386__) */

#  if defined(__x86_64__) && !defined(__ILP32__)

    .text
    .globl CSYM(fiber_switchContext)
    .p2align 4
CSYM(fiber_switchContext):
LFB0:
    // Save current stack state.save current stack state
    push %rbp
    mov  %rsp, %rbp
    push %r15
    push %r14
    push %r13
    push %r12
    push %rbx

    // store oldp again with more accurate address
    mov %rsp, (%rdi)
    // load newp to begin context switch
    mov %rsi, %rsp

    // load saved state from new stack
    pop %rbx
    pop %r12
    pop %r13
    pop %r14
    pop %r15
    pop %rbp

    // 'return' to complete switch
    ret
LFE0:

/* CFI */
        .section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
        .set L$set$0,LECIE1-LSCIE1
        .long L$set$0   # Length of Common Information Entry
LSCIE1:
        .long   0       # CIE Identifier Tag
        .byte   0x1     # CIE Version
        .ascii "zR\0"   # CIE Augmentation
        .byte   0x1     # uleb128 0x1; CIE Code Alignment Factor
        .byte   0x78    # sleb128 -8; CIE Data Alignment Factor
        .byte   0x10    # CIE RA Column
        .byte   0x1     # uleb128 0x1; Augmentation size
        .byte   0x10    # FDE Encoding (pcrel)
        .byte   0xc     # DW_CFA_def_cfa
        .byte   0x7     # uleb128 0x7
        .byte   0x8     # uleb128 0x8
        .byte   0x90    # DW_CFA_offset, column 0x10
        .byte   0x1     # uleb128 0x1
        .p2align 3,0
LECIE1:

/* minimal FDE - does not record the stack frame changes. */
LSFDE1:
        .set L$set$1,LEFDE1-LASFDE1
        .long L$set$1   # FDE Length
LASFDE1:
        .long   LASFDE1-EH_frame1       # FDE CIE offset
        .quad   LFB0-.  # FDE initial location
        .set L$set$2,LFE0-LFB0
        .quad L$set$2   # FDE address range
        .byte   0       # uleb128 0; Augmentation size
        .p2align 3,0
LEFDE1:

#  endif /* defined(__MACH__) && defined(__x86_64__) && !defined(__ILP32__) */

# endif /* defined (__MACH__) */

#endif /* !defined(__CET__) */
