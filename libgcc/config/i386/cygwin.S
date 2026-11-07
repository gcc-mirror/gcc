/* stuff needed for libgcc on win32.
 *
 *   Copyright (C) 1996-2026 Free Software Foundation, Inc.
 *   Written By Steve Chamberlain
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 * 
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "i386-asm.h"

#ifdef HAVE_AS_CFI_SECTIONS
	.cfi_sections	.debug_frame
#endif

/* Regarding call-frame info notes: 32bits Windows ports are either DWARF2 or
   sjlj-eh, with .cfi notes emitted or not, respectively, through calls to
   the cfi_ macros exposed by i386-asm.h.  64bits Windows ports are either SEH
   or sjlj-eh, with .seh directives emitted directly here, as they are harmless
   for sjlj-eh and here is the only file where they are of potential use, for
   example to help backtraces on memory faults triggered by the stack checking
   probes.  */

#ifdef L_chkstk
/* Function prologue calls __chkstk to probe the stack when allocating more
   than CHECK_STACK_LIMIT bytes in one go.  Touching the stack at 4K
   increments is necessary to ensure that the guard pages used
   by the OS virtual memory manger are allocated in correct sequence.  */

	.global ___chkstk
	.global	__alloca
#ifdef __x86_64__
/* __alloca is a normal function call, which uses %rcx as the argument.  */
	.seh_proc __alloca
__alloca:
	/* No own frame, no non-volatile register push;  No prologue as
	   far as SEH is concerned.  */
	.seh_endprologue

	movq	%rcx, %rax
	/* FALLTHRU */

/* ___chkstk is a *special* function call, which uses %rax as the argument.
   We avoid clobbering the 4 integer argument registers, %rcx, %rdx, 
   %r8 and %r9, which leaves us with %rax, %r10, and %r11 to use.  */
	.align	4
___chkstk:
	popq	%r11			/* pop return address; .seh? */
	movq	%rsp, %r10
	cmpq	$0x1000, %rax		/* > 4k ?*/
	jb	2f

1:	subq	$0x1000, %r10  		/* yes, move pointer down 4k*/
	orl	$0x0, (%r10)   		/* probe there */
	subq	$0x1000, %rax  	 	/* decrement count */
	cmpq	$0x1000, %rax
	ja	1b			/* and do it again */

2:	subq	%rax, %r10
	movq	%rsp, %rax		/* hold CFA until return; .seh? */
	orl	$0x0, (%r10)		/* less than 4k, just peek here */
	movq	%r10, %rsp		/* decrement stack */

	/* Push the return value back.  Doing this instead of just
	   jumping to %r11 preserves the cached call-return stack
	   used by most modern processors.  */
	pushq	%r11
	ret
	.seh_endproc
#else
	cfi_startproc()
___chkstk:
__alloca:
	pushl	%ecx			/* save temp */
	cfi_push(%ecx)
	leal	8(%esp), %ecx		/* point past return addr */
	cmpl	$0x1000, %eax		/* > 4k ?*/
	jb	2f

1:	subl	$0x1000, %ecx  		/* yes, move pointer down 4k*/
	orl	$0x0, (%ecx)   		/* probe there */
	subl	$0x1000, %eax  	 	/* decrement count */
	cmpl	$0x1000, %eax
	ja	1b			/* and do it again */

2:	subl	%eax, %ecx	   
	orl	$0x0, (%ecx)		/* less than 4k, just peek here */
	movl	%esp, %eax		/* save current stack pointer */
	cfi_def_cfa_register(%eax)
	movl	%ecx, %esp		/* decrement stack */
	movl	(%eax), %ecx		/* recover saved temp */

	/* Copy the return register.  Doing this instead of just jumping to
	   the address preserves the cached call-return stack used by most
	   modern processors.  */
	pushl	4(%eax)
	ret
	cfi_endproc()
#endif /* __x86_64__ */
#endif /* L_chkstk */

#ifdef L_chkstk_ms
/* ___chkstk_ms is a *special* function call, which uses %rax as the argument.
   We avoid clobbering any registers.  Unlike ___chkstk, it just probes the
   stack and does no stack allocation.  */
	.global ___chkstk_ms
#ifdef __x86_64__
	.seh_proc ___chkstk_ms
___chkstk_ms:
	/* prologue, using a code sequence matching the MS documentation
	   of expected patterns to allow SEH through;  No non-volatile reg
	   to push in our case, allocate space (for volatile temps we want
	   to preserve) with a plain sp move.  */
	subq   $16, %rsp
	.seh_stackalloc 16
	.seh_endprologue

	movq   %rax, (%rsp)
	movq   %rcx, 8(%rsp)

	cmpq	$0x1000, %rax		/* > 4k ?*/
	leaq	24(%rsp), %rcx		/* point past return addr */
	jb	2f

1:	subq	$0x1000, %rcx  		/* yes, move pointer down 4k */
	orq	$0x0, (%rcx)   		/* probe there */
	subq	$0x1000, %rax  	 	/* decrement count */
	cmpq	$0x1000, %rax
	ja	1b			/* and do it again */

2:	subq	%rax, %rcx
	orq	$0x0, (%rcx)		/* less than 4k, just peek here */

	movq   8(%rsp), %rcx
	movq   (%rsp), %rax
	addq   $16, %rsp
	ret
	.seh_endproc
#else
	cfi_startproc()
___chkstk_ms:
	pushl	%ecx			/* save temp */
	cfi_push(%ecx)
	pushl	%eax
	cfi_push(%eax)
	cmpl	$0x1000, %eax		/* > 4k ?*/
	leal	12(%esp), %ecx		/* point past return addr */
	jb	2f

1:	subl	$0x1000, %ecx  		/* yes, move pointer down 4k*/
	orl	$0x0, (%ecx)   		/* probe there */
	subl	$0x1000, %eax  	 	/* decrement count */
	cmpl	$0x1000, %eax
	ja	1b			/* and do it again */

2:	subl	%eax, %ecx
	orl	$0x0, (%ecx)		/* less than 4k, just peek here */

	popl	%eax
	cfi_pop(%eax)
	popl	%ecx
	cfi_pop(%ecx)
	ret
	cfi_endproc()
#endif /* __x86_64__ */
#endif /* L_chkstk_ms */
