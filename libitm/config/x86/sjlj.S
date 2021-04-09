/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


#include "asmcfi.h"
#include "config.h"
#include "cet.h"

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

#ifdef __USER_LABEL_PREFIX__
#  define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)
#else
#  define SYM(x) x
#endif

#ifdef __ELF__
#  define TYPE(x) .type SYM(x), @function
#  define SIZE(x) .size SYM(x), . - SYM(x)
#  ifdef HAVE_ATTRIBUTE_VISIBILITY
#    define HIDDEN(x) .hidden SYM(x)
#  else
#    define HIDDEN(x)
#  endif
#else
#  define TYPE(x)
#  define SIZE(x)
#  ifdef __MACH__
#    define HIDDEN(x) .private_extern SYM(x)
#  else
#    define HIDDEN(x)
#  endif
#endif

/* These are duplicates of the canonical definitions in libitm.h.  Note that
   the code relies on pr_uninstrumentedCode == a_runUninstrumentedCode.  */
#define pr_uninstrumentedCode	0x02
#define pr_hasNoAbort		0x08
#define pr_HTMRetryableAbort	0x800000
#define pr_HTMRetriedAfterAbort	0x1000000
#define a_runInstrumentedCode	0x01
#define a_runUninstrumentedCode	0x02
#define a_tryHTMFastPath	0x20

#define _XABORT_EXPLICIT	(1 << 0)
#define _XABORT_RETRY		(1 << 1)

	.text

	.align 4
	.globl	SYM(_ITM_beginTransaction)

SYM(_ITM_beginTransaction):
	cfi_startproc
	_CET_ENDBR
#ifdef __x86_64__
#ifdef HAVE_AS_RTM
	/* Custom HTM fast path.  We start the HW transaction here and let
	   gtm_thread::begin_transaction (aka GTM_begin_transaction) decide
	   how to proceed on aborts: We either retry the fast path, or fall
	   back to another execution method.  RTM restores all registers after
	   a HW transaction abort, so we can do the SW setjmp after aborts,
	   and we have to because we might choose a SW fall back.  However,
	   we have to explicitly save/restore the first argument (edi).
	   The htm_fastpath field is the second int in gtm_rwlock.  */
	cmpl	$0, (SYM(gtm_serial_lock)+4)(%rip)
	jz	.Lno_htm
	testl	$pr_hasNoAbort, %edi
	jz	.Lno_htm
.Lhtm_fastpath:
	xbegin	.Ltxn_abort
	/* Monitor the serial lock (specifically, the 32b writer/summary field
	   at its start), and only continue if there is no serial-mode
	   transaction.  Note that we might be just a nested transaction and
	   our outermost transaction might be in serial mode; we check for
	   this case in the retry policy implementation.  */
	cmpl	$0, SYM(gtm_serial_lock)(%rip)
	jnz	1f
	/* Now also check that HW transactions are still allowed to run (see
	   gtm_thread::begin_transaction for why this is necessary).  */
	cmpl	$0, (SYM(gtm_serial_lock)+4)(%rip)
	jz	1f
	/* Everything is good.  Run the transaction, preferably using the
	   uninstrumented code path.  Note that the following works because
	   pr_uninstrumentedCode == a_runUninstrumentedCode.  */
	andl	$pr_uninstrumentedCode, %edi
	mov	$a_runInstrumentedCode, %eax
	cmovnz	%edi, %eax
	ret
	/* There is a serial-mode transaction or HW transactions are not
	   allowed anymore, so abort (see htm_abort() regarding the abort
	   code).  */
1:	xabort	$0xff
.Ltxn_abort:
	/* If it might make sense to retry the HTM fast path, let the C++
	   code decide.  */
	testl	$(_XABORT_RETRY|_XABORT_EXPLICIT), %eax
	jz	.Lno_htm
	orl	$pr_HTMRetryableAbort, %edi
	/* Let the C++ code handle the retry policy.  */
.Lno_htm:
#endif
	leaq	8(%rsp), %rax
	subq	$72, %rsp
	cfi_adjust_cfa_offset(72)
	/* Store edi for future HTM fast path retries.  We use a stack slot
	   lower than the jmpbuf so that the jmpbuf's rip field will overlap
	   with the proper return address on the stack.  */
	movl	%edi, (%rsp)
	/* Save the jmpbuf for any non-HTM-fastpath execution method.
	   Because rsp-based addressing is 1 byte larger and we've got rax
	   handy, use it.  */
	movq	%rax, -72(%rax)
	movq	%rbx, -64(%rax)
	movq	%rbp, -56(%rax)
	movq	%r12, -48(%rax)
	movq	%r13, -40(%rax)
	movq	%r14, -32(%rax)
	movq	%r15, -24(%rax)
	xorq	%rdx, %rdx
	/* Save zero or shadow stack pointer in the new field.  */
#if defined __SHSTK__ && defined __CET__ && (__CET__ & 2) != 0
	rdsspq	%rdx
#endif
	movq	%rdx, -16(%rax)
	leaq	-72(%rax), %rsi
	call	SYM(GTM_begin_transaction)
	movl	(%rsp), %edi
	addq	$72, %rsp
	cfi_adjust_cfa_offset(-72)
#ifdef HAVE_AS_RTM
	/* If a_tryHTMFastPath was returned, then we need to retry the
	   fast path.  We also restore edi and set pr_HTMRetriedAfterAbort
	   to state that we have retried the fast path already (it's harmless
	   if this bit is set even if we don't retry the fast path because it
	   is checked iff pr_HTMRetryableAbort is set).  We clear
	   pr_HTMRetryableAbort because it applies to a previous HW
	   transaction attempt.  */
	cmpl	$a_tryHTMFastPath, %eax
	jnz	2f
	andl	$(0xffffffff-pr_HTMRetryableAbort), %edi
	orl	$pr_HTMRetriedAfterAbort, %edi
	jmp	.Lhtm_fastpath
2:
#endif
#else
	leal	4(%esp), %ecx
	movl	4(%esp), %eax
	subl	$28, %esp
	cfi_def_cfa_offset(32)
	movl	%ecx, 4(%esp)
	movl	%ebx, 8(%esp)
	movl	%esi, 12(%esp)
	movl	%edi, 16(%esp)
	movl	%ebp, 20(%esp)
	xorl	%edx, %edx
	/* Save zero or shadow stack pointer in the new field.  */
#if defined __SHSTK__ && defined __CET__ && (__CET__ & 2) != 0
	rdsspd	%edx
#endif
	movl	%edx, 24(%esp)
	leal	4(%esp), %edx
#if defined HAVE_ATTRIBUTE_VISIBILITY || !defined __PIC__
	call	SYM(GTM_begin_transaction)
#elif defined __ELF__
	call	1f
1:	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-1b], %ebx
	call	SYM(GTM_begin_transaction)@PLT
	movl	8(%esp), %ebx
#else
# error "Unsupported PIC sequence"
#endif
	addl	$28, %esp
	cfi_def_cfa_offset(4)
#endif
	ret
	cfi_endproc

	TYPE(_ITM_beginTransaction)
	SIZE(_ITM_beginTransaction)

	.align 4
	.globl	SYM(GTM_longjmp)

SYM(GTM_longjmp):
	cfi_startproc
	_CET_ENDBR
#ifdef __x86_64__
	movq	(%rsi), %rcx
	movq	8(%rsi), %rbx
	movq	16(%rsi), %rbp
	movq	24(%rsi), %r12
	movq	32(%rsi), %r13
	movq	40(%rsi), %r14
	movq	48(%rsi), %r15
	movl	%edi, %eax
	cfi_def_cfa(%rsi, 0)
	cfi_offset(%rip, 64)
	cfi_register(%rsp, %rcx)
	movq	%rcx, %rsp
#if defined __SHSTK__ && defined __CET__ && (__CET__ & 2) != 0
	/* Check if Shadow Stack is enabled.  */
	xorq	%rcx, %rcx
	rdsspq	%rcx
	testq	%rcx, %rcx
	je	.L1
	/* Calculate number of frames to skip.  */
	subq	56(%rsi), %rcx
	negq	%rcx
	shrq	$3, %rcx
	incq	%rcx
	/* If # of frames is greater 255 then loop
	   and adjust.  */
	cmpq	$255, %rcx
	jbe	.L3
	movl	$255, %edi
	.p2align 4,,10
	.p2align 3
.L4:
	incsspq	%rdi
	subq	$255, %rcx
	cmpq	$255, %rcx
	ja	.L4
.L3:
	incsspq	%rcx
.L1:
#endif
	jmp	*64(%rsi)
#else
	movl	(%edx), %ecx
	movl	4(%edx), %ebx
	movl	8(%edx), %esi
	movl	12(%edx), %edi
	movl	16(%edx), %ebp
	cfi_def_cfa(%edx, 0)
	cfi_offset(%eip, 24)
	cfi_register(%esp, %ecx)
	movl	%ecx, %esp
#if defined __SHSTK__ && defined __CET__ && (__CET__ & 2) != 0
	/* Check if Shadow Stack is enabled.  */
	xorl	%ecx, %ecx
	rdsspd	%ecx
	testl	%ecx, %ecx
	je	.L1
	/* Calculate # of frames to skip.  */
	subl	20(%edx), %ecx
	negl	%ecx
	shrl	$2, %ecx
	incl	%ecx
	/* If # of frames is greater 255 then loop
	   and adjust.  */
	cmpl	$255, %ecx
	jbe	.L3
	pushl	%eax
	movl	$255, %eax
	.p2align 4,,10
	.p2align 3
.L4:
	incsspd	%eax
	subl	$255, %ecx
	cmpl	$255, %ecx
	ja	.L4
	popl	%eax
.L3:
	incsspd	%ecx
.L1:
#endif
	jmp	*24(%edx)
#endif
	cfi_endproc

	TYPE(GTM_longjmp)
	HIDDEN(GTM_longjmp)
	SIZE(GTM_longjmp)

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
