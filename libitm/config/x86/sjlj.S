/* Copyright (C) 2008, 2009, 2011 Free Software Foundation, Inc.
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

	.text

	.align 4
	.globl	SYM(_ITM_beginTransaction)

SYM(_ITM_beginTransaction):
	cfi_startproc
#ifdef __x86_64__
	leaq	8(%rsp), %rax
	subq	$56, %rsp
	cfi_def_cfa_offset(64)
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	%rbp, 16(%rsp)
	movq	%r12, 24(%rsp)
	movq	%r13, 32(%rsp)
	movq	%r14, 40(%rsp)
	movq	%r15, 48(%rsp)
	movq	%rsp, %rsi
	call	SYM(GTM_begin_transaction)
	addq	$56, %rsp
	cfi_def_cfa_offset(8)
	ret
#else
	leal	4(%esp), %ecx
	movl	4(%esp), %eax
	subl	$28, %esp
	cfi_def_cfa_offset(32)
	movl	%ecx, 8(%esp)
	movl	%ebx, 12(%esp)
	movl	%esi, 16(%esp)
	movl	%edi, 20(%esp)
	movl	%ebp, 24(%esp)
	leal	8(%esp), %edx
#if defined HAVE_ATTRIBUTE_VISIBILITY || !defined __PIC__
	call	SYM(GTM_begin_transaction)
#elif defined __ELF__
	call	1f
1:	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-1b], %ebx
	call	SYM(GTM_begin_transaction)@PLT
	movl	12(%esp), %ebx
#else
# error "Unsupported PIC sequence"
#endif
	addl	$28, %esp
	cfi_def_cfa_offset(4)
	ret
#endif
	cfi_endproc

	TYPE(_ITM_beginTransaction)
	SIZE(_ITM_beginTransaction)

	.align 4
	.globl	SYM(GTM_longjmp)

SYM(GTM_longjmp):
	cfi_startproc
#ifdef __x86_64__
	cfi_def_cfa(%rsi, 0)
	movq	(%rsi), %rcx
	movq	8(%rsi), %rbx
	movq	16(%rsi), %rbp
	movq	24(%rsi), %r12
	movq	32(%rsi), %r13
	movq	40(%rsi), %r14
	movq	48(%rsi), %r15
	cfi_offset(%rip, 56)
	movl	%edi, %eax
	movq	%rcx, %rsp
	cfi_register(%rsp, %rcx)
	jmp	*56(%rsi)
#else
	cfi_def_cfa(%edx, 0)
	movl	(%edx), %ecx
	movl	4(%edx), %ebx
	movl	8(%edx), %esi
	movl	12(%edx), %edi
	movl	16(%edx), %ebp
	cfi_offset(%eip, 20)
	movl	%ecx, %esp
	cfi_register(%esp, %ecx)
	jmp	*20(%edx)
#endif
	cfi_endproc

	TYPE(GTM_longjmp)
	HIDDEN(GTM_longjmp)
	SIZE(GTM_longjmp)

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
