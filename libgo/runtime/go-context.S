// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This provides a simplified version of getcontext and
// setcontext. They are like the corresponding functions
// in libc, but we only save/restore the callee-save
// registers and PC, SP. Unlike the libc functions, we
// don't save/restore the signal masks and floating point
// environment.

#if defined(__x86_64__) && defined(__linux__) && !defined(__CET__)

#define RBP_OFF	(0*8)
#define RBX_OFF	(1*8)
#define R12_OFF	(2*8)
#define R13_OFF	(3*8)
#define R14_OFF	(4*8)
#define R15_OFF	(5*8)
#define SP_OFF	(6*8)
#define PC_OFF	(7*8)

.globl __go_getcontext
.text
__go_getcontext:
	movq	%rbx, RBX_OFF(%rdi)
	movq	%rbp, RBP_OFF(%rdi)
	movq	%r12, R12_OFF(%rdi)
	movq	%r13, R13_OFF(%rdi)
	movq	%r14, R14_OFF(%rdi)
	movq	%r15, R15_OFF(%rdi)

	movq	(%rsp), %rax	// return PC
	movq	%rax, PC_OFF(%rdi)
	leaq	8(%rsp), %rax	// the SP before pushing return PC
	movq	%rax, SP_OFF(%rdi)

	ret

.globl __go_setcontext
.text
__go_setcontext:
	movq	RBX_OFF(%rdi), %rbx
	movq	RBP_OFF(%rdi), %rbp
	movq	R12_OFF(%rdi), %r12
	movq	R13_OFF(%rdi), %r13
	movq	R14_OFF(%rdi), %r14
	movq	R15_OFF(%rdi), %r15
	movq	SP_OFF(%rdi), %rsp
	movq	PC_OFF(%rdi), %rdx

	jmp	*%rdx

.globl __go_makecontext
.text
__go_makecontext:
	addq	%rcx, %rdx

	// Align the SP, and push a dummy return address.
	andq	$~0xf, %rdx
	subq	$8, %rdx
	movq	$0, (%rdx)

	movq	%rdx, SP_OFF(%rdi)
	movq	%rsi, PC_OFF(%rdi)

	ret

	.section	.note.GNU-stack,"",@progbits
	.section	.note.GNU-split-stack,"",@progbits
	.section	.note.GNU-no-split-stack,"",@progbits

#endif
