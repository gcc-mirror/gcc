# Copyright 2013 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# MakeFunc amd64 assembly code.

	.global	reflect.makeFuncStub

#ifdef __ELF__
	.type	reflect.makeFuncStub,@function
#endif

reflect.makeFuncStub:
	.cfi_startproc

	# Store all the parameter registers in a struct that looks
	# like:
	# struct {
	#   rax uint64		// 0x0
	#   rdi uint64		// 0x8
	#   rsi uint64		// 0x10
	#   rdx uint64		// 0x18
	#   rcx uint64		// 0x20
	#   r8 uint64		// 0x28
	#   r9 uint64		// 0x30
	#   rsp uint64		// 0x38 Pointer to arguments on stack.
	#   xmm0 [2]uint64	// 0x40
	#   xmm1 [2]uint64	// 0x50
	#   xmm2 [2]uint64	// 0x60
	#   xmm3 [2]uint64	// 0x70
	#   xmm4 [2]uint64	// 0x80
	#   xmm5 [2]uint64	// 0x90
	#   xmm6 [2]uint64	// 0xa0
	#   xmm7 [2]uint64	// 0xb0
	# };

	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp

	subq	$0xc0, %rsp		# Space for struct on stack.

	movq	%rax, 0x0(%rsp)
	movq	%rdi, 0x8(%rsp)
	movq	%rsi, 0x10(%rsp)
	movq	%rdx, 0x18(%rsp)
	movq	%rcx, 0x20(%rsp)
	movq	%r8, 0x28(%rsp)
	movq	%r9, 0x30(%rsp)
	leaq	16(%rbp), %rax
	movq	%rax, 0x38(%rsp)
	movdqa	%xmm0, 0x40(%rsp)
	movdqa	%xmm1, 0x50(%rsp)
	movdqa	%xmm2, 0x60(%rsp)
	movdqa	%xmm3, 0x70(%rsp)
	movdqa	%xmm4, 0x80(%rsp)
	movdqa	%xmm5, 0x90(%rsp)
	movdqa	%xmm6, 0xa0(%rsp)
	movdqa	%xmm7, 0xb0(%rsp)

	# Get function type.
#ifdef __PIC__
	call	__go_get_closure@PLT
#else
	call	__go_get_closure
#endif
	movq	%rax, %rsi

	movq	%rsp, %rdi

#ifdef __PIC__
	call	reflect.MakeFuncStubGo@PLT
#else
	call	reflect.MakeFuncStubGo
#endif

	# The structure will be updated with any return values.  Load
	# all possible return registers before returning to the caller.

	movq	0x0(%rsp), %rax
	movq	0x18(%rsp), %rdx
	movq	0x8(%rsp), %rdi
	movq	0x10(%rsp), %rsi
	movdqa	0x40(%rsp), %xmm0
	movdqa	0x50(%rsp), %xmm1

	# long double values are returned on the floating point stack,
	# but we don't worry about that since Go doesn't have a long
	# double type.

	leave
	.cfi_def_cfa %rsp, 8

	ret

	.cfi_endproc
#ifdef __ELF__
	.size	reflect.makeFuncStub, . - reflect.makeFuncStub
#endif

#ifdef __ELF__
	.section	.note.GNU-stack,"",@progbits
	.section	.note.GNU-split-stack,"",@progbits
	.section	.note.GNU-no-split-stack,"",@progbits
#endif
