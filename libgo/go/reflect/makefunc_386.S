# Copyright 2013 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# MakeFunc 386 assembly code.

	.global reflect.makeFuncStub

#ifdef __ELF__
	.type reflect.makeFuncStub,@function
#endif

reflect.makeFuncStub:
	.cfi_startproc

	# Go does not provide any equivalent to the regparm function
	# attribute, so on Go we do not need to worry about passing
	# parameters in registers.  We just pass a pointer to the
	# arguments on the stack.
	#
	# We do need to pick up the return values, though, so we pass
	# a pointer to a struct that looks like this.
	# struct {
	#   esp uint32		// 0x0
	#   eax uint32		// 0x4
	#   st0 uint64		// 0x8
	# }

	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp

	pushl	%ebx		# In case this is PIC.

	subl	$36, %esp	# Enough for args and to align stack.
	.cfi_offset %ebx, -12

#ifdef __PIC__
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
#endif

	leal	8(%ebp), %eax	# Set esp field in struct.
	movl	%eax, -24(%ebp)

#ifdef __PIC__
	call	__go_get_closure@PLT
#else
	call	__go_get_closure
#endif

	movl	%eax, 4(%esp)

	leal	-24(%ebp), %eax
	movl	%eax, (%esp)

#ifdef __PIC__
	call	reflect.MakeFuncStubGo@PLT
#else
	call	reflect.MakeFuncStubGo
#endif

	# Set return registers.

	movl	-20(%ebp), %eax
	fldl	-16(%ebp)

#ifdef __SSE2__
	# In case we are compiling with -msseregparm.  This won't work
	# correctly if only SSE1 is supported, but that seems unlikely.
	movsd	-16(%ebp), %xmm0
#endif

	addl	$36, %esp
	popl	%ebx
	.cfi_restore %ebx
	popl	%ebp
	.cfi_restore %ebp
	.cfi_def_cfa %esp, 4

	ret
	.cfi_endproc

#ifdef __ELF__
	.size	reflect.makeFuncStub, . - reflect.makeFuncStub
#endif

#ifdef __PIC__
	.section	.text.__x86.get_pc_thunk.bx,"axG",@progbits,__x86.get_pc_thunk.bx,comdat
	.globl	__x86.get_pc_thunk.bx
	.hidden	__x86.get_pc_thunk.bx
#ifdef __ELF__
	.type	__x86.get_pc_thunk.bx, @function
#endif
__x86.get_pc_thunk.bx:
	.cfi_startproc
	movl	(%esp), %ebx
	ret
	.cfi_endproc
#ifdef __ELF__
	.size	__x86.get_pc_thunk.bx, . - __x86.get_pc_thunk.bx
#endif
#endif

#ifdef __ELF__
	.section	.note.GNU-stack,"",@progbits
	.section	.note.GNU-split-stack,"",@progbits
	.section	.note.GNU-no-split-stack,"",@progbits
#endif
