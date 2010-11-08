# x86/x86_64 support for -fsplit-stack.
# Copyright (C) 2009, 2010 Free Software Foundation, Inc.
# Contributed by Ian Lance Taylor <iant@google.com>.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.

# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.

# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.


# Support for allocating more stack space when using -fsplit-stack.
# When a function discovers that it needs more stack space, it will
# call __morestack with the size of the stack frame and the size of
# the parameters to copy from the old stack frame to the new one.
# The __morestack function preserves the parameter registers and
# calls __generic_morestack to actually allocate the stack space.

# When this is called stack space is very low, but we ensure that
# there is enough space to push the parameter registers and to call
# __generic_morestack.

# When calling __generic_morestack, FRAME_SIZE points to the size of
# the desired frame when the function is called, and the function
# sets it to the size of the allocated stack.  OLD_STACK points to
# the parameters on the old stack and PARAM_SIZE is the number of
# bytes of parameters to copy to the new stack.  These are the
# parameters of the function that called __morestack.  The
# __generic_morestack function returns the new stack pointer,
# pointing to the address of the first copied parameter.  The return
# value minus the returned *FRAME_SIZE will be the first address on
# the stack which we should not use.

# void *__generic_morestack (size_t *frame_size, void *old_stack,
#			     size_t param_size);

# The __morestack routine has to arrange for the caller to return to a
# stub on the new stack.  The stub is responsible for restoring the
# old stack pointer and returning to the caller's caller.  This calls
# __generic_releasestack to retrieve the old stack pointer and release
# the newly allocated stack.

# void *__generic_releasestack (size_t *available);

# We do a little dance so that the processor's call/return return
# address prediction works out.  The compiler arranges for the caller
# to look like this:
#   call __generic_morestack
#   ret
#  L:
#   // carry on with function
# After we allocate more stack, we call L, which is in our caller.
# When that returns (to the predicted instruction), we release the
# stack segment and reset the stack pointer.  We then return to the
# predicted instruction, namely the ret instruction immediately after
# the call to __generic_morestack.  That then returns to the caller of
# the original caller.


# The amount of extra space we ask for.  In general this has to be
# enough for the dynamic loader to find a symbol and for a signal
# handler to run.
	
#ifndef __x86_64__
#define BACKOFF (1024)
#else
#define BACKOFF (1536)
#endif


# This entry point is for split-stack code which calls non-split-stack
# code.  When the linker sees this case, it converts the call to
# __morestack to call __morestack_non_split instead.  We just bump the
# requested stack space by 16K.

	.global __morestack_non_split
	.hidden	__morestack_non_split

#ifdef __ELF__
       .type	__morestack_non_split,@function
#endif

__morestack_non_split:

#ifndef __x86_64__
	addl	$0x4000,4(%esp)
#else
	addq	$0x4000,%r10
#endif

#ifdef __ELF__
	.size	__morestack_non_split, . - __morestack_non_split
#endif

# __morestack_non_split falls through into __morestack.


# The __morestack function.

	.global	__morestack
	.hidden	__morestack

#ifdef __ELF__
	.type	__morestack,@function
#endif

__morestack:
.LFB1:
	.cfi_startproc


#ifndef __x86_64__


# The 32-bit __morestack function.

	# We use a cleanup to restore the stack guard if an exception
	# is thrown through this code.
#ifndef __PIC__
	.cfi_personality 0,__gcc_personality_v0
	.cfi_lsda 0,.LLSDA1
#else
	.cfi_personality 0x9b,DW.ref.__gcc_personality_v0
	.cfi_lsda 0x1b,.LLSDA1
#endif

	# Set up a normal backtrace.
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp

	# We return below with a ret $8.  We will return to a single
	# return instruction, which will return to the caller of our
	# caller.  We let the unwinder skip that single return
	# instruction, and just return to the real caller.
	.cfi_offset 8, 8
	.cfi_escape 0x15, 4, 0x7d	# DW_CFA_val_offset_sf, %esp, 12/-4

	# In 32-bit mode the parameters are pushed on the stack.  The
	# argument size is pushed then the new stack frame size is
	# pushed.

	# In 32-bit mode the registers %eax, %edx, and %ecx may be
	# used for parameters, depending on the regparm and fastcall
	# attributes.

	pushl	%eax
	pushl	%edx
	pushl	%ecx

	call	__morestack_block_signals

	pushl	12(%ebp)		# The size of the parameters.
	leal	20(%ebp),%eax		# Address of caller's parameters.
	pushl	%eax
	addl	$BACKOFF,8(%ebp)	# Ask for backoff bytes.
	leal	8(%ebp),%eax		# The address of the new frame size.
	pushl	%eax

	# Note that %esp is exactly 32 bytes below the CFA -- perfect for
	# a 16-byte aligned stack.  That said, we still ought to compile
	# generic-morestack.c with -mpreferred-stack-boundary=2.  FIXME.
	call	__generic_morestack

	movl	%eax,%esp		# Switch to the new stack.
	subl	8(%ebp),%eax		# The end of the stack space.
	addl	$BACKOFF,%eax		# Back off 512 bytes.

.LEHB0:
	# FIXME: The offset must match
	# TARGET_THREAD_SPLIT_STACK_OFFSET in
	# gcc/config/i386/linux.h.
	movl	%eax,%gs:0x30		# Save the new stack boundary.

	call	__morestack_unblock_signals

	movl	-8(%ebp),%edx		# Restore registers.
	movl	-12(%ebp),%ecx

	movl	4(%ebp),%eax		# Increment the return address
	cmpb	$0xc3,(%eax)		# to skip the ret instruction;
	je	1f			# see above.
	addl	$2,%eax
1:	inc	%eax

	movl	%eax,-8(%ebp)		# Store return address in an
					# unused slot.

	movl	-4(%ebp),%eax		# Restore the last register.

	call	*-8(%ebp)		# Call our caller!

	# The caller will return here, as predicted.

	# Save the registers which may hold a return value.  We
	# assume that __generic_releasestack does not touch any
	# floating point or vector registers.
	pushl	%eax
	pushl	%edx

	# Push the arguments to __generic_releasestack now so that the
	# stack is at a 16-byte boundary for
	# __morestack_block_signals.
	pushl	$0			# Where the available space is returned.
	leal	0(%esp),%eax		# Push its address.
	push	%eax

	call	__morestack_block_signals

	call	__generic_releasestack

	subl	4(%esp),%eax		# Subtract available space.
	addl	$BACKOFF,%eax		# Back off 512 bytes.
.LEHE0:
	movl	%eax,%gs:0x30		# Save the new stack boundary.

	addl	$8,%esp			# Remove values from stack.

	# We need to restore the old stack pointer, which is in %rbp,
	# before we unblock signals.  We also need to restore %eax and
	# %edx after we unblock signals but before we return.  Do this
	# by moving %eax and %edx from the current stack to the old
	# stack.

	popl	%edx			# Pop return value from current stack.
	popl	%eax

	movl	%ebp,%esp		# Restore stack pointer.

	pushl	%eax			# Push return value on old stack.
	pushl	%edx
	subl	$8,%esp			# Align stack to 16-byte boundary.

	call	__morestack_unblock_signals

	addl	$8,%esp
	popl	%edx			# Restore return value.
	popl	%eax

	.cfi_remember_state
	popl	%ebp
	.cfi_restore %ebp
	.cfi_def_cfa %esp, 12
	ret	$8			# Return to caller, which will
					# immediately return.  Pop
					# arguments as we go.

# This is the cleanup code called by the stack unwinder when unwinding
# through the code between .LEHB0 and .LEHE0 above.
	
.L1:
	.cfi_restore_state
	subl	$16,%esp		# Maintain 16 byte alignment.
	movl	%eax,4(%esp)		# Save exception header.
	movl	%ebp,(%esp)		# Stack pointer after resume.
	call	__generic_findstack
	movl	%ebp,%ecx		# Get the stack pointer.
	subl	%eax,%ecx		# Subtract available space.
	addl	$BACKOFF,%ecx		# Back off 512 bytes.
	movl	%ecx,%gs:0x30		# Save new stack boundary.
	movl	4(%esp),%eax		# Function argument.
	movl	%eax,(%esp)
#ifdef __PIC__
#undef __i686
	call	__i686.get_pc_thunk.bx	# %ebx may not be set up for us.
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	call	_Unwind_Resume@PLT	# Resume unwinding.
#else
	call	_Unwind_Resume
#endif

#else /* defined(__x86_64__) */


# The 64-bit __morestack function.

	# We use a cleanup to restore the stack guard if an exception
	# is thrown through this code.
#ifndef __PIC__
	.cfi_personality 0x3,__gcc_personality_v0
	.cfi_lsda 0x3,.LLSDA1
#else
	.cfi_personality 0x9b,DW.ref.__gcc_personality_v0
	.cfi_lsda 0x1b,.LLSDA1
#endif

	# Set up a normal backtrace.
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp

	# We will return a single return instruction, which will
	# return to the caller of our caller.  Let the unwinder skip
	# that single return instruction, and just return to the real
	# caller.
	.cfi_offset 16, 0
	.cfi_escape 0x15, 7, 0x7f	# DW_CFA_val_offset_sf, %esp, 8/-8

	# In 64-bit mode the new stack frame size is passed in r10
        # and the argument size is passed in r11.

	addq	$BACKOFF,%r10		# Ask for backoff bytes.
	pushq	%r10			# Save new frame size.

	# In 64-bit mode the registers %rdi, %rsi, %rdx, %rcx, %r8,
	# and %r9 may be used for parameters.  We also preserve %rax
	# which the caller may use to hold %r10.

	pushq	%rax
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	pushq	%r8
	pushq	%r9

	pushq	%r11
	pushq	$0			# For alignment.

	call	__morestack_block_signals

	leaq	-8(%rbp),%rdi		# Address of new frame size.
	leaq	24(%rbp),%rsi		# The caller's parameters.
	addq	$8,%rsp
	popq	%rdx			# The size of the parameters.

	call	__generic_morestack

	movq	-8(%rbp),%r10		# Reload modified frame size
	movq	%rax,%rsp		# Switch to the new stack.
	subq	%r10,%rax		# The end of the stack space.
	addq	$BACKOFF,%rax		# Back off 1024 bytes.

.LEHB0:
	# FIXME: The offset must match
	# TARGET_THREAD_SPLIT_STACK_OFFSET in
	# gcc/config/i386/linux64.h.
	movq	%rax,%fs:0x70		# Save the new stack boundary.

	call	__morestack_unblock_signals

	movq	-24(%rbp),%rdi		# Restore registers.
	movq	-32(%rbp),%rsi
	movq	-40(%rbp),%rdx
	movq	-48(%rbp),%rcx
	movq	-56(%rbp),%r8
	movq	-64(%rbp),%r9

	movq	8(%rbp),%r10		# Increment the return address
	incq	%r10			# to skip the ret instruction;
					# see above.

	movq	-16(%rbp),%rax		# Restore caller's %rax.

	call	*%r10			# Call our caller!

	# The caller will return here, as predicted.

	# Save the registers which may hold a return value.  We
	# assume that __generic_releasestack does not touch any
	# floating point or vector registers.
	pushq	%rax
	pushq	%rdx

	call	__morestack_block_signals

	pushq	$0			# For alignment.
	pushq	$0			# Where the available space is returned.
	leaq	0(%rsp),%rdi		# Pass its address.

	call	__generic_releasestack

	subq	0(%rsp),%rax		# Subtract available space.
	addq	$BACKOFF,%rax		# Back off 1024 bytes.
.LEHE0:
	movq	%rax,%fs:0x70		# Save the new stack boundary.

	addq	$16,%rsp		# Remove values from stack.

	# We need to restore the old stack pointer, which is in %rbp,
	# before we unblock signals.  We also need to restore %rax and
	# %rdx after we unblock signals but before we return.  Do this
	# by moving %rax and %rdx from the current stack to the old
	# stack.

	popq	%rdx			# Pop return value from current stack.
	popq	%rax

	movq	%rbp,%rsp		# Restore stack pointer.

	pushq	%rax			# Push return value on old stack.
	pushq	%rdx

	call	__morestack_unblock_signals

	popq	%rdx			# Restore return value.
	popq	%rax

	.cfi_remember_state
	popq	%rbp
	.cfi_restore %rbp
	.cfi_def_cfa %rsp, 8
	ret				# Return to caller, which will
					# immediately return.

# This is the cleanup code called by the stack unwinder when unwinding
# through the code between .LEHB0 and .LEHE0 above.
	
.L1:
	.cfi_restore_state
	subq	$16,%rsp		# Maintain 16 byte alignment.
	movq	%rax,(%rsp)		# Save exception header.
	movq	%rbp,%rdi		# Stack pointer after resume.
	call	__generic_findstack
	movq	%rbp,%rcx		# Get the stack pointer.
	subq	%rax,%rcx		# Subtract available space.
	addq	$BACKOFF,%rcx		# Back off 1024 bytes.
	movq	%rcx,%fs:0x70		# Save new stack boundary.
	movq	(%rsp),%rdi		# Restore exception data for call.
#ifdef __PIC__
	call	_Unwind_Resume@PLT	# Resume unwinding.
#else
	call	_Unwind_Resume		# Resume unwinding.
#endif

#endif /* defined(__x86_64__) */

	.cfi_endproc
#ifdef __ELF__
	.size	__morestack, . - __morestack
#endif


# The exception table.  This tells the personality routine to execute
# the exception handler.

	.section	.gcc_except_table,"a",@progbits
	.align	4
.LLSDA1:
	.byte	0xff	# @LPStart format (omit)
	.byte	0xff	# @TType format (omit)
	.byte	0x1	# call-site format (uleb128)
	.uleb128 .LLSDACSE1-.LLSDACSB1	# Call-site table length
.LLSDACSB1:
	.uleb128 .LEHB0-.LFB1	# region 0 start
	.uleb128 .LEHE0-.LEHB0	# length
	.uleb128 .L1-.LFB1	# landing pad
	.uleb128 0		# action
.LLSDACSE1:


	.global __gcc_personality_v0
#ifdef __PIC__
	# Build a position independent reference to the basic
        # personality function.
	.hidden DW.ref.__gcc_personality_v0
	.weak   DW.ref.__gcc_personality_v0
	.section .data.DW.ref.__gcc_personality_v0,"awG",@progbits,DW.ref.__gcc_personality_v0,comdat
	.type	DW.ref.__gcc_personality_v0, @object
DW.ref.__gcc_personality_v0:
#ifndef __x86_64
	.align 4
	.size	DW.ref.__gcc_personality_v0, 4
	.long	__gcc_personality_v0
#else
	.align 8
	.size	DW.ref.__gcc_personality_v0, 8
	.quad	__gcc_personality_v0
#endif
#endif

#ifdef __x86_64__

# This entry point is used for the large model.  With this entry point
# the upper 32 bits of %r10 hold the argument size and the lower 32
# bits hold the new stack frame size.  There doesn't seem to be a way
# to know in the assembler code that we are assembling for the large
# model, and there doesn't seem to be a large model multilib anyhow.
# If one is developed, then the non-PIC code is probably OK since we
# will probably be close to the morestack code, but the PIC code
# almost certainly needs to be changed.  FIXME.

	.text
	.global	__morestack_large_model
	.hidden	__morestack_large_model

#ifdef __ELF__
	.type	__morestack_large_model,@function
#endif

__morestack_large_model:

	.cfi_startproc

	movq	%r10, %r11
	andl	$0xffffffff, %r10d
	sarq	$32, %r11
	jmp	__morestack

	.cfi_endproc
#ifdef __ELF__
       .size	__morestack_large_model, . - __morestack_large_model
#endif

#endif /* __x86_64__ */

# Initialize the stack test value when the program starts or when a
# new thread starts.  We don't know how large the main stack is, so we
# guess conservatively.  We might be able to use getrlimit here.

	.text
	.global	__stack_split_initialize
	.hidden	__stack_split_initialize

#ifdef __ELF__
	.type	__stack_split_initialize, @function
#endif

__stack_split_initialize:

#ifndef __x86_64__

	leal	-16000(%esp),%eax	# We should have at least 16K.
	movl	%eax,%gs:0x30
	pushl	$16000
	pushl	%esp
#ifdef __PIC__
	call	__generic_morestack_set_initial_sp@PLT
#else
	call	__generic_morestack_set_initial_sp
#endif
	addl	$8,%esp
	ret

#else /* defined(__x86_64__) */

	leaq	-16000(%rsp),%rax	# We should have at least 16K.
	movq	%rax,%fs:0x70
	movq	%rsp,%rdi
	movq	$16000,%rsi
#ifdef __PIC__
	call	__generic_morestack_set_initial_sp@PLT
#else
	call	__generic_morestack_set_initial_sp
#endif
	ret

#endif /* defined(__x86_64__) */

#ifdef __ELF__
	.size	__stack_split_initialize, . - __stack_split_initialize
#endif


# Make __stack_split_initialize a high priority constructor.  FIXME:
# This is ELF specific.

	.section	.ctors.65535,"aw",@progbits

#ifndef __x86_64__
	.align	4
	.long	__stack_split_initialize
	.long	__morestack_load_mmap
#else
	.align	8
	.quad	__stack_split_initialize
	.quad	__morestack_load_mmap
#endif

#ifdef __ELF__
	.section	.note.GNU-stack,"",@progbits
	.section	.note.GNU-split-stack,"",@progbits
	.section	.note.GNU-no-split-stack,"",@progbits
#endif
