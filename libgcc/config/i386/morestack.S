# x86/x86_64 support for -fsplit-stack.
# Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

#include "auto-host.h"

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
#define BACKOFF (3584)
#endif


# The amount of space we ask for when calling non-split-stack code.
#define NON_SPLIT_STACK 0x100000

# This entry point is for split-stack code which calls non-split-stack
# code.  When the linker sees this case, it converts the call to
# __morestack to call __morestack_non_split instead.  We just bump the
# requested stack space by 16K.

#include <cet.h>

	.global __morestack_non_split
	.hidden	__morestack_non_split

#ifdef __ELF__
       .type	__morestack_non_split,@function
#endif

__morestack_non_split:
	.cfi_startproc

#ifndef __x86_64__

	# See below for an extended explanation of this.
	.cfi_def_cfa %esp,16

	pushl	%eax			# Save %eax in case it is a parameter.

	.cfi_adjust_cfa_offset 4	# Account for pushed register.

	movl	%esp,%eax		# Current stack,
	subl	8(%esp),%eax		# less required stack frame size,
	subl	$NON_SPLIT_STACK,%eax	# less space for non-split code.
	cmpl	%gs:0x30,%eax		# See if we have enough space.
	jb	2f			# Get more space if we need it.

	# Here the stack is
	#	%esp + 20:	stack pointer after two returns
	#	%esp + 16:	return address of morestack caller's caller
	#	%esp + 12:	size of parameters
	#	%esp + 8:	new stack frame size
	#	%esp + 4:	return address of this function
	#	%esp:		saved %eax
	#
	# Since we aren't doing a full split stack, we don't need to
	# do anything when our caller returns.  So we return to our
	# caller rather than calling it, and let it return as usual.
	# To make that work we adjust the return address.

	# This breaks call/return address prediction for the call to
	# this function.  I can't figure out a way to make it work
	# short of copying the parameters down the stack, which will
	# probably take more clock cycles than we will lose breaking
	# call/return address prediction.  We will only break
	# prediction for this call, not for our caller.

	movl	4(%esp),%eax		# Increment the return address
	cmpb	$0xc3,(%eax)		# to skip the ret instruction;
	je	1f			# see above.
	addl	$2,%eax
1:	inc	%eax

	# If the instruction that we return to is
	#   leal  20(%ebp),{%eax,%ecx,%edx}
	# then we have been called by a varargs function that expects
	# %ebp to hold a real value.  That can only work if we do the
	# full stack split routine.  FIXME: This is fragile.
	cmpb	$0x8d,(%eax)
	jne	3f
	cmpb	$0x14,2(%eax)
	jne	3f
	cmpb	$0x45,1(%eax)
	je	2f
	cmpb	$0x4d,1(%eax)
	je	2f
	cmpb	$0x55,1(%eax)
	je	2f

3:
	movl	%eax,4(%esp)		# Update return address.

	popl	%eax			# Restore %eax and stack.

	.cfi_adjust_cfa_offset -4	# Account for popped register.

	ret	$8			# Return to caller, popping args.

2:
	.cfi_adjust_cfa_offset 4	# Back to where we were.

	popl	%eax			# Restore %eax and stack.

	.cfi_adjust_cfa_offset -4	# Account for popped register.

	# Increment space we request.
	addl	$NON_SPLIT_STACK+0x1000+BACKOFF,4(%esp)

	# Fall through into morestack.

#else

	# See below for an extended explanation of this.
	.cfi_def_cfa %rsp,16

	pushq	%rax			# Save %rax in case caller is using
					# it to preserve original %r10.
	.cfi_adjust_cfa_offset 8	# Adjust for pushed register.

	movq	%rsp,%rax		# Current stack,
	subq	%r10,%rax		# less required stack frame size,
	subq	$NON_SPLIT_STACK,%rax	# less space for non-split code.

#ifdef __LP64__
	cmpq	%fs:0x70,%rax		# See if we have enough space.
#else
	cmpl	%fs:0x40,%eax
#endif

	jb	2f			# Get more space if we need it.

	# If the instruction that we return to is
	#   leaq  24(%rbp), %r11n
	# then we have been called by a varargs function that expects
	# %ebp to hold a real value.  That can only work if we do the
	# full stack split routine.  FIXME: This is fragile.
	movq	8(%rsp),%rax
	incq	%rax			# Skip ret instruction in caller.
	cmpl	$0x185d8d4c,(%rax)
	je	2f

	# This breaks call/return prediction, as described above.
	incq	8(%rsp)			# Increment the return address.

	popq	%rax			# Restore register.

	.cfi_adjust_cfa_offset -8	# Adjust for popped register.

	ret				# Return to caller.

2:
	popq	%rax			# Restore register.

	.cfi_adjust_cfa_offset -8	# Adjust for popped register.

	# Increment space we request.
	addq	$NON_SPLIT_STACK+0x1000+BACKOFF,%r10

	# Fall through into morestack.

#endif

	.cfi_endproc
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

	# We return below with a ret $8.  We will return to a single
	# return instruction, which will return to the caller of our
	# caller.  We let the unwinder skip that single return
	# instruction, and just return to the real caller.

	# Here CFA points just past the return address on the stack,
	# e.g., on function entry it is %esp + 4.  The stack looks
	# like this:
	#	CFA + 12:	stack pointer after two returns
	#	CFA + 8:	return address of morestack caller's caller
	#	CFA + 4:	size of parameters
	#	CFA:		new stack frame size
	#	CFA - 4:	return address of this function
	#	CFA - 8:	previous value of %ebp; %ebp points here
	# Setting the new CFA to be the current CFA + 12 (i.e., %esp +
	# 16) will make the unwinder pick up the right return address.

	.cfi_def_cfa %esp,16

	pushl	%ebp
	.cfi_adjust_cfa_offset 4
	.cfi_offset %ebp, -20
	movl	%esp,%ebp
	.cfi_def_cfa_register %ebp

	# In 32-bit mode the parameters are pushed on the stack.  The
	# argument size is pushed then the new stack frame size is
	# pushed.

	# In the body of a non-leaf function, the stack pointer will
	# be aligned to a 16-byte boundary.  That is CFA + 12 in the
	# stack picture above: (CFA + 12) % 16 == 0.  At this point we
	# have %esp == CFA - 8, so %esp % 16 == 12.  We need some
	# space for saving registers and passing parameters, and we
	# need to wind up with %esp % 16 == 0.
	subl	$44,%esp

	# Because our cleanup code may need to clobber %ebx, we need
	# to save it here so the unwinder can restore the value used
	# by the caller.  Note that we don't have to restore the
	# register, since we don't change it, we just have to save it
	# for the unwinder.
	movl	%ebx,-4(%ebp)
	.cfi_offset %ebx, -24

	# In 32-bit mode the registers %eax, %edx, and %ecx may be
	# used for parameters, depending on the regparm and fastcall
	# attributes.

	movl	%eax,-8(%ebp)
	movl	%edx,-12(%ebp)
	movl	%ecx,-16(%ebp)

	call	__morestack_block_signals

	movl	12(%ebp),%eax		# The size of the parameters.
	movl	%eax,8(%esp)
	leal	20(%ebp),%eax		# Address of caller's parameters.
	movl	%eax,4(%esp)
	addl	$BACKOFF,8(%ebp)	# Ask for backoff bytes.
	leal	8(%ebp),%eax		# The address of the new frame size.
	movl	%eax,(%esp)

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

	movl	-12(%ebp),%edx		# Restore registers.
	movl	-16(%ebp),%ecx

	movl	4(%ebp),%eax		# Increment the return address
	cmpb	$0xc3,(%eax)		# to skip the ret instruction;
	je	1f			# see above.
	addl	$2,%eax
1:	inc	%eax

	movl	%eax,-12(%ebp)		# Store return address in an
					# unused slot.

	movl	-8(%ebp),%eax		# Restore the last register.

	call	*-12(%ebp)		# Call our caller!

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

	# As before, we now have %esp % 16 == 12.

	pushl	%eax			# Push return value on old stack.
	pushl	%edx
	subl	$4,%esp			# Align stack to 16-byte boundary.

	call	__morestack_unblock_signals

	addl	$4,%esp
	popl	%edx			# Restore return value.
	popl	%eax

	.cfi_remember_state

	# We never changed %ebx, so we don't have to actually restore it.
	.cfi_restore %ebx

	popl	%ebp
	.cfi_restore %ebp
	.cfi_def_cfa %esp, 16
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
	call	__x86.get_pc_thunk.bx	# %ebx may not be set up for us.
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

	# We will return a single return instruction, which will
	# return to the caller of our caller.  Let the unwinder skip
	# that single return instruction, and just return to the real
	# caller.
	.cfi_def_cfa %rsp,16

	# Set up a normal backtrace.
	pushq	%rbp
	.cfi_adjust_cfa_offset 8
	.cfi_offset %rbp, -24
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp

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

	# We entered morestack with the stack pointer aligned to a
	# 16-byte boundary (the call to morestack's caller used 8
	# bytes, and the call to morestack used 8 bytes).  We have now
	# pushed 10 registers, so we are still aligned to a 16-byte
	# boundary.

	call	__morestack_block_signals

	leaq	-8(%rbp),%rdi		# Address of new frame size.
	leaq	24(%rbp),%rsi		# The caller's parameters.
	popq	%rdx			# The size of the parameters.

	subq	$8,%rsp			# Align stack.

	call	__generic_morestack

	movq	-8(%rbp),%r10		# Reload modified frame size
	movq	%rax,%rsp		# Switch to the new stack.
	subq	%r10,%rax		# The end of the stack space.
	addq	$BACKOFF,%rax		# Back off 1024 bytes.

.LEHB0:
	# FIXME: The offset must match
	# TARGET_THREAD_SPLIT_STACK_OFFSET in
	# gcc/config/i386/linux64.h.
	# Macro to save the new stack boundary.
#ifdef __LP64__
#define X86_64_SAVE_NEW_STACK_BOUNDARY(reg)	movq	%r##reg,%fs:0x70
#else
#define X86_64_SAVE_NEW_STACK_BOUNDARY(reg)	movl	%e##reg,%fs:0x40
#endif
	X86_64_SAVE_NEW_STACK_BOUNDARY (ax)

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
	X86_64_SAVE_NEW_STACK_BOUNDARY (ax)

	addq	$16,%rsp		# Remove values from stack.

	# We need to restore the old stack pointer, which is in %rbp,
	# before we unblock signals.  We also need to restore %rax and
	# %rdx after we unblock signals but before we return.  Do this
	# by moving %rax and %rdx from the current stack to the old
	# stack.

	popq	%rdx			# Pop return value from current stack.
	popq	%rax

	movq	%rbp,%rsp		# Restore stack pointer.

	# Now (%rsp & 16) == 8.

	subq	$8,%rsp			# For alignment.
	pushq	%rax			# Push return value on old stack.
	pushq	%rdx

	call	__morestack_unblock_signals

	popq	%rdx			# Restore return value.
	popq	%rax
	addq	$8,%rsp

	.cfi_remember_state
	popq	%rbp
	.cfi_restore %rbp
	.cfi_def_cfa %rsp, 16
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
	X86_64_SAVE_NEW_STACK_BOUNDARY (cx)
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

#if !defined(__x86_64__) && defined(__PIC__)
# Output the thunk to get PC into bx, since we use it above.
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
#ifndef __LP64__
	.align 4
	.size	DW.ref.__gcc_personality_v0, 4
	.long	__gcc_personality_v0
#else
	.align 8
	.size	DW.ref.__gcc_personality_v0, 8
	.quad	__gcc_personality_v0
#endif
#endif

#if defined __x86_64__ && defined __LP64__

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
	_CET_ENDBR

	movq	%r10, %r11
	andl	$0xffffffff, %r10d
	sarq	$32, %r11
	jmp	__morestack

	.cfi_endproc
#ifdef __ELF__
       .size	__morestack_large_model, . - __morestack_large_model
#endif

#endif /* __x86_64__ && __LP64__ */

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
	_CET_ENDBR

#ifndef __x86_64__

	leal	-16000(%esp),%eax	# We should have at least 16K.
	movl	%eax,%gs:0x30
	subl	$4,%esp			# Align stack.
	pushl	$16000
	pushl	%esp
#ifdef __PIC__
	call	__generic_morestack_set_initial_sp@PLT
#else
	call	__generic_morestack_set_initial_sp
#endif
	addl	$12,%esp
	ret

#else /* defined(__x86_64__) */

	leaq	-16000(%rsp),%rax	# We should have at least 16K.
	X86_64_SAVE_NEW_STACK_BOUNDARY (ax)
	subq	$8,%rsp			# Align stack.
	movq	%rsp,%rdi
	movq	$16000,%rsi
#ifdef __PIC__
	call	__generic_morestack_set_initial_sp@PLT
#else
	call	__generic_morestack_set_initial_sp
#endif
	addq	$8,%rsp
	ret

#endif /* defined(__x86_64__) */

#ifdef __ELF__
	.size	__stack_split_initialize, . - __stack_split_initialize
#endif

# Routines to get and set the guard, for __splitstack_getcontext,
# __splitstack_setcontext, and __splitstack_makecontext.

# void *__morestack_get_guard (void) returns the current stack guard.
	.text
	.global	__morestack_get_guard
	.hidden	__morestack_get_guard

#ifdef __ELF__
	.type	__morestack_get_guard,@function
#endif

__morestack_get_guard:

#ifndef __x86_64__
	movl	%gs:0x30,%eax
#else
#ifdef __LP64__
	movq	%fs:0x70,%rax
#else
	movl	%fs:0x40,%eax
#endif
#endif
	ret

#ifdef __ELF__
	.size	__morestack_get_guard, . - __morestack_get_guard
#endif

# void __morestack_set_guard (void *) sets the stack guard.
	.global	__morestack_set_guard
	.hidden	__morestack_set_guard

#ifdef __ELF__
	.type	__morestack_set_guard,@function
#endif

__morestack_set_guard:

#ifndef __x86_64__
	movl	4(%esp),%eax
	movl	%eax,%gs:0x30
#else
	X86_64_SAVE_NEW_STACK_BOUNDARY (di)
#endif
	ret

#ifdef __ELF__
	.size	__morestack_set_guard, . - __morestack_set_guard
#endif

# void *__morestack_make_guard (void *, size_t) returns the stack
# guard value for a stack.
	.global	__morestack_make_guard
	.hidden	__morestack_make_guard

#ifdef __ELF__
	.type	__morestack_make_guard,@function
#endif

__morestack_make_guard:

#ifndef __x86_64__
	movl	4(%esp),%eax
	subl	8(%esp),%eax
	addl	$BACKOFF,%eax
#else
	subq	%rsi,%rdi
	addq	$BACKOFF,%rdi
	movq	%rdi,%rax
#endif
	ret

#ifdef __ELF__
	.size	__morestack_make_guard, . - __morestack_make_guard
#endif

# Make __stack_split_initialize a high priority constructor.  FIXME:
# This is ELF specific.

#if HAVE_INITFINI_ARRAY_SUPPORT
	.section	.init_array.00000,"aw",@progbits
#else
	.section	.ctors.65535,"aw",@progbits
#endif

#ifndef __LP64__
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
