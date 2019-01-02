# s390 support for -fsplit-stack.
# Copyright (C) 2015-2019 Free Software Foundation, Inc.
# Contributed by Marcin Kościelnicki <koriakin@0x04.net>.

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

# Excess space needed to call ld.so resolver for lazy plt
# resolution.  Go uses sigaltstack so this doesn't need to
# also cover signal frame size.
#define BACKOFF 0x1000

# The __morestack function.

	.global	__morestack
	.hidden	__morestack

	.type	__morestack,@function

__morestack:
.LFB1:
	.cfi_startproc


#ifndef __s390x__


# The 31-bit __morestack function.

	# We use a cleanup to restore the stack guard if an exception
	# is thrown through this code.
#ifndef __PIC__
	.cfi_personality 0,__gcc_personality_v0
	.cfi_lsda 0,.LLSDA1
#else
	.cfi_personality 0x9b,DW.ref.__gcc_personality_v0
	.cfi_lsda 0x1b,.LLSDA1
#endif

	stm	%r2, %r15, 0x8(%r15)	# Save %r2-%r15.
	.cfi_offset %r6, -0x48
	.cfi_offset %r7, -0x44
	.cfi_offset %r8, -0x40
	.cfi_offset %r9, -0x3c
	.cfi_offset %r10, -0x38
	.cfi_offset %r11, -0x34
	.cfi_offset %r12, -0x30
	.cfi_offset %r13, -0x2c
	.cfi_offset %r14, -0x28
	.cfi_offset %r15, -0x24
	lr	%r11, %r15		# Make frame pointer for vararg.
	.cfi_def_cfa_register %r11
	ahi	%r15, -0x60		# 0x60 for standard frame.
	st	%r11, 0(%r15)		# Save back chain.
	lr	%r8, %r0		# Save %r0 (static chain).
	lr	%r10, %r1		# Save %r1 (address of parameter block).

	l	%r7, 0(%r10)		# Required frame size to %r7
	ear	%r1, %a0		# Extract thread pointer.
	l	%r1, 0x20(%r1)		# Get stack bounduary
	ar	%r1, %r7		# Stack bounduary + frame size
	a	%r1, 4(%r10)		# + stack param size
	clr	%r1, %r15		# Compare with current stack pointer
	jle	.Lnoalloc		# guard > sp - frame-size: need alloc

	brasl	%r14, __morestack_block_signals

	# We abuse one of caller's fpr save slots (which we don't use for fprs)
	# as a local variable.  Not needed here, but done to be consistent with
	# the below use.
	ahi	%r7, BACKOFF		# Bump requested size a bit.
	st	%r7, 0x40(%r11)		# Stuff frame size on stack.
	la	%r2, 0x40(%r11)		# Pass its address as parameter.
	la	%r3, 0x60(%r11)		# Caller's stack parameters.
	l	%r4, 4(%r10)		# Size of stack parameters.
	brasl	%r14, __generic_morestack

	lr	%r15, %r2		# Switch to the new stack.
	ahi	%r15, -0x60		# Make a stack frame on it.
	st	%r11, 0(%r15)		# Save back chain.

	s	%r2, 0x40(%r11)		# The end of stack space.
	ahi	%r2, BACKOFF		# Back off a bit.
	ear	%r1, %a0		# Extract thread pointer.
.LEHB0:
	st	%r2, 0x20(%r1)	# Save the new stack boundary.

	brasl	%r14, __morestack_unblock_signals

	lr	%r0, %r8		# Static chain.
	lm	%r2, %r6, 0x8(%r11)	# Paremeter registers.

	# Third parameter is address of function meat - address of parameter
	# block.
	a	%r10, 0x8(%r10)

	# Leave vararg pointer in %r1, in case function uses it
	la	%r1, 0x60(%r11)

	# State of registers:
	# %r0: Static chain from entry.
	# %r1: Vararg pointer.
	# %r2-%r6: Parameters from entry.
	# %r7-%r10: Indeterminate.
	# %r11: Frame pointer (%r15 from entry).
	# %r12-%r13: Indeterminate.
	# %r14: Return address.
	# %r15: Stack pointer.
	basr	%r14, %r10		# Call our caller.

	stm	%r2, %r3, 0x8(%r11)	# Save return registers.

	brasl	%r14, __morestack_block_signals

	# We need a stack slot now, but have no good way to get it - the frame
	# on new stack had to be exactly 0x60 bytes, or stack parameters would
	# be passed wrong.  Abuse fpr save area in caller's frame (we don't
	# save actual fprs).
	la	%r2, 0x40(%r11)
	brasl	%r14, __generic_releasestack

	s	%r2, 0x40(%r11)		# Subtract available space.
	ahi	%r2, BACKOFF		# Back off a bit.
	ear	%r1, %a0		# Extract thread pointer.
.LEHE0:
	st	%r2, 0x20(%r1)	# Save the new stack boundary.

	# We need to restore the old stack pointer before unblocking signals.
	# We also need 0x60 bytes for a stack frame.  Since we had a stack
	# frame at this place before the stack switch, there's no need to
	# write the back chain again.
	lr	%r15, %r11
	ahi	%r15, -0x60

	brasl	%r14, __morestack_unblock_signals

	lm	%r2, %r15, 0x8(%r11)	# Restore all registers.
	.cfi_remember_state
	.cfi_restore %r15
	.cfi_restore %r14
	.cfi_restore %r13
	.cfi_restore %r12
	.cfi_restore %r11
	.cfi_restore %r10
	.cfi_restore %r9
	.cfi_restore %r8
	.cfi_restore %r7
	.cfi_restore %r6
	.cfi_def_cfa_register %r15
	br	%r14			# Return to caller's caller.

# Executed if no new stack allocation is needed.

.Lnoalloc:
	.cfi_restore_state
	# We may need to copy stack parameters.
	l	%r9, 0x4(%r10)		# Load stack parameter size.
	ltr	%r9, %r9		# And check if it's 0.
	je	.Lnostackparm		# Skip the copy if not needed.
	sr	%r15, %r9		# Make space on the stack.
	la	%r8, 0x60(%r15)		# Destination.
	la	%r12, 0x60(%r11)	# Source.
	lr	%r13, %r9		# Source size.
.Lcopy:
	mvcle	%r8, %r12, 0		# Copy.
	jo	.Lcopy

.Lnostackparm:
	# Third parameter is address of function meat - address of parameter
	# block.
	a	%r10, 0x8(%r10)

	# Leave vararg pointer in %r1, in case function uses it
	la	%r1, 0x60(%r11)

	# OK, no stack allocation needed.  We still follow the protocol and
	# call our caller - it doesn't cost much and makes sure vararg works.
	# No need to set any registers here - %r0 and %r2-%r6 weren't modified.
	basr	%r14, %r10		# Call our caller.

	lm	%r6, %r15, 0x18(%r11)	# Restore all callee-saved registers.
	.cfi_remember_state
	.cfi_restore %r15
	.cfi_restore %r14
	.cfi_restore %r13
	.cfi_restore %r12
	.cfi_restore %r11
	.cfi_restore %r10
	.cfi_restore %r9
	.cfi_restore %r8
	.cfi_restore %r7
	.cfi_restore %r6
	.cfi_def_cfa_register %r15
	br	%r14			# Return to caller's caller.

# This is the cleanup code called by the stack unwinder when unwinding
# through the code between .LEHB0 and .LEHE0 above.

.L1:
	.cfi_restore_state
	lr	%r2, %r11		# Stack pointer after resume.
	brasl	%r14, __generic_findstack
	lr	%r3, %r11		# Get the stack pointer.
	sr	%r3, %r2		# Subtract available space.
	ahi	%r3, BACKOFF		# Back off a bit.
	ear	%r1, %a0		# Extract thread pointer.
	st	%r3, 0x20(%r1)	# Save the new stack boundary.

	# We need GOT pointer in %r12 for PLT entry.
	larl	%r12,_GLOBAL_OFFSET_TABLE_
	lr	%r2, %r6		# Exception header.
#ifdef __PIC__
	brasl	%r14, _Unwind_Resume@PLT
#else
	brasl	%r14, _Unwind_Resume
#endif

#else /* defined(__s390x__) */


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

	stmg	%r2, %r15, 0x10(%r15)	# Save %r2-%r15.
	.cfi_offset %r6, -0x70
	.cfi_offset %r7, -0x68
	.cfi_offset %r8, -0x60
	.cfi_offset %r9, -0x58
	.cfi_offset %r10, -0x50
	.cfi_offset %r11, -0x48
	.cfi_offset %r12, -0x40
	.cfi_offset %r13, -0x38
	.cfi_offset %r14, -0x30
	.cfi_offset %r15, -0x28
	lgr	%r11, %r15		# Make frame pointer for vararg.
	.cfi_def_cfa_register %r11
	aghi	%r15, -0xa0		# 0xa0 for standard frame.
	stg	%r11, 0(%r15)		# Save back chain.
	lgr	%r8, %r0		# Save %r0 (static chain).
	lgr	%r10, %r1		# Save %r1 (address of parameter block).

	lg	%r7, 0(%r10)		# Required frame size to %r7
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1		# Extract thread pointer.
	lg	%r1, 0x38(%r1)		# Get stack bounduary
	agr	%r1, %r7		# Stack bounduary + frame size
	ag	%r1, 8(%r10)		# + stack param size
	clgr	%r1, %r15		# Compare with current stack pointer
	jle	.Lnoalloc		# guard > sp - frame-size: need alloc

	brasl	%r14, __morestack_block_signals

	# We abuse one of caller's fpr save slots (which we don't use for fprs)
	# as a local variable.  Not needed here, but done to be consistent with
	# the below use.
	aghi	%r7, BACKOFF		# Bump requested size a bit.
	stg	%r7, 0x80(%r11)		# Stuff frame size on stack.
	la	%r2, 0x80(%r11)		# Pass its address as parameter.
	la	%r3, 0xa0(%r11)		# Caller's stack parameters.
	lg	%r4, 8(%r10)		# Size of stack parameters.
	brasl	%r14, __generic_morestack

	lgr	%r15, %r2		# Switch to the new stack.
	aghi	%r15, -0xa0		# Make a stack frame on it.
	stg	%r11, 0(%r15)		# Save back chain.

	sg	%r2, 0x80(%r11)		# The end of stack space.
	aghi	%r2, BACKOFF		# Back off a bit.
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1		# Extract thread pointer.
.LEHB0:
	stg	%r2, 0x38(%r1)	# Save the new stack boundary.

	brasl	%r14, __morestack_unblock_signals

	lgr	%r0, %r8		# Static chain.
	lmg	%r2, %r6, 0x10(%r11)	# Paremeter registers.

	# Third parameter is address of function meat - address of parameter
	# block.
	ag	%r10, 0x10(%r10)

	# Leave vararg pointer in %r1, in case function uses it
	la	%r1, 0xa0(%r11)

	# State of registers:
	# %r0: Static chain from entry.
	# %r1: Vararg pointer.
	# %r2-%r6: Parameters from entry.
	# %r7-%r10: Indeterminate.
	# %r11: Frame pointer (%r15 from entry).
	# %r12-%r13: Indeterminate.
	# %r14: Return address.
	# %r15: Stack pointer.
	basr	%r14, %r10		# Call our caller.

	stg	%r2, 0x10(%r11)		# Save return register.

	brasl	%r14, __morestack_block_signals

	# We need a stack slot now, but have no good way to get it - the frame
	# on new stack had to be exactly 0xa0 bytes, or stack parameters would
	# be passed wrong.  Abuse fpr save area in caller's frame (we don't
	# save actual fprs).
	la	%r2, 0x80(%r11)
	brasl	%r14, __generic_releasestack

	sg	%r2, 0x80(%r11)		# Subtract available space.
	aghi	%r2, BACKOFF		# Back off a bit.
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1		# Extract thread pointer.
.LEHE0:
	stg	%r2, 0x38(%r1)	# Save the new stack boundary.

	# We need to restore the old stack pointer before unblocking signals.
	# We also need 0xa0 bytes for a stack frame.  Since we had a stack
	# frame at this place before the stack switch, there's no need to
	# write the back chain again.
	lgr	%r15, %r11
	aghi	%r15, -0xa0

	brasl	%r14, __morestack_unblock_signals

	lmg	%r2, %r15, 0x10(%r11)	# Restore all registers.
	.cfi_remember_state
	.cfi_restore %r15
	.cfi_restore %r14
	.cfi_restore %r13
	.cfi_restore %r12
	.cfi_restore %r11
	.cfi_restore %r10
	.cfi_restore %r9
	.cfi_restore %r8
	.cfi_restore %r7
	.cfi_restore %r6
	.cfi_def_cfa_register %r15
	br	%r14			# Return to caller's caller.

# Executed if no new stack allocation is needed.

.Lnoalloc:
	.cfi_restore_state
	# We may need to copy stack parameters.
	lg	%r9, 0x8(%r10)		# Load stack parameter size.
	ltgr	%r9, %r9		# Check if it's 0.
	je	.Lnostackparm		# Skip the copy if not needed.
	sgr	%r15, %r9		# Make space on the stack.
	la	%r8, 0xa0(%r15)		# Destination.
	la	%r12, 0xa0(%r11)	# Source.
	lgr	%r13, %r9		# Source size.
.Lcopy:
	mvcle	%r8, %r12, 0		# Copy.
	jo	.Lcopy

.Lnostackparm:
	# Third parameter is address of function meat - address of parameter
	# block.
	ag	%r10, 0x10(%r10)

	# Leave vararg pointer in %r1, in case function uses it
	la	%r1, 0xa0(%r11)

	# OK, no stack allocation needed.  We still follow the protocol and
	# call our caller - it doesn't cost much and makes sure vararg works.
	# No need to set any registers here - %r0 and %r2-%r6 weren't modified.
	basr	%r14, %r10		# Call our caller.

	lmg	%r6, %r15, 0x30(%r11)	# Restore all callee-saved registers.
	.cfi_remember_state
	.cfi_restore %r15
	.cfi_restore %r14
	.cfi_restore %r13
	.cfi_restore %r12
	.cfi_restore %r11
	.cfi_restore %r10
	.cfi_restore %r9
	.cfi_restore %r8
	.cfi_restore %r7
	.cfi_restore %r6
	.cfi_def_cfa_register %r15
	br	%r14			# Return to caller's caller.

# This is the cleanup code called by the stack unwinder when unwinding
# through the code between .LEHB0 and .LEHE0 above.

.L1:
	.cfi_restore_state
	lgr	%r2, %r11		# Stack pointer after resume.
	brasl	%r14, __generic_findstack
	lgr	%r3, %r11		# Get the stack pointer.
	sgr	%r3, %r2		# Subtract available space.
	aghi	%r3, BACKOFF		# Back off a bit.
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1		# Extract thread pointer.
	stg	%r3, 0x38(%r1)	# Save the new stack boundary.

	lgr	%r2, %r6		# Exception header.
#ifdef __PIC__
	brasl	%r14, _Unwind_Resume@PLT
#else
	brasl	%r14, _Unwind_Resume
#endif

#endif /* defined(__s390x__) */

	.cfi_endproc
	.size	__morestack, . - __morestack


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



# Initialize the stack test value when the program starts or when a
# new thread starts.  We don't know how large the main stack is, so we
# guess conservatively.  We might be able to use getrlimit here.

	.text
	.global	__stack_split_initialize
	.hidden	__stack_split_initialize

	.type	__stack_split_initialize, @function

__stack_split_initialize:

#ifndef __s390x__

	ear	%r1, %a0
	lr	%r0, %r15
	ahi	%r0, -0x4000	# We should have at least 16K.
	st	%r0, 0x20(%r1)

	lr	%r2, %r15
	lhi	%r3, 0x4000
#ifdef __PIC__
	jg	__generic_morestack_set_initial_sp@PLT	# Tail call
#else
	jg	__generic_morestack_set_initial_sp	# Tail call
#endif

#else /* defined(__s390x__) */

	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1
	lgr	%r0, %r15
	aghi	%r0, -0x4000	# We should have at least 16K.
	stg	%r0, 0x38(%r1)

	lgr	%r2, %r15
	lghi	%r3, 0x4000
#ifdef __PIC__
	jg	__generic_morestack_set_initial_sp@PLT	# Tail call
#else
	jg	__generic_morestack_set_initial_sp	# Tail call
#endif

#endif /* defined(__s390x__) */

	.size	__stack_split_initialize, . - __stack_split_initialize

# Routines to get and set the guard, for __splitstack_getcontext,
# __splitstack_setcontext, and __splitstack_makecontext.

# void *__morestack_get_guard (void) returns the current stack guard.
	.text
	.global	__morestack_get_guard
	.hidden	__morestack_get_guard

	.type	__morestack_get_guard,@function

__morestack_get_guard:

#ifndef __s390x__
	ear	%r1, %a0
	l	%r2, 0x20(%r1)
#else
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1
	lg	%r2, 0x38(%r1)
#endif
	br %r14

	.size	__morestack_get_guard, . - __morestack_get_guard

# void __morestack_set_guard (void *) sets the stack guard.
	.global	__morestack_set_guard
	.hidden	__morestack_set_guard

	.type	__morestack_set_guard,@function

__morestack_set_guard:

#ifndef __s390x__
	ear	%r1, %a0
	st	%r2, 0x20(%r1)
#else
	ear	%r1, %a0
	sllg	%r1, %r1, 32
	ear	%r1, %a1
	stg	%r2, 0x38(%r1)
#endif
	br	%r14

	.size	__morestack_set_guard, . - __morestack_set_guard

# void *__morestack_make_guard (void *, size_t) returns the stack
# guard value for a stack.
	.global	__morestack_make_guard
	.hidden	__morestack_make_guard

	.type	__morestack_make_guard,@function

__morestack_make_guard:

#ifndef __s390x__
	sr	%r2, %r3
	ahi	%r2, BACKOFF
#else
	sgr	%r2, %r3
	aghi	%r2, BACKOFF
#endif
	br	%r14

	.size	__morestack_make_guard, . - __morestack_make_guard

# Make __stack_split_initialize a high priority constructor.

	.section .ctors.65535,"aw",@progbits

#ifndef __LP64__
	.align	4
	.long	__stack_split_initialize
	.long	__morestack_load_mmap
#else
	.align	8
	.quad	__stack_split_initialize
	.quad	__morestack_load_mmap
#endif

	.section	.note.GNU-stack,"",@progbits
	.section	.note.GNU-split-stack,"",@progbits
	.section	.note.GNU-no-split-stack,"",@progbits
