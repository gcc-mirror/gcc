#ifdef __powerpc64__
# PowerPC64 support for -fsplit-stack.
# Copyright (C) 2009-2017 Free Software Foundation, Inc.
# Contributed by Alan Modra <amodra@gmail.com>.

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

#if _CALL_ELF == 2
	.abiversion 2
#define PARAMS 32
#else
#define PARAMS 48
#endif
#define MORESTACK_FRAMESIZE	(PARAMS+96)
#define PARAMREG_SAVE		-MORESTACK_FRAMESIZE+PARAMS+0
#define STATIC_CHAIN_SAVE	-MORESTACK_FRAMESIZE+PARAMS+64
#define R29_SAVE		-MORESTACK_FRAMESIZE+PARAMS+72
#define LINKREG_SAVE		-MORESTACK_FRAMESIZE+PARAMS+80
#define NEWSTACKSIZE_SAVE	-MORESTACK_FRAMESIZE+PARAMS+88

# Excess space needed to call ld.so resolver for lazy plt
# resolution.  Go uses sigaltstack so this doesn't need to
# also cover signal frame size.
#define BACKOFF 4096
# Large excess allocated when calling non-split-stack code.
#define NON_SPLIT_STACK 0x100000


#if _CALL_ELF == 2

#define BODY_LABEL(name) name

#define ENTRY0(name)					\
	.global name;					\
	.hidden	name;					\
	.type name,@function;				\
name##:

#define ENTRY(name)					\
	ENTRY0(name);					\
0:	addis %r2,%r12,.TOC.-0b@ha;			\
        addi %r2,%r2,.TOC.-0b@l;			\
	.localentry name, .-name

#else

#define BODY_LABEL(name) .L.##name

#define ENTRY0(name)					\
	.global name;					\
	.hidden	name;					\
	.type name,@function;				\
	.pushsection ".opd","aw";			\
	.p2align 3;					\
name##: .quad BODY_LABEL (name), .TOC.@tocbase, 0;	\
	.popsection;					\
BODY_LABEL(name)##:

#define ENTRY(name) ENTRY0(name)

#endif

#define SIZE(name) .size name, .-BODY_LABEL(name)


	.text
# Just like __morestack, but with larger excess allocation
ENTRY0(__morestack_non_split)
.LFB1:
	.cfi_startproc
# We use a cleanup to restore the tcbhead_t.__private_ss if
# an exception is thrown through this code.
#ifdef __PIC__
	.cfi_personality 0x9b,DW.ref.__gcc_personality_v0
	.cfi_lsda 0x1b,.LLSDA1
#else
	.cfi_personality 0x3,__gcc_personality_v0
	.cfi_lsda 0x3,.LLSDA1
#endif
# LR is already saved by the split-stack prologue code.
# We may as well have the unwinder skip over the call in the
# prologue too.
	.cfi_offset %lr,16

	addis %r12,%r12,-NON_SPLIT_STACK@h
	SIZE (__morestack_non_split)
# Fall through into __morestack


# This function is called with non-standard calling conventions.
# On entry, r12 is the requested stack pointer.  One version of the
# split-stack prologue that calls __morestack looks like
#	ld %r0,-0x7000-64(%r13)
#	addis %r12,%r1,-allocate@ha
#	addi %r12,%r12,-allocate@l
#	cmpld %r12,%r0
#	bge+ enough
#	mflr %r0
#	std %r0,16(%r1)
#	bl __morestack
#	ld %r0,16(%r1)
#	mtlr %r0
#	blr
# enough:
# The normal function prologue follows here, with a small addition at
# the end to set up the arg pointer.  The arg pointer is set up with:
#	addi %r12,%r1,offset
#	bge %cr7,.+8
#	mr %r12,%r29
#
# Note that the lr save slot 16(%r1) has already been used.
# r3 thru r11 possibly contain arguments and a static chain
# pointer for the function we're calling, so must be preserved.
# cr7 must also be preserved.

ENTRY0(__morestack)
# Save parameter passing registers, our arguments, lr, r29
# and use r29 as a frame pointer.
	std %r3,PARAMREG_SAVE+0(%r1)
	sub %r3,%r1,%r12		# calculate requested stack size
	mflr %r12
	std %r4,PARAMREG_SAVE+8(%r1)
	std %r5,PARAMREG_SAVE+16(%r1)
	std %r6,PARAMREG_SAVE+24(%r1)
	std %r7,PARAMREG_SAVE+32(%r1)
	addi %r3,%r3,BACKOFF
	std %r8,PARAMREG_SAVE+40(%r1)
	std %r9,PARAMREG_SAVE+48(%r1)
	std %r10,PARAMREG_SAVE+56(%r1)
	std %r11,STATIC_CHAIN_SAVE(%r1)
	std %r29,R29_SAVE(%r1)
	std %r12,LINKREG_SAVE(%r1)
	std %r3,NEWSTACKSIZE_SAVE(%r1)	# new stack size
	mr %r29,%r1
	.cfi_offset %r29,R29_SAVE
	.cfi_def_cfa_register %r29
	stdu %r1,-MORESTACK_FRAMESIZE(%r1)

	# void __morestack_block_signals (void)
	bl __morestack_block_signals

	# void *__generic_morestack (size_t *pframe_size,
	#			     void *old_stack,
	#			     size_t param_size)
	addi %r3,%r29,NEWSTACKSIZE_SAVE
	mr %r4,%r29
	li %r5,0			# no copying from old stack
	bl __generic_morestack

# Start using new stack
	stdu %r29,-32(%r3)		# back-chain
	mr %r1,%r3

# Set __private_ss stack guard for the new stack.
	ld %r12,NEWSTACKSIZE_SAVE(%r29)	# modified size
	addi %r3,%r3,BACKOFF-32
	sub %r3,%r3,%r12
# Note that a signal frame has $pc pointing at the instruction
# where the signal occurred.  For something like a timer
# interrupt this means the instruction has already executed,
# thus the region starts at the instruction modifying
# __private_ss, not one instruction after.
.LEHB0:
	std %r3,-0x7000-64(%r13)	# tcbhead_t.__private_ss

	# void __morestack_unblock_signals (void)
	bl __morestack_unblock_signals

# Set up for a call to the target function, located 3
# instructions after __morestack's return address.
#
	ld %r12,LINKREG_SAVE(%r29)
	ld %r3,PARAMREG_SAVE+0(%r29)	# restore arg regs
	ld %r4,PARAMREG_SAVE+8(%r29)
	ld %r5,PARAMREG_SAVE+16(%r29)
	ld %r6,PARAMREG_SAVE+24(%r29)
	ld %r7,PARAMREG_SAVE+32(%r29)
	ld %r8,PARAMREG_SAVE+40(%r29)
	ld %r9,PARAMREG_SAVE+48(%r29)
	addi %r0,%r12,12		# add 3 instructions
	ld %r10,PARAMREG_SAVE+56(%r29)
	ld %r11,STATIC_CHAIN_SAVE(%r29)
	cmpld %cr7,%r12,%r0		# indicate we were called
	mtctr %r0
	bctrl				# call caller!

# On return, save regs possibly used to return a value, and
# possibly trashed by calls to __morestack_block_signals,
# __generic_releasestack and __morestack_unblock_signals.
# Assume those calls don't use vector or floating point regs.
	std %r3,PARAMREG_SAVE+0(%r29)
	std %r4,PARAMREG_SAVE+8(%r29)
	std %r5,PARAMREG_SAVE+16(%r29)
	std %r6,PARAMREG_SAVE+24(%r29)
#if _CALL_ELF == 2
	std %r7,PARAMREG_SAVE+32(%r29)
	std %r8,PARAMREG_SAVE+40(%r29)
	std %r9,PARAMREG_SAVE+48(%r29)
	std %r10,PARAMREG_SAVE+56(%r29)
#endif

	bl __morestack_block_signals

	# void *__generic_releasestack (size_t *pavailable)
	addi %r3,%r29,NEWSTACKSIZE_SAVE
	bl __generic_releasestack

# Reset __private_ss stack guard to value for old stack
	ld %r12,NEWSTACKSIZE_SAVE(%r29)
	addi %r3,%r3,BACKOFF
	sub %r3,%r3,%r12
.LEHE0:
	std %r3,-0x7000-64(%r13)	# tcbhead_t.__private_ss

	bl __morestack_unblock_signals

# Use old stack again.
	mr %r1,%r29

# Restore return value regs, and return.
	ld %r0,LINKREG_SAVE(%r29)
	mtlr %r0
	ld %r3,PARAMREG_SAVE+0(%r29)
	ld %r4,PARAMREG_SAVE+8(%r29)
	ld %r5,PARAMREG_SAVE+16(%r29)
	ld %r6,PARAMREG_SAVE+24(%r29)
#if _CALL_ELF == 2
	ld %r7,PARAMREG_SAVE+32(%r29)
	ld %r8,PARAMREG_SAVE+40(%r29)
	ld %r9,PARAMREG_SAVE+48(%r29)
	ld %r10,PARAMREG_SAVE+56(%r29)
#endif
	ld %r29,R29_SAVE(%r29)
	.cfi_def_cfa_register %r1
	blr

# This is the cleanup code called by the stack unwinder when
# unwinding through code between .LEHB0 and .LEHE0 above.
cleanup:
	.cfi_def_cfa_register %r29
	std %r3,PARAMREG_SAVE(%r29)	# Save exception header
	# size_t __generic_findstack (void *stack)
	mr %r3,%r29
	bl __generic_findstack
	sub %r3,%r29,%r3
	addi %r3,%r3,BACKOFF
	std %r3,-0x7000-64(%r13)	# tcbhead_t.__private_ss
	ld %r3,PARAMREG_SAVE(%r29)
	bl _Unwind_Resume
	nop
	.cfi_endproc
	SIZE (__morestack)


	.section .gcc_except_table,"a",@progbits
	.p2align 2
.LLSDA1:
	.byte	0xff	# @LPStart format (omit)
	.byte	0xff	# @TType format (omit)
	.byte	0x1	# call-site format (uleb128)
	.uleb128 .LLSDACSE1-.LLSDACSB1	# Call-site table length
.LLSDACSB1:
	.uleb128 .LEHB0-.LFB1	# region 0 start
	.uleb128 .LEHE0-.LEHB0	# length
	.uleb128 cleanup-.LFB1	# landing pad
	.uleb128 0		# no action, ie. a cleanup
.LLSDACSE1:


#ifdef __PIC__
# Build a position independent reference to the personality function.
	.hidden DW.ref.__gcc_personality_v0
	.weak DW.ref.__gcc_personality_v0
	.section .data.DW.ref.__gcc_personality_v0,"awG",@progbits,DW.ref.__gcc_personality_v0,comdat
	.p2align 3
DW.ref.__gcc_personality_v0:
	.quad __gcc_personality_v0
	.type DW.ref.__gcc_personality_v0, @object
	.size DW.ref.__gcc_personality_v0, 8
#endif


	.text
# Initialize the stack guard when the program starts or when a
# new thread starts.  This is called from a constructor.
# void __stack_split_initialize (void)
ENTRY(__stack_split_initialize)
	addi %r3,%r1,-0x4000		# We should have at least 16K.
	std %r3,-0x7000-64(%r13)	# tcbhead_t.__private_ss
	# void __generic_morestack_set_initial_sp (void *sp, size_t len)
	mr %r3,%r1
	li %r4, 0x4000
	b __generic_morestack_set_initial_sp
	SIZE (__stack_split_initialize)


# Return current __private_ss
# void *__morestack_get_guard (void)
ENTRY0(__morestack_get_guard)
	ld %r3,-0x7000-64(%r13)		# tcbhead_t.__private_ss
	blr
	SIZE (__morestack_get_guard)


# Set __private_ss
# void __morestack_set_guard (void *ptr)
ENTRY0(__morestack_set_guard)
	std %r3,-0x7000-64(%r13)	# tcbhead_t.__private_ss
	blr
	SIZE (__morestack_set_guard)


# Return the stack guard value for given stack
# void *__morestack_make_guard (void *stack, size_t size)
ENTRY0(__morestack_make_guard)
	sub %r3,%r3,%r4
	addi %r3,%r3,BACKOFF
	blr
	SIZE (__morestack_make_guard)


# Make __stack_split_initialize a high priority constructor.
	.section .ctors.65535,"aw",@progbits
	.p2align 3
	.quad __stack_split_initialize
	.quad __morestack_load_mmap

	.section .note.GNU-stack,"",@progbits
	.section .note.GNU-split-stack,"",@progbits
	.section .note.GNU-no-split-stack,"",@progbits
#endif /* __powerpc64__ */
