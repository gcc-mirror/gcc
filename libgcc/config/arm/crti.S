#   Copyright (C) 2001-2019 Free Software Foundation, Inc.
#   Written By Nick Clifton
# 
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

/* An executable stack is *not* required for these functions.  */
#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

# This file just make a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

#ifdef __ELF__
#define TYPE(x) .type x,function
#else
#define TYPE(x)
#endif
#ifdef __ARM_EABI__
/* Some attributes that are common to all routines in this file.  */
	/* Tag_ABI_align_needed: This code does not require 8-byte
	   alignment from the caller.  */
	/* .eabi_attribute 24, 0  -- default setting.  */
	/* Tag_ABI_align_preserved: This code preserves 8-byte
	   alignment in any callee.  */
	.eabi_attribute 25, 1
#endif /* __ARM_EABI__ */

	# Note - this macro is complemented by the FUNC_END macro
	# in crtn.S.  If you change this macro you must also change
	# that macro match.
.macro FUNC_START
#ifdef __thumb__
	.thumb
	
	push	{r3, r4, r5, r6, r7, lr}
#else
	.arm
	#  Create a stack frame and save any call-preserved registers
	mov	ip, sp
	stmdb	sp!, {r3, r4, r5, r6, r7, r8, r9, sl, fp, ip, lr, pc}
	sub	fp, ip, #4
#endif
.endm
		
	.section	".init"
	.align 2
	.global	_init
#ifdef __thumb__
	.thumb_func
#endif
	TYPE(_init)
_init:
	FUNC_START
	
		
	.section	".fini"
	.align	2
	.global	_fini
#ifdef __thumb__
	.thumb_func
#endif
	TYPE(_fini)
_fini:
	FUNC_START
	
# end of crti.S
