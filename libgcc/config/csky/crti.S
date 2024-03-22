# Define _init and _fini entry points for C-SKY.
# Copyright (C) 2018-2024 Free Software Foundation, Inc.
# Contributed by C-SKY Microsystems and Mentor Graphics.
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


# This file just makes a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

	.file    "crti.S"

/* We use more complicated versions of this code with GLIBC.  */
#if defined(__gnu_linux__)

#ifndef PREINIT_FUNCTION
# define PREINIT_FUNCTION __gmon_start__
#endif

#ifndef PREINIT_FUNCTION_WEAK
# define PREINIT_FUNCTION_WEAK 1
#endif

#if PREINIT_FUNCTION_WEAK
	.global PREINIT_FUNCTION
	.weak PREINIT_FUNCTION
	.align 4
	.type call_weak_fn, %function
call_weak_fn:
	// push  lr
	subi    sp, 4
	stw     lr, (sp)
#ifdef  __PIC__
	lrw     a2, PREINIT_FUNCTION@GOT
	addu    a2, gb
	ldw     a2, (a2)
#else
	lrw     a2, PREINIT_FUNCTION
#endif
	cmpnei  a2, 0
	bf      1f
	jsr     a2
1:
	// pop lr
	ldw     lr, (sp)
	addi    sp, 4
	rts

	.align 4
#else
	.hidden PREINIT_FUNCTION
#endif /* PREINIT_FUNCTION_WEAK */

	.section .init,"ax",@progbits
	.align 4
	.globl _init
	.type _init, @function
_init:
	subi    sp, 8
	stw     lr, (sp, 0)
#ifdef __PIC__
	//  stw     gb, (sp, 4)
	bsr     .Lgetpc
.Lgetpc:
	lrw     gb, .Lgetpc@GOTPC
	add     gb, lr
#endif
#if PREINIT_FUNCTION_WEAK
#ifdef __PIC__
	lrw     a2, call_weak_fn@GOTOFF
	add     a2, gb
	jsr     a2
#else
	jsri    call_weak_fn
#endif
#else /* !PREINIT_FUNCTION_WEAK */
#ifdef  __PIC__
	lrw     a2, PREINIT_FUNCTION@PLT
	addu    a2, gb
	ldw     a2, (a2)
	jsr     a2
#else
	jsri    PREINIT_FUNCTION
#endif
#endif /* PREINIT_FUNCTION_WEAK */

	br      2f
	.literals
	.align  4
2:
	.section .fini,"ax",@progbits
	.align 4
	.globl _fini
	.type _fini, @function
_fini:
	subi    sp,8
	stw     lr, (sp, 0)
	br      2f
	.literals
	.align  4
2:

/* These are the non-GLIBC versions.  */
#else  /* !defined(__gnu_linux__) */
	.section  ".init"
	.global  _init
	.type  _init,@function
	.align  2
_init:
	subi  sp, 16
	st.w  lr, (sp, 12)
	mov     r0, r0

	.section  ".fini"
	.global  _fini
	.type  _fini,@function
	.align  2
_fini:
	subi  sp, 16
	st.w  lr, (sp, 12)
	mov     r0, r0
#endif /* defined(__gnu_linux__) */
