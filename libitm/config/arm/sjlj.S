/* Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

#include "hwcap.h"
#include "asmcfi.h"

	.syntax	unified

#if defined(__thumb2__)
# define PC_OFS 4
        .thumb
        .thumb_func
#else
# define PC_OFS 8
#endif

#if defined (__thumb2__) && defined(__ARM_ARCH_6T2__)
# define HAVE_MOVT
	.arch	armv6t2
#elif defined (__ARM_ARCH_7A__)
# define HAVE_MOVT
	.arch	armv7-a
#elif defined (__ARM_ARCH_7R__)
# define HAVE_MOVT
	.arch	armv7-r
#elif defined (__ARM_ARCH_7M__)
# define HAVE_MOVT
	.arch	armv7-m
#endif

#if defined(HAVE_MOVT) && defined(PIC)
.macro ldaddr reg, addr
	movw	\reg, #:lower16:(\addr - (98f + PC_OFS))
	movt	\reg, #:upper16:(\addr - (98f + PC_OFS))
98:	add	\reg, \reg, pc
.endm
#elif defined(HAVE_MOVT)
.macro ldaddr reg, addr
	movw	\reg, #:lower16:\addr
	movt	\reg, #:upper16:\addr
.endm
#elif defined(PIC)
.macro ldaddr reg, addr
	ldr	\reg, 99f
98:	add	\reg, \reg, pc
.subsection 1
	.align	2
99:	.word	\addr - (98b + PC_OFS)
.subsection 0
.endm
#else
.macro ldaddr reg, addr
	ldr	\reg, =\addr
.endm
#endif

	.text
	.align	2
	.global	_ITM_beginTransaction
	.type	_ITM_beginTransaction, %function

_ITM_beginTransaction:
	.fnstart
	cfi_startproc
	mov	ip, sp
	push	{ r4-r11, ip, lr }
	.save	{ lr }
	.pad	#(9*4)
	cfi_adjust_cfa_offset(40)
	cfi_rel_offset(lr, 36)
	sub	sp, sp, #(14*8)
	.pad	#(14*8)
	cfi_adjust_cfa_offset(14*8)

	ldaddr	r2, GTM_hwcap
	ldr	r2, [r2]

	/* Store the VFP registers.  Don't use VFP instructions directly
	   because this code is used in non-VFP multilibs.  */
	tst	r2, #HWCAP_ARM_VFP
	beq	1f
	stc	p11, cr8, [sp], {16}	/* vstm sp, {d8-d15} */
1:
	/* Save the call-preserved iWMMXt registers.  */
	tst	r2, #HWCAP_ARM_IWMMXT
	beq	1f
	stcl	p1, cr10, [sp, #64]	/* wstrd wr10, [sp, #64] */
	stcl	p1, cr11, [sp, #72]
	stcl	p1, cr12, [sp, #80]
	stcl	p1, cr13, [sp, #88]
	stcl	p1, cr14, [sp, #96]
	stcl	p1, cr15, [sp, #104]
1:
	/* Invoke GTM_begin_transaction with the struct we just built.  */
	mov	r1, sp
	bl	GTM_begin_transaction

	/* Return; we don't need to restore any of the call-saved regs.  */
	add     sp, sp, #(14*8 + 9*4)
	cfi_adjust_cfa_offset(-(14*8 + 9*4))
	pop	{ pc }
	.fnend
	cfi_endproc
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.align	2
	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, %function

GTM_longjmp:
	cfi_startproc
	ldaddr	r2, GTM_hwcap
	ldr	r2, [r2]

	tst	r2, #HWCAP_ARM_VFP
	beq	1f
	ldc	p11, cr8, [r1], {16}	/* vldmia r1, {d8-d15} */
1:
	tst	r2, #HWCAP_ARM_IWMMXT
	beq	1f
	ldcl	p1, cr10, [r1, #64]	/* wldrd wr10, [r1, #64] */
	ldcl	p1, cr11, [r1, #72]
	ldcl	p1, cr12, [r1, #80]
	ldcl	p1, cr13, [r1, #88]
	ldcl	p1, cr14, [r1, #96]
	ldcl	p1, cr15, [r1, #104]
1:
	add	r1, r1, #(14*8)		/* Skip both VFP and iWMMXt blocks */
#ifdef __thumb2__
	ldm	r1, { r4-r11, ip, lr }
	cfi_def_cfa(ip, 0)
	mov	sp, ip
	bx	lr
#else
	ldm	r1, { r4-r11, sp, pc }
#endif
	cfi_endproc
	.size	GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", %progbits
#endif
