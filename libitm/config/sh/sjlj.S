/* Copyright (C) 2011-2024 Free Software Foundation, Inc.

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

#include "asmcfi.h"

	.text
	.align	2
	.global	_ITM_beginTransaction
	.type	_ITM_beginTransaction, %function

_ITM_beginTransaction:
	cfi_startproc
	mov	r15, r1
#ifdef __SH_FPU_ANY__
	fmov.s	fr15, @-r15
	fmov.s	fr14, @-r15
	fmov.s	fr13, @-r15
	fmov.s	fr12, @-r15
	sts.l	fpscr, @-r15
#endif /* __SH_FPU_ANY__ */
	stc.l	gbr, @-r15
	sts.l	pr, @-r15
	mov.l	r1, @-r15
	mov.l	r14, @-r15
	mov.l	r13, @-r15
	mov.l	r12, @-r15
	mov.l	r11, @-r15
	mov.l	r10, @-r15
	mov.l	r9, @-r15
	mov.l	r8, @-r15
#ifdef __SH_FPU_ANY__
	cfi_def_cfa_offset (4*15)
#else
	cfi_def_cfa_offset (4*10)
#endif
#if !defined __PIC__
	mov.l	.Lbegin, r1
	jsr	@r1
	 mov	r15, r5
#else
	mov.l	.Lbegin, r1
	bsrf	r1
	 mov	r15, r5
.Lbegin0:
	mov.l	@(4*4,r15), r12
#endif
	mov.l	@(8*4,r15), r1
	lds	r1, pr
#ifdef __SH_FPU_ANY__
	add	#(15*4), r15
#else
	add	#(10*5), r15
#endif
	cfi_def_cfa_offset (0)
	rts
	 nop
	cfi_endproc

	.align  2
.Lbegin:
#if !defined __PIC__
	.long	GTM_begin_transaction
#else
	.long	GTM_begin_transaction@PCREL-(.Lbegin0-.)
#endif
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, %function

GTM_longjmp:
	mov.l	@r5+, r8
	mov.l	@r5+, r9
	mov.l	@r5+, r10
	mov.l	@r5+, r11
	mov.l	@r5+, r12
	mov.l	@r5+, r13
	mov.l	@r5+, r14
	mov.l	@r5+, r15
	lds.l	@r5+, pr
	ldc.l	@r5+, gbr
#ifdef __SH_FPU_ANY__
	lds.l	@r5+, fpscr
	fmov.s	@r5+, fr12
	fmov.s	@r5+, fr13
	fmov.s	@r5+, fr14
	fmov.s	@r5+, fr15
#endif
	rts
	 mov	r4, r0

	.size	GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", %progbits
#endif
