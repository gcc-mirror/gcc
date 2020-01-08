/* Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include "asmcfi.h"

	.text
	.align	2
	.global	_ITM_beginTransaction
	.type	_ITM_beginTransaction, %function

_ITM_beginTransaction:
	cfi_startproc
	mov	x1, sp
	stp	x29, x30, [sp, -11*16]!
	cfi_adjust_cfa_offset(11*16)
	cfi_rel_offset(x29, 0)
	cfi_rel_offset(x30, 8)
	mov	x29, sp
	stp	x19, x20, [sp, 1*16]
	stp	x21, x22, [sp, 2*16]
	stp	x23, x24, [sp, 3*16]
	stp	x25, x26, [sp, 4*16]
	stp	x27, x28, [sp, 5*16]
	stp	d8, d9,   [sp, 6*16]
	stp	d10, d11, [sp, 7*16]
	stp	d12, d13, [sp, 8*16]
	stp	d14, d15, [sp, 9*16]
	str	x1, [sp, 10*16]

	/* Invoke GTM_begin_transaction with the struct we just built.  */
	mov	x1, sp
	bl	GTM_begin_transaction

	/* Return; we don't need to restore any of the call-saved regs.  */
	ldp	x29, x30, [sp], 11*16
	cfi_adjust_cfa_offset(-11*16)
	cfi_restore(x29)
	cfi_restore(x30)
	ret
	cfi_endproc
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.align	2
	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, %function

GTM_longjmp:
	/* The first parameter becomes the return value (x0).
	   The third parameter is ignored for now.  */
	cfi_startproc
	ldp	x19, x20, [x1, 1*16]
	ldp	x21, x22, [x1, 2*16]
	ldp	x23, x24, [x1, 3*16]
	ldp	x25, x26, [x1, 4*16]
	ldp	x27, x28, [x1, 5*16]
	ldp	d8, d9,   [x1, 6*16]
	ldp	d10, d11, [x1, 7*16]
	ldp	d12, d13, [x1, 8*16]
	ldp	d14, d15, [x1, 9*16]
	ldr	x3, [x1, 10*16]
	ldp	x29, x30, [x1]
	cfi_def_cfa(x1, 0)
	mov	sp, x3
	br	x30
	cfi_endproc
	.size	GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", %progbits
#endif
