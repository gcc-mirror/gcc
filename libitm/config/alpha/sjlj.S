/* Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

	.text
	.align 4
	.globl	_ITM_beginTransaction
	.ent	_ITM_beginTransaction

#define FRAME	144

_ITM_beginTransaction:
	ldgp	$29, 0($27)
	subq	$30, FRAME, $30
	.frame	$30, FRAME, $26, 0
	.mask	0x04000000, 0
	stq	$26, 0($30)
	.prologue 1

	stq	$9, 8($30)
	stq	$10, 16($30)
	addq	$30, FRAME, $0
	stq	$11, 24($30)

	stq	$12, 32($30)
	stq	$13, 40($30)
	stq	$14, 48($30)
	stq	$15, 56($30)

	stq	$0, 64($30)
	stt	$f2, 72($30)
	stt	$f3, 80($30)
	stt	$f4, 88($30)

	stt	$f5, 96($30)
	stt	$f6, 104($30)
	stt	$f7, 112($30)
	stt	$f8, 120($30)

	stt	$f9, 128($30)
	mov	$30, $17
#ifdef __PIC__
	unop
	bsr	$26, GTM_begin_transaction !samegp
#else
	jsr	$26, GTM_begin_transaction
	ldgp	$29, 0($26)
#endif

	ldq	$26, 0($30)
	addq	$30, FRAME, $30
	ret
.end _ITM_beginTransaction

	.align 4
	.globl	GTM_longjmp
#ifdef __ELF__
	.hidden	GTM_longjmp
#endif
	.ent	GTM_longjmp

GTM_longjmp:
	.prologue 0
	ldq	$26, 0($17)
	ldq	$9, 8($17)
	ldq	$10, 16($17)
	ldq	$11, 24($17)

	ldq	$12, 32($17)
	ldq	$13, 40($17)
	ldq	$14, 48($17)
	ldq	$15, 56($17)

	ldq	$1, 64($17)
	ldt	$f2, 72($17)
	ldt	$f3, 80($17)
	ldt	$f4, 88($17)

	ldt	$f5, 96($17)
	ldt	$f6, 104($17)
	ldt	$f7, 112($17)
	ldt	$f8, 120($17)

	ldt	$f9, 128($17)
	mov	$16, $0
	mov	$1, $30
	ret
.end GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
