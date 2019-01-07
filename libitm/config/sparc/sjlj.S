/* Copyright (C) 2012-2019 Free Software Foundation, Inc.

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

#ifdef __arch64__
# define WORD_SIZE 8
# define MIN_FRAME_SIZE 176
# define STACK_BIAS 2047
# define load  ldx
# define store stx
#else
# define WORD_SIZE 4
# define MIN_FRAME_SIZE 96
# define STACK_BIAS 0
# define load  ld
# define store st
#endif

/* Fields of the JmpBuf structure.  */
#define JB_CFA 0
#define JB_PC  1
#define OFFSET(FIELD) ((FIELD) * WORD_SIZE)

/* The frame size must be a multiple of the double-word size.  */
#define FRAME_SIZE (MIN_FRAME_SIZE + 2 * WORD_SIZE)
#define JB_OFFSET  (STACK_BIAS + MIN_FRAME_SIZE)

	.text
	.align 4
	.globl	_ITM_beginTransaction
	.type	_ITM_beginTransaction, #function
	.proc	016
_ITM_beginTransaction:
	cfi_startproc
	add	%sp, STACK_BIAS, %g1
	sub	%sp, FRAME_SIZE, %sp
	cfi_def_cfa_offset(STACK_BIAS + FRAME_SIZE)
	store	%g1, [%sp + JB_OFFSET + OFFSET (JB_CFA)]
	store	%o7, [%sp + JB_OFFSET + OFFSET (JB_PC)]
	/* ??? This triggers an internal error in GDB.  */
	cfi_offset(%o7, -WORD_SIZE)
	call	GTM_begin_transaction
	 add	%sp, JB_OFFSET, %o1
	load	[%sp + JB_OFFSET + OFFSET (JB_PC)], %o7
	jmp	%o7+8
	 add	%sp, FRAME_SIZE, %sp
	cfi_def_cfa_offset(STACK_BIAS)
	cfi_endproc
	.size _ITM_beginTransaction, . - _ITM_beginTransaction

	.align 4
	.globl	GTM_longjmp
#ifdef HAVE_ATTRIBUTE_VISIBILITY
	.hidden	GTM_longjmp
#endif
	.type	GTM_longjmp, #function
	.proc	016
GTM_longjmp:
	cfi_startproc
	flushw
#if STACK_BIAS
	load	[%o1 + OFFSET (JB_CFA)], %g1
	sub	%g1, STACK_BIAS, %fp
#else
	load	[%o1 + OFFSET (JB_CFA)], %fp
#endif
	cfi_def_cfa(%fp, STACK_BIAS)
	load	[%o1 + OFFSET (JB_PC)], %o7
	jmp	%o7+8
	 restore %g0, %o0, %o0
	cfi_endproc
	.size GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
	.section .note.GNU-stack, "", @progbits
#endif
