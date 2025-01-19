/* Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Xiongchuan Tan <xc-tan@outlook.com>.

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
#include "asm.h"

	.text
	.align	2
	.global	_ITM_beginTransaction
	.type	_ITM_beginTransaction, @function

_ITM_beginTransaction:
	cfi_startproc
	mv a1, sp
	addi sp, sp, -ADJ_STACK_SIZE
	cfi_adjust_cfa_offset(ADJ_STACK_SIZE)

	/* Return Address */
	GPR_S ra, 0*SZ_GPR(sp)
	cfi_rel_offset(ra, 0*SZ_GPR)

	/* Caller's sp */
	GPR_S a1, 1*SZ_GPR(sp)

	/* Caller's s0/fp */
	GPR_S fp, 2*SZ_GPR(sp)
	cfi_rel_offset(fp, 2*SZ_GPR)

	/* Callee-saved registers */
	GPR_S s1, 3*SZ_GPR(sp)
	GPR_S s2, 4*SZ_GPR(sp)
	GPR_S s3, 5*SZ_GPR(sp)
	GPR_S s4, 6*SZ_GPR(sp)
	GPR_S s5, 7*SZ_GPR(sp)
	GPR_S s6, 8*SZ_GPR(sp)
	GPR_S s7, 9*SZ_GPR(sp)
	GPR_S s8, 10*SZ_GPR(sp)
	GPR_S s9, 11*SZ_GPR(sp)
	GPR_S s10, 12*SZ_GPR(sp)
	GPR_S s11, 13*SZ_GPR(sp)

#if defined(__riscv_flen)
	/* Callee-saved floating-point registers */
	FPR_S fs0, 0*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs1, 1*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs2, 2*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs3, 3*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs4, 4*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs5, 5*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs6, 6*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs7, 7*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs8, 8*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs9, 9*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs10, 10*SZ_FPR+LEN_GPR*SZ_GPR(sp)
	FPR_S fs11, 11*SZ_FPR+LEN_GPR*SZ_GPR(sp)
#endif
	mv fp, sp

	/* Invoke GTM_begin_transaction with the struct we've just built.  */
	mv a1, sp
	jal ra, GTM_begin_transaction

	/* Return; we don't need to restore any of the call-saved regs.  */
	GPR_L ra, 0*SZ_GPR(sp)
	cfi_restore(ra)
	GPR_L fp, 2*SZ_GPR(sp)
	cfi_restore(fp)

	addi sp, sp, ADJ_STACK_SIZE
	cfi_adjust_cfa_offset(-ADJ_STACK_SIZE)

	ret
	cfi_endproc
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.align	2
	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, @function

GTM_longjmp:
	/* The first parameter becomes the return value (a0).
	   The third parameter is ignored for now.  */
    cfi_startproc
	GPR_L s1, 3*SZ_GPR(a1)
	GPR_L s2, 4*SZ_GPR(a1)
	GPR_L s3, 5*SZ_GPR(a1)
	GPR_L s4, 6*SZ_GPR(a1)
	GPR_L s5, 7*SZ_GPR(a1)
	GPR_L s6, 8*SZ_GPR(a1)
	GPR_L s7, 9*SZ_GPR(a1)
	GPR_L s8, 10*SZ_GPR(a1)
	GPR_L s9, 11*SZ_GPR(a1)
	GPR_L s10, 12*SZ_GPR(a1)
	GPR_L s11, 13*SZ_GPR(a1)

#if defined(__riscv_flen)
	FPR_L fs0, 0*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs1, 1*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs2, 2*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs3, 3*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs4, 4*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs5, 5*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs6, 6*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs7, 7*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs8, 8*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs9, 9*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs10, 10*SZ_FPR+LEN_GPR*SZ_GPR(a1)
	FPR_L fs11, 11*SZ_FPR+LEN_GPR*SZ_GPR(a1)
#endif

	GPR_L ra, 0*SZ_GPR(a1)
	GPR_L fp, 2*SZ_GPR(a1)
	GPR_L a3, 1*SZ_GPR(a1)
	cfi_def_cfa(a1, 0)
	mv sp, a3
	jr ra
	cfi_endproc
	.size	GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
