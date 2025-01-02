/* Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Loongson Co. Ltd.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

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
        move   $r5, $sp
        ADDSP(-(12*SZ_GPR+8*SZ_FPR))
        cfi_adjust_cfa_offset(12*SZ_GPR+8*SZ_FPR)

        /* Frame Pointer */
        GPR_S  $fp, $sp, 0*SZ_GPR
        cfi_rel_offset(22, 0)

        /* Return Address */
        GPR_S  $r1, $sp, 1*SZ_GPR
        cfi_rel_offset(1, SZ_GPR)

        /* Caller's $sp */
        GPR_S  $r5, $sp, 2*SZ_GPR

        /* Callee-saved scratch GPRs (r23-r31) */
        GPR_S  $s0, $sp, 3*SZ_GPR
        GPR_S  $s1, $sp, 4*SZ_GPR
        GPR_S  $s2, $sp, 5*SZ_GPR
        GPR_S  $s3, $sp, 6*SZ_GPR
        GPR_S  $s4, $sp, 7*SZ_GPR
        GPR_S  $s5, $sp, 8*SZ_GPR
        GPR_S  $s6, $sp, 9*SZ_GPR
        GPR_S  $s7, $sp, 10*SZ_GPR
        GPR_S  $s8, $sp, 11*SZ_GPR

#if !defined(__loongarch_soft_float)
        /* Callee-saved scratch FPRs (f24-f31) */
        FPR_S  $f24, $sp, 12*SZ_GPR + 0*SZ_FPR
        FPR_S  $f25, $sp, 12*SZ_GPR + 1*SZ_FPR
        FPR_S  $f26, $sp, 12*SZ_GPR + 2*SZ_FPR
        FPR_S  $f27, $sp, 12*SZ_GPR + 3*SZ_FPR
        FPR_S  $f28, $sp, 12*SZ_GPR + 4*SZ_FPR
        FPR_S  $f29, $sp, 12*SZ_GPR + 5*SZ_FPR
        FPR_S  $f30, $sp, 12*SZ_GPR + 6*SZ_FPR
        FPR_S  $f31, $sp, 12*SZ_GPR + 7*SZ_FPR
#endif
        move   $fp, $sp

        /* Invoke GTM_begin_transaction with the struct we've just built.  */
        move   $r5, $sp
        bl     %plt(GTM_begin_transaction)

        /* Return. (no call-saved scratch reg needs to be restored here)  */
        GPR_L  $fp, $sp, 0*SZ_GPR
        cfi_restore(22)
        GPR_L  $r1, $sp, 1*SZ_GPR
        cfi_restore(1)

        ADDSP(12*SZ_GPR+8*SZ_FPR)
        cfi_adjust_cfa_offset(-(12*SZ_GPR+8*SZ_FPR))

        jr     $r1
        cfi_endproc
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.align	2
	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, @function

GTM_longjmp:
        cfi_startproc
        GPR_L  $s0, $r5, 3*SZ_GPR
        GPR_L  $s1, $r5, 4*SZ_GPR
        GPR_L  $s2, $r5, 5*SZ_GPR
        GPR_L  $s3, $r5, 6*SZ_GPR
        GPR_L  $s4, $r5, 7*SZ_GPR
        GPR_L  $s5, $r5, 8*SZ_GPR
        GPR_L  $s6, $r5, 9*SZ_GPR
        GPR_L  $s7, $r5, 10*SZ_GPR
        GPR_L  $s8, $r5, 11*SZ_GPR

#if !defined(__loongarch_soft_float)
        /* Callee-saved scratch FPRs (f24-f31) */
        FPR_L  $f24, $r5, 12*SZ_GPR + 0*SZ_FPR
        FPR_L  $f25, $r5, 12*SZ_GPR + 1*SZ_FPR
        FPR_L  $f26, $r5, 12*SZ_GPR + 2*SZ_FPR
        FPR_L  $f27, $r5, 12*SZ_GPR + 3*SZ_FPR
        FPR_L  $f28, $r5, 12*SZ_GPR + 4*SZ_FPR
        FPR_L  $f29, $r5, 12*SZ_GPR + 5*SZ_FPR
        FPR_L  $f30, $r5, 12*SZ_GPR + 6*SZ_FPR
        FPR_L  $f31, $r5, 12*SZ_GPR + 7*SZ_FPR
#endif

        GPR_L  $r7, $r5, 2*SZ_GPR
        GPR_L  $fp, $r5, 0*SZ_GPR
        GPR_L  $r1, $r5, 1*SZ_GPR
        cfi_def_cfa(5, 0)
        move   $sp, $r7
        jr     $r1
        cfi_endproc
	.size	GTM_longjmp, . - GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
