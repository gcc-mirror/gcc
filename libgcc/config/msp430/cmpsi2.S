;   Copyright (C) 2012-2019 Free Software Foundation, Inc.
;   Contributed by Red Hat.
; 
; This file is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by the
; Free Software Foundation; either version 3, or (at your option) any
; later version.
; 
; This file is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
; 
; Under Section 7 of GPL version 3, you are granted additional
; permissions described in the GCC Runtime Library Exception, version
; 3.1, as published by the Free Software Foundation.
;
; You should have received a copy of the GNU General Public License and
; a copy of the GCC Runtime Library Exception along with this program;
; see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
; <http://www.gnu.org/licenses/>.

#ifdef __MSP430X_LARGE__
#define ret_	RETA
#else
#define ret_	RET
#endif

	.text

	;;   int __cmpsi2 (signed long A, signed long B)
	;;
	;; Performs a signed comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

	;;  Note - this code is also used by the __ucmpsi2 routine below.

	.global	__cmpsi2
        .type   __cmpsi2, @function
__cmpsi2:
	;; A is in r12 (low), r13 (high)
	;; B is in r14 (low), r15 (high)
	;; Result put in r12

	cmp.w	r13, r15
	jeq	.L_compare_low
	jge	.L_less_than
.L_greater_than:
	mov.w	#2, r12
	ret_
.L_less_than:
	mov.w	#0, r12
	ret_

.L_compare_low:
	cmp.w	r12, r14
	jl	.L_greater_than
	jne     .L_less_than
	mov.w	#1, r12
	ret_

	.size	__cmpsi2, . - __cmpsi2


	;;   int __ucmpsi2 (unsigned long A, unsigned long B)
	;;
	;; Performs an unsigned comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

;;;  Note - this function branches into the __cmpsi2 code above.

	.global	__ucmpsi2
        .type   __ucmpsi2, @function
__ucmpsi2:
	;; A is in r12 (low), r13 (high)
	;; B is in r14 (low), r15 (high)
	;; Result put in r12

	tst	r13
	jn	.L_top_bit_set_in_A
	tst	r15
;;; If the top bit of B is set, but A's is clear we know that A < B.
	jn	.L_less_than
;;; Neither A nor B has their top bit set so we can use the __cmpsi2 routine.
;;; Note we use Jc rather than BR as that saves two bytes.  The TST insn always
;;; sets the C bit.
	jc	__cmpsi2

.L_top_bit_set_in_A:
	tst	r15
;;;  If both A and B have their top bit set we can use the __cmpsi2 routine.
	jn	__cmpsi2
;;; Otherwise A has its top bit set and B does not so A > B.
	jc	.L_greater_than

	.size	__ucmpsi2, . - __ucmpsi2
