;   Copyright (C) 2011 Free Software Foundation, Inc.
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


; clobberable
r8	=	0xffef0

	.text

	;;   int __cmpsi2 (signed long A, signed long B)
	;;
	;; Performs a signed comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

	.global	___cmpsi2
        .type   ___cmpsi2, @function
___cmpsi2:
	;; A is at [sp+4]
	;; B is at [sp+8]
	;; Result put in R8

	;; Initialise default return value.
	onew	bc
	
	;;  Compare the high words.
	movw	ax, [sp + 10]
	movw	de, ax
	movw	ax, [sp + 6]
	cmpw	ax, de
	skz
	br	!!.Lconvert_to_signed

.Lcompare_bottom_words:	
	;; The top words are equal - compare the bottom words.
	;; Note - code from __ucmpsi2 branches into here.
	movw   ax, [sp + 8]
	movw   de, ax
	movw   ax, [sp + 4]
	cmpw   ax, de
	sknz
	br	!!.Lless_than_or_greater_than
	;; The words are equal - return 1.
	;; Note - we could branch to the return code at the end of the
	;; function but a branch instruction takes 4 bytes, and the
	;; return sequence itself is only 4 bytes long...
	movw	ax, bc
	movw	r8, ax
	ret

.Lconvert_to_signed:	
	;; The top words are different.  Unfortunately the comparison
	;; is always unsigned, so to get a signed result we XOR the CY
	;; flag with the top bits of AX and DE.
	xor1	cy, a.7
	mov	a, d
	xor1	cy, a.7
	;; Fall through.

.Lless_than_or_greater_than:
	;; We now have a signed less than/greater than result in CY.
	;; Return 0 for less than, 2 for greater than.
	;; Note - code from __ucmpsi2 branches into here.
	incw	bc
	sknc
	clrw	bc

	;; Get the result value, currently in BC, into r8
	movw	ax, bc
	movw	r8, ax
	ret

	.size	___cmpsi2, . - ___cmpsi2
	
	
	;;   int __ucmpsi2 (unsigned long A, unsigned long B)
	;;
	;; Performs an unsigned comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

	.global	___ucmpsi2
        .type   ___ucmpsi2, @function
___ucmpsi2:
	;; A is at [sp+4]
	;; B is at [sp+8]
	;; Result put in R8..R9

	;; Initialise default return value.
	onew	bc

	;;  Compare the high words.
	movw	ax, [sp + 10]
	movw	de, ax
	movw	ax, [sp + 6]
	cmpw	ax, de
	skz
	;; Note: These branches go into the __cmpsi2 code!
	br	!!.Lless_than_or_greater_than
	br	!!.Lcompare_bottom_words

	.size	___ucmpsi2, . - ___ucmpsi2
	