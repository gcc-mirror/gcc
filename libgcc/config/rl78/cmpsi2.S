;   Copyright (C) 2011-2019 Free Software Foundation, Inc.
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


#include "vregs.h"

	.text

	;;   int __cmpsi2 (signed long A, signed long B)
	;;
	;; Performs a signed comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

START_FUNC ___cmpsi2

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

END_FUNC ___cmpsi2

;; ------------------------------------------------------

	;;   int __ucmpsi2 (unsigned long A, unsigned long B)
	;;
	;; Performs an unsigned comparison of A and B.
	;; If A is less than B it returns 0.  If A is greater
	;; than B it returns 2.  If they are equal it returns 1.

START_FUNC ___ucmpsi2

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

END_FUNC ___ucmpsi2

;; ------------------------------------------------------
	
	;;   signed int __gcc_bcmp (const unsigned char *s1, const unsigned char *s2, size_t size)
	;;   Result is negative if S1 is less than S2,
	;;   positive if S1 is greater, 0 if S1 and S2 are equal.

START_FUNC __gcc_bcmp

	;; S1 is at [sp+4]
	;; S2 is at [sp+6]
	;; SIZE is at [sp+8]
	;; Result in r8/r9
	
        movw	r10, #0
1:
	;; Compare R10 against the SIZE parameter
        movw	ax, [sp+8]
        subw	ax, r10
        sknz
        br	!!1f

	;; Load S2[r10] into R8
        movw	ax, [sp+6]
        addw	ax, r10
        movw	hl, ax
        mov	a, [hl]
        mov	r8, a

	;; Load S1[r10] into A
        movw	ax, [sp+4]
        addw	ax, r10
        movw	hl, ax
        mov	a, [hl]

	;; Increment offset
        incw	r10

	;; Compare loaded bytes
        cmp	a, r8
        sknz
        br	!!1b

	;; They differ.  Subtract *S2 from *S1 and return as the result.
	mov	x, a
	mov	a, #0
	mov	r9, #0
	subw	ax, r8
1:
	movw	r8, ax
        ret

END_FUNC __gcc_bcmp
