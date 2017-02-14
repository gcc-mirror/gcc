/* SImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "vregs.h"

#if defined __RL78_MUL_G14__

START_FUNC ___divsi3
	;; r8,r10 = 4[sp],6[sp] / 8[sp],10[sp]

	;; Load and test for a negative denumerator.
	movw	ax, [sp+8]
	movw	de, ax
	movw	ax, [sp+10]
	mov1	cy, a.7
	movw	hl, ax
	bc	$__div_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	bc, ax
	movw	ax, [sp+4]
	bc	$__div_neg_num

	;; Neither are negative - we can use the unsigned divide instruction.
__div_no_convert:	
	push	psw
	di
	divwu
	pop	psw
	
	movw	r8, ax
	movw	ax, bc
	movw	r10, ax
	ret

__div_neg_den:
	;; Negate the denumerator (which is in HLDE)
	clrw	ax
	subw	ax, de
	movw	de, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, hl
	movw	hl, ax
	
	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	bc, ax
	movw	ax, [sp+4]
	;; If it is not negative then we perform the division and then negate the result.
	bnc	$__div_then_convert

	;; Otherwise we negate the numerator and then go with a straightforward unsigned division.
	;; The negation is complicated because AX, BC, DE and HL are already in use.
	;;              ax: numL  bc: numH  r8:       r10:
	xchw	ax, bc			    
	;;              ax: numH  bc: numL  r8:       r10:
	movw	r8, ax			    
	;;              ax:       bc: numL  r8: numH  r10:
	clrw	ax			    
	;;              ax:    0  bc: numL  r8: numH  r10:
	subw	ax, bc			    
	;;              ax: -numL bc:       r8: numH  r10:
	movw	r10, ax			    
	;;              ax:       bc:       r8: numH  r10: -numL
	movw	ax, r8			    
	;;              ax: numH  bc:       r8:       r10: -numL
	movw	bc, ax			    
	;;              ax:       bc: numH  r8:       r10: -numL
	clrw	ax			    
	;;              ax:    0  bc: numH  r8:       r10: -numL
	sknc				    
	decw	ax			    
	;;              ax:    -1 bc: numH  r8:       r10: -numL
	subw	ax, bc			    
	;;              ax: -numH bc:       r8:       r10: -numL
	movw	bc, ax			    
	;;              ax:       bc: -numH r8:       r10: -numL
	movw	ax, r10			    
	;;              ax: -numL bc: -numH r8:       r10:
	br	$!__div_no_convert

__div_neg_num:
	;; Negate the numerator (which is in BCAX)
	;; We know that the denumerator is positive.
	;; Note - we temporarily overwrite DE.  We know that we can safely load it again off the stack again.
	movw	de, ax
	clrw	ax
	subw	ax, de
	movw	de, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, bc
	movw	bc, ax

	movw	ax, [sp+8]
	xchw	ax, de
	
__div_then_convert:
	push	psw
	di
	divwu
	pop	psw

	;; Negate result (in BCAX) and transfer into r8,r10
	movw	de, ax
	clrw	ax
	subw	ax, de
	movw	r8, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, bc
	movw	r10, ax
	ret

END_FUNC ___divsi3

;----------------------------------------------------------------------

START_FUNC ___udivsi3
	;; r8,r10 = 4[sp],6[sp] / 8[sp],10[sp]
	;; Used when compiling with -Os specified.

	movw	ax, [sp+10]
	movw	hl, ax
	movw	ax, [sp+8]
	movw	de, ax
	movw	ax, [sp+6]
	movw	bc, ax
	movw    ax, [sp+4]
	push	psw	; Save the current interrupt status
	di		; Disable interrupts. See Renesas Technical update TN-RL*-A025B/E
	divwu   	; bcax = bcax / hlde
	pop	psw	; Restore saved interrupt status
	movw    r8, ax
	movw	ax, bc
	movw    r10, ax
	ret

END_FUNC ___udivsi3

;----------------------------------------------------------------------
	
START_FUNC ___modsi3
	;; r8,r10 = 4[sp],6[sp] % 8[sp],10[sp]

	;; Load and test for a negative denumerator.
	movw	ax, [sp+8]
	movw	de, ax
	movw	ax, [sp+10]
	mov1	cy, a.7
	movw	hl, ax
	bc	$__mod_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	bc, ax
	movw	ax, [sp+4]
	bc	$__mod_neg_num

	;; Neither are negative - we can use the unsigned divide instruction.
__mod_no_convert:	
	push	psw
	di
	divwu
	pop	psw

	movw	ax, de
	movw	r8, ax
	movw	ax, hl
	movw	r10, ax
	ret

__mod_neg_den:
	;; Negate the denumerator (which is in HLDE)
	clrw	ax
	subw	ax, de
	movw	de, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, hl
	movw	hl, ax
	
	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	bc, ax
	movw	ax, [sp+4]
	;; If it is not negative then we perform the modulo operation without conversion
	bnc	$__mod_no_convert

	;; Otherwise we negate the numerator and then go with a modulo followed by negation.
	;; The negation is complicated because AX, BC, DE and HL are already in use.
	xchw	ax, bc			    
	movw	r8, ax			    
	clrw	ax			    
	subw	ax, bc			    
	movw	r10, ax			    
	movw	ax, r8			    
	movw	bc, ax			    
	clrw	ax			    
	sknc				    
	decw	ax			    
	subw	ax, bc			    
	movw	bc, ax			    
	movw	ax, r10			    
	br	$!__mod_then_convert

__mod_neg_num:
	;; Negate the numerator (which is in BCAX)
	;; We know that the denumerator is positive.
	;; Note - we temporarily overwrite DE.  We know that we can safely load it again off the stack again.
	movw	de, ax
	clrw	ax
	subw	ax, de
	movw	de, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, bc
	movw	bc, ax

	movw	ax, [sp+8]
	xchw	ax, de
	
__mod_then_convert:
	push	psw
	di
	divwu
	pop	psw

	;; Negate result (in HLDE) and transfer into r8,r10
	clrw	ax
	subw	ax, de
	movw	r8, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, hl
	movw	r10, ax
	ret

END_FUNC ___modsi3

;----------------------------------------------------------------------

START_FUNC ___umodsi3
	;; r8,r10 = 4[sp],6[sp] % 8[sp],10[sp]
	;; Used when compiling with -Os specified.

	movw	ax, [sp+10]
	movw	hl, ax
	movw	ax, [sp+8]
	movw	de, ax
	movw	ax, [sp+6]
	movw	bc, ax
	movw    ax, [sp+4]
	push	psw	; Save the current interrupt status
	di		; Disable interrupts. See Renesas Technical update TN-RL*-A025B/E
	divwu   	; hlde = bcax %% hlde
	pop	psw	; Restore saved interrupt status
	movw	ax, de
	movw    r8, ax
	movw	ax, hl
	movw    r10, ax
	ret

END_FUNC   ___umodsi3

;----------------------------------------------------------------------

#elif defined __RL78_MUL_G13__

;----------------------------------------------------------------------

	;; Hardware registers.  Note - these values match the silicon, not the documentation.
	MDAL = 0xffff0
	MDAH = 0xffff2
	MDBL = 0xffff6
	MDBH = 0xffff4
	MDCL = 0xf00e0
	MDCH = 0xf00e2
	MDUC = 0xf00e8

.macro _Negate low, high
	movw	ax, \low
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	movw	\low, ax
	movw	ax, \high
	movw	bc, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, bc
	movw	\high, ax
.endm
	
;----------------------------------------------------------------------

START_FUNC ___divsi3
	;; r8,r10 = 4[sp],6[sp] / 8[sp],10[sp]

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	;; Load and test for a negative denumerator.
	movw	ax, [sp+8]
	movw	MDBL, ax
	movw	ax, [sp+10]
	mov1	cy, a.7
	movw	MDBH, ax
	bc	$__div_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	MDAH, ax
	movw	ax, [sp+4]
	movw	MDAL, ax
	bc	$__div_neg_num

	;; Neither are negative - we can use the unsigned divide hardware.
__div_no_convert:	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, MDAL	; Read the result
	movw	r8, ax
	movw	ax, MDAH
	movw	r10, ax	
	ret

__div_neg_den:
	;; Negate the denumerator (which is in MDBL/MDBH)
	_Negate MDBL MDBH

	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	MDAH, ax
	movw	ax, [sp+4]
	movw	MDAL, ax
	;; If it is not negative then we perform the division and then negate the result.
	bnc	$__div_then_convert

	;; Otherwise we negate the numerator and then go with a straightforward unsigned division.
	_Negate MDAL MDAH
	br	$!__div_no_convert

__div_neg_num:
	;; Negate the numerator (which is in MDAL/MDAH)
	;; We know that the denumerator is positive.
	_Negate MDAL MDAH
	
__div_then_convert:
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

	;; Negate result and transfer into r8,r10
	_Negate MDAL MDAH    	; FIXME: This could be coded more efficiently.
	movw	r10, ax
	movw	ax, MDAL
	movw	r8, ax

	ret

END_FUNC ___divsi3

;----------------------------------------------------------------------

START_FUNC ___modsi3
	;; r8,r10 = 4[sp],6[sp] % 8[sp],10[sp]

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	;; Load and test for a negative denumerator.
	movw	ax, [sp+8]
	movw	MDBL, ax
	movw	ax, [sp+10]
	mov1	cy, a.7
	movw	MDBH, ax
	bc	$__mod_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	MDAH, ax
	movw	ax, [sp+4]
	movw	MDAL, ax
	bc	$__mod_neg_num

	;; Neither are negative - we can use the unsigned divide hardware
__mod_no_convert:	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, !MDCL	; Read the remainder
	movw	r8, ax
	movw	ax, !MDCH
	movw	r10, ax	
	ret

__mod_neg_den:
	;; Negate the denumerator (which is in MDBL/MDBH)
	_Negate MDBL MDBH
	
	;; Load and test for a negative numerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	MDAH, ax
	movw	ax, [sp+4]
	movw	MDAL, ax
	;; If it is not negative then we perform the modulo operation without conversion
	bnc	$__mod_no_convert

	;; Otherwise we negate the numerator and then go with a modulo followed by negation.
	_Negate MDAL MDAH
	br	$!__mod_then_convert

__mod_neg_num:
	;; Negate the numerator (which is in MDAL/MDAH)
	;; We know that the denumerator is positive.
	_Negate MDAL MDAH
	
__mod_then_convert:
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

	movw	ax, !MDCL
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	movw	r8, ax
	movw	ax, !MDCH
	movw	bc, ax
	clrw	ax
	sknc
	decw	ax
	subw	ax, bc
	movw	r10, ax
	ret

END_FUNC ___modsi3

;----------------------------------------------------------------------

START_FUNC ___udivsi3
	;; r8,r10 = 4[sp],6[sp] / 8[sp],10[sp]
	;; Used when compilng with -Os specified.

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	movw	ax, [sp+4]	; Load the divisor
	movw	MDAL, ax
	movw	ax, [sp+6]
	movw	MDAH, ax
	movw	ax, [sp+8]	; Load the dividend
	movw	MDBL, ax
	movw    ax, [sp+10]
	movw	MDBH, ax
	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, !MDAL	; Read the result
	movw	r8, ax
	movw	ax, !MDAH
	movw	r10, ax	
	ret
	
END_FUNC   ___udivsi3

;----------------------------------------------------------------------

START_FUNC ___umodsi3
	;; r8,r10 = 4[sp],6[sp] % 8[sp],10[sp]
	;; Used when compilng with -Os specified.
	;; Note - hardware address match the silicon, not the documentation

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	movw	ax, [sp+4]	; Load the divisor
	movw	MDAL, ax
	movw	ax, [sp+6]
	movw	MDAH, ax
	movw	ax, [sp+8]	; Load the dividend
	movw	MDBL, ax
	movw    ax, [sp+10]
	movw	MDBH, ax
	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, !MDCL	; Read the remainder
	movw	r8, ax
	movw	ax, !MDCH
	movw	r10, ax	
	ret
	
END_FUNC   ___umodsi3

;----------------------------------------------------------------------

#elif defined __RL78_MUL_NONE__

.macro MAKE_GENERIC  which,need_result

	.if \need_result
	quot = r8
	num = r12
	den = r16
	bit = r20
	.else
	num = r8
	quot = r12
	den = r16
	bit = r20
	.endif

	quotH = quot+2
	quotL = quot
	quotB0 = quot
	quotB1 = quot+1
	quotB2 = quot+2
	quotB3 = quot+3
	
	numH = num+2
	numL = num
	numB0 = num
	numB1 = num+1
	numB2 = num+2
	numB3 = num+3
	
#define	denH bc
	denL = den
	denB0 = den
	denB1 = den+1
#define	denB2 c
#define	denB3 b
	
	bitH = bit+2
	bitL = bit
	bitB0 = bit
	bitB1 = bit+1
	bitB2 = bit+2
	bitB3 = bit+3

;----------------------------------------------------------------------

START_FUNC __generic_sidivmod\which

num_lt_den\which:
	.if \need_result
	movw	r8, #0
	movw	r10, #0
	.else
	movw	ax, [sp+8]
	movw	r8, ax
	movw	ax, [sp+10]
	movw	r10, ax
	.endif
	ret

shift_den_bit16\which:
	movw	ax, denL
	movw	denH, ax
	movw	denL, #0
	.if \need_result
	movw	ax, bitL
	movw	bitH, ax
	movw	bitL, #0
	.else
	mov	a, bit
	add	a, #16
	mov	bit, a
	.endif
	br	$shift_den_bit\which

	;; These routines leave DE alone - the signed functions use DE
	;; to store sign information that must remain intact

	.if \need_result
	.global __generic_sidiv
__generic_sidiv:

	.else

	.global __generic_simod
__generic_simod:

	.endif

	;; (quot,rem) = 8[sp] /% 12[sp]

	movw	hl, sp
	movw	ax, [hl+14] ; denH
	cmpw	ax, [hl+10] ; numH
	movw	ax, [hl+12] ; denL
	sknz
	cmpw	ax, [hl+8] ; numL
	bh	$num_lt_den\which

#ifdef __RL78_G10__
	movw	ax, denL
	push	ax
	movw	ax, bitL
	push	ax
	movw	ax, bitH
	push	ax
#else
	sel	rb2
	push	ax		; denL
;	push	bc		; denH
	push	de		; bitL
	push	hl		; bitH - stored in BC
	sel	rb0
#endif

	;; (quot,rem) = 16[sp] /% 20[sp]

	;; copy numerator
	movw	ax, [hl+8]
	movw	numL, ax
	movw	ax, [hl+10]
	movw	numH, ax

	;; copy denomonator
	movw	ax, [hl+12]
	movw	denL, ax
	movw	ax, [hl+14]
	movw	denH, ax

	movw	ax, denL
	or	a, denB2
	or	a, denB3	; not x
	cmpw	ax, #0
	bnz	$den_not_zero\which
	.if \need_result
	movw	quotL, #0
	movw	quotH, #0
	.else
	movw	numL, #0
	movw	numH, #0
	.endif
	br	$!main_loop_done_himode\which

den_not_zero\which:
	.if \need_result
	;; zero out quot
	movw	quotL, #0
	movw	quotH, #0
	.endif

	;; initialize bit to 1
	movw	bitL, #1
	movw	bitH, #0

; while (den < num && !(den & (1L << BITS_MINUS_1)))

	.if 1
	;; see if we can short-circuit a bunch of shifts
	movw	ax, denH
	cmpw	ax, #0
	bnz	$shift_den_bit\which
	movw	ax, denL
	cmpw	ax, numH
	bnh	$shift_den_bit16\which
	.endif

shift_den_bit\which:	
	movw	ax, denH
	mov1	cy,a.7
	bc	$enter_main_loop\which
	cmpw	ax, numH
	movw	ax, denL	; we re-use this below
	sknz
	cmpw	ax, numL
	bh	$enter_main_loop\which

	;; den <<= 1
;	movw	ax, denL	; already has it from the cmpw above
	shlw	ax, 1
	movw	denL, ax
;	movw	ax, denH
	rolwc	denH, 1
;	movw	denH, ax

	;; bit <<= 1
	.if \need_result
	movw	ax, bitL
	shlw	ax, 1
	movw	bitL, ax
	movw	ax, bitH
	rolwc	ax, 1
	movw	bitH, ax
	.else
	;; if we don't need to compute the quotent, we don't need an
	;; actual bit *mask*, we just need to keep track of which bit
	inc	bitB0
	.endif

	br	$shift_den_bit\which

	;; while (bit)
main_loop\which:

	;; if (num >= den) (cmp den > num)
	movw	ax, numH
	cmpw	ax, denH
	movw	ax, numL
	sknz
	cmpw	ax, denL
	skz
	bnh	$next_loop\which

	;; num -= den
;	movw	ax, numL	; already has it from the cmpw above
	subw	ax, denL
	movw	numL, ax
	movw	ax, numH
	sknc
	decw	ax
	subw	ax, denH
	movw	numH, ax

	.if \need_result
	;; res |= bit
	mov	a, quotB0
	or	a, bitB0
	mov	quotB0, a
	mov	a, quotB1
	or	a, bitB1
	mov	quotB1, a
	mov	a, quotB2
	or	a, bitB2
	mov	quotB2, a
	mov	a, quotB3
	or	a, bitB3
	mov	quotB3, a
	.endif

next_loop\which:	

	;; den >>= 1
	movw	ax, denH
	shrw	ax, 1
	movw	denH, ax
	mov	a, denB1
	rorc	a, 1
	mov	denB1, a
	mov	a, denB0
	rorc	a, 1
	mov	denB0, a

	;; bit >>= 1
	.if \need_result
	movw	ax, bitH
	shrw	ax, 1
	movw	bitH, ax
	mov	a, bitB1
	rorc	a, 1
	mov	bitB1, a
	mov	a, bitB0
	rorc	a, 1
	mov	bitB0, a
	.else
	dec	bitB0
	.endif

enter_main_loop\which:
	.if \need_result
	movw	ax, bitH
	cmpw	ax, #0
	bnz	$main_loop\which
	.else
	cmp	bitB0, #15
	bh	$main_loop\which
	.endif
	;; bit is HImode now; check others
	movw	ax, numH	; numerator
	cmpw	ax, #0
	bnz	$bit_high_set\which
	movw	ax, denH	; denominator
	cmpw	ax, #0
	bz	$switch_to_himode\which
bit_high_set\which:	
	.if \need_result
	movw	ax, bitL
	cmpw	ax, #0
	.else
	cmp0	bitB0
	.endif
	bnz	$main_loop\which

switch_to_himode\which:
	.if \need_result
	movw	ax, bitL
	cmpw	ax, #0
	.else
	cmp0	bitB0
	.endif
	bz	$main_loop_done_himode\which

	;; From here on in, r22, r14, and r18 are all zero
	;; while (bit)
main_loop_himode\which:

	;; if (num >= den) (cmp den > num)
	movw	ax, denL
	cmpw	ax, numL
	bh	$next_loop_himode\which

	;; num -= den
	movw	ax, numL
	subw	ax, denL
	movw	numL, ax
	movw	ax, numH
	sknc
	decw	ax
	subw	ax, denH
	movw	numH, ax

	.if \need_result
	;; res |= bit
	mov	a, quotB0
	or	a, bitB0
	mov	quotB0, a
	mov	a, quotB1
	or	a, bitB1
	mov	quotB1, a
	.endif

next_loop_himode\which:	

	;; den >>= 1
	movw	ax, denL
	shrw	ax, 1
	movw	denL, ax

	.if \need_result
	;; bit >>= 1
	movw	ax, bitL
	shrw	ax, 1
	movw	bitL, ax
	.else
	dec	bitB0
	.endif

	.if \need_result
	movw	ax, bitL
	cmpw	ax, #0
	.else
	cmp0	bitB0
	.endif
	bnz	$main_loop_himode\which

main_loop_done_himode\which:	
#ifdef __RL78_G10__
	pop	ax
	movw	bitH, ax
	pop	ax
	movw	bitL, ax
	pop	ax
	movw	denL, ax
#else
	sel	rb2
	pop	hl		; bitH - stored in BC
	pop	de		; bitL
;	pop	bc		; denH
	pop	ax		; denL
	sel	rb0
#endif

	ret
END_FUNC __generic_sidivmod\which
.endm

;----------------------------------------------------------------------

	MAKE_GENERIC _d 1
	MAKE_GENERIC _m 0

;----------------------------------------------------------------------

START_FUNC ___udivsi3
	;; r8 = 4[sp] / 8[sp]
	call	$!__generic_sidiv
	ret
END_FUNC ___udivsi3
	

START_FUNC ___umodsi3
	;; r8 = 4[sp] % 8[sp]
	call	$!__generic_simod
	ret
END_FUNC ___umodsi3

;----------------------------------------------------------------------

.macro NEG_AX
	movw	hl, ax
	movw	ax, #0
	subw	ax, [hl]
	movw	[hl], ax
	movw	ax, #0
	sknc
	decw	ax
	subw	ax, [hl+2]
	movw	[hl+2], ax
.endm

;----------------------------------------------------------------------

START_FUNC ___divsi3
	;; r8 = 4[sp] / 8[sp]
	movw	de, #0
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+11]
	mov1	cy, a.7
	bc	$div_signed_den
	call	$!__generic_sidiv
	ret

div_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	NEG_AX
	mov	d, #1
	mov	a, [sp+11]
	mov1	cy, a.7
	bnc	$div_unsigned_den
div_signed_den:	
	;; neg [sp+8]
	movw	ax, sp
	addw	ax, #8
	NEG_AX
	mov	e, #1
div_unsigned_den:	
	call	$!__generic_sidiv

	mov	a, d
	cmp0	a
	bz	$div_skip_restore_num
	;;  We have to restore the numerator [sp+4]
	movw	ax, sp
	addw	ax, #4
	NEG_AX
	mov	a, d
div_skip_restore_num:	
	xor	a, e
	bz	$div_no_neg
	movw	ax, #r8
	NEG_AX
div_no_neg:
	mov	a, e
	cmp0	a
	bz	$div_skip_restore_den
	;;  We have to restore the denominator [sp+8]
	movw	ax, sp
	addw	ax, #8
	NEG_AX
div_skip_restore_den:
	ret
END_FUNC ___divsi3
	

START_FUNC ___modsi3
	;; r8 = 4[sp] % 8[sp]
	movw	de, #0
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [sp+11]
	mov1	cy, a.7
	bc	$mod_signed_den
	call	$!__generic_simod
	ret

mod_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	NEG_AX
	mov	d, #1
	mov	a, [sp+11]
	mov1	cy, a.7
	bnc	$mod_unsigned_den
mod_signed_den:	
	;; neg [sp+8]
	movw	ax, sp
	addw	ax, #8
	NEG_AX
	mov	e, #1
mod_unsigned_den:	
	call	$!__generic_simod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	movw	ax, #r8
	NEG_AX
	;;  We have to restore [sp+4] as well.
	movw	ax, sp
	addw	ax, #4
	NEG_AX
mod_no_neg:
 .if 1
	mov	a, e
	cmp0	a
	bz	$mod_skip_restore_den
	movw	ax, sp
	addw	ax, #8
	NEG_AX
mod_skip_restore_den:	
 .endif	
	ret
END_FUNC ___modsi3

;----------------------------------------------------------------------

#else

#error "Unknown RL78 hardware multiply/divide support"

#endif
