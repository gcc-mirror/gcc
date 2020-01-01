/* HImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "vregs.h"

#if defined __RL78_MUL_G14__

START_FUNC ___divhi3
	;; r8 = 4[sp] / 6[sp]

	;; Test for a negative denumerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	de, ax
	bc	$__div_neg_den

	;; Test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	bc	$__div_neg_num

	;; Neither are negative - we can use the unsigned divide instruction.
__div_no_convert:	
	push	psw
	di
	divhu
	pop	psw
	
	movw	r8, ax
	ret

__div_neg_den:
	;; Negate the denumerator (which is in DE)
	clrw	ax
	subw	ax, de
	movw	de, ax
	
	;; Test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	;; If it is not negative then we perform the division and then negate the result.
	bnc	$__div_then_convert

	;; Otherwise we negate the numerator and then go with an unsigned division.
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	br	$__div_no_convert

__div_neg_num:
	;; Negate the numerator (which is in AX)
	;; We know that the denumerator is positive.
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	
__div_then_convert:
	push	psw
	di
	divhu
	pop	psw
	
	;; Negate result and transfer into r8
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	movw	r8, ax
	ret

END_FUNC ___divhi3

;----------------------------------------------------------------------

START_FUNC ___modhi3
	;; r8 = 4[sp] % 6[sp]

	;; Test for a negative denumerator.
	movw	ax, [sp+6]
	mov1	cy, a.7
	movw	de, ax
	bc	$__mod_neg_den

	;; Test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	bc	$__mod_neg_num

	;; Neither are negative - we can use the unsigned divide instruction.
__mod_no_convert:	
	push	psw
	di
	divhu
	pop	psw

	movw	ax, de
	movw	r8, ax
	ret

__mod_neg_den:	
	;; Negate the denumerator (which is in DE)
	clrw	ax
	subw	ax, de
	movw	de, ax
	
	;; Test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	;; If it is not negative then we perform the modulo operation without conversion.
	bnc	$__mod_no_convert

	;; Otherwise we negate the numerator and then go with an unsigned modulo operation.
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	br	$__mod_then_convert

__mod_neg_num:
	;; Negate the numerator (which is in AX)
	;; We know that the denumerator is positive.
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	
__mod_then_convert:
	push	psw
	di
	divhu
	pop	psw

	;; Negate result and transfer into r8
	clrw	  ax
	subw	  ax, de
	movw	  r8, ax
	ret

END_FUNC ___modhi3

;----------------------------------------------------------------------

#elif defined __RL78_MUL_G13__

	;; The G13 S2 core does not have a 16 bit divide peripheral.
	;; So instead we perform a 32-bit divide and twiddle the inputs
	;; as necessary.

	;; Hardware registers.  Note - these values match the silicon, not the documentation.
	MDAL = 0xffff0
	MDAH = 0xffff2
	MDBL = 0xffff6
	MDBH = 0xffff4
	MDCL = 0xf00e0
	MDCH = 0xf00e2
	MDUC = 0xf00e8

.macro _Negate src, dest
	movw	ax, !\src
	movw	bc, ax
	clrw	ax
	subw	ax, bc
	movw	\dest, ax
.endm
	
;----------------------------------------------------------------------
	
START_FUNC ___divhi3
	;; r8 = 4[sp] / 6[sp] (signed division)

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	clrw	ax     		; Clear the top 16-bits of the divisor and dividend
	movw	MDBH, ax
	movw	MDAH, ax
	
	;; Load and test for a negative denumerator.
	movw	ax, [sp+6]
	movw	MDBL, ax
	mov1	cy, a.7
	bc	$__div_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
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
	ret

__div_neg_den:
	;; Negate the denumerator (which is in MDBL)
	_Negate MDBL MDBL

	;; Load and test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	movw	MDAL, ax
	;; If it is not negative then we perform the division and then negate the result.
	bnc	$__div_then_convert

	;; Otherwise we negate the numerator and then go with a straightforward unsigned division.
	_Negate MDAL MDAL
	br	$!__div_no_convert

__div_neg_num:
	;; Negate the numerator (which is in MDAL)
	;; We know that the denumerator is positive.
	_Negate MDAL MDAL
	
__div_then_convert:
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

	;; Negate result and transfer into r8
	_Negate MDAL r8
	ret

END_FUNC ___divhi3

;----------------------------------------------------------------------

START_FUNC ___modhi3
	;; r8 = 4[sp] % 6[sp] (signed modulus)

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	clrw	ax     		; Clear the top 16-bits of the divisor and dividend
	movw	MDBH, ax
	movw	MDAH, ax
	
	;; Load and test for a negative denumerator.
	movw	ax, [sp+6]
	movw	MDBL, ax
	mov1	cy, a.7
	bc	$__mod_neg_den

	;; Load and test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
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
	ret

__mod_neg_den:
	;; Negate the denumerator (which is in MDBL)
	_Negate MDBL MDBL
	
	;; Load and test for a negative numerator.
	movw	ax, [sp+4]
	mov1	cy, a.7
	movw	MDAL, ax
	;; If it is not negative then we perform the modulo operation without conversion.
	bnc	$__mod_no_convert

	;; Otherwise we negate the numerator and then go with a modulo followed by negation.
	_Negate MDAL MDAL
	br	$!__mod_then_convert

__mod_neg_num:
	;; Negate the numerator (which is in MDAL)
	;; We know that the denumerator is positive.
	_Negate MDAL MDAL
	
__mod_then_convert:
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

	_Negate	MDCL r8
	ret

END_FUNC ___modhi3

;----------------------------------------------------------------------

START_FUNC ___udivhi3
	;; r8 = 4[sp] / 6[sp] (unsigned division)

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	movw	ax, [sp+4]	; Load the divisor
	movw	MDAL, ax
	movw	ax, [sp+6]	; Load the dividend
	movw	MDBL, ax
	clrw	ax
	movw	MDAH, ax
	movw	MDBH, ax
	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, !MDAL	; Read the remainder
	movw	r8, ax
	ret

END_FUNC   ___udivhi3

;----------------------------------------------------------------------

START_FUNC ___umodhi3
	;; r8 = 4[sp] % 6[sp] (unsigned modulus)

	mov	a, #0xC0	; Set DIVMODE=1 and MACMODE=1
	mov	!MDUC, a	; This preps the peripheral for division without interrupt generation

	movw	ax, [sp+4]	; Load the divisor
	movw	MDAL, ax
	movw	ax, [sp+6]	; Load the dividend
	movw	MDBL, ax
	clrw	ax
	movw	MDAH, ax
	movw	MDBH, ax
	
	mov	a, #0xC1	; Set the DIVST bit in MDUC
	mov	!MDUC, a	; This starts the division op

1:	mov	a, !MDUC	; Wait 16 clocks or until DIVST is clear
	bt	a.0, $1b

  	movw	ax, !MDCL	; Read the remainder
	movw	r8, ax
	ret
	
END_FUNC   ___umodhi3

;----------------------------------------------------------------------
	
#elif defined __RL78_MUL_NONE__
	
.macro MAKE_GENERIC  which,need_result

	.if \need_result
	quot = r8
	num = r10
	den = r12
	bit = r14
	.else
	num = r8
	quot = r10
	den = r12
	bit = r14
	.endif

	quotB0 = quot
	quotB1 = quot+1
	
	numB0 = num
	numB1 = num+1
	
	denB0 = den
	denB1 = den+1
	
	bitB0 = bit
	bitB1 = bit+1

#define bit	bc
#define bitB0	c
#define bitB1	b

	START_FUNC __generic_hidivmod\which

num_lt_den\which:
	.if \need_result
	movw	r8, #0
	.else
	movw	ax, [sp+8]
	movw	r8, ax
	.endif
	ret

	;; These routines leave DE alone - the signed functions use DE
	;; to store sign information that must remain intact

	.if \need_result
	.global __generic_hidiv
__generic_hidiv:

	.else

	.global __generic_himod
__generic_himod:

	.endif

	;; (quot,rem) = 8[sp] /% 10[sp]

	movw	hl, sp
	movw	ax, [hl+10] ; denH
	cmpw	ax, [hl+8] ; numH
	bh	$num_lt_den\which

	;; (quot,rem) = 16[sp] /% 20[sp]

	;; copy numerator
	movw	ax, [hl+8]
	movw	num, ax

	;; copy denomonator
	movw	ax, [hl+10]
	movw	den, ax

	movw	ax, den
	cmpw	ax, #0
	bnz	$den_not_zero\which
	.if \need_result
	movw    quot, #0
	.else
	movw	num, #0
	.endif
	ret

den_not_zero\which:
	.if \need_result
	;; zero out quot
	movw	quot, #0
	.endif

	;; initialize bit to 1
	movw	bit, #1

; while (den < num && !(den & (1L << BITS_MINUS_1)))

shift_den_bit\which:	
	movw	ax, den
	mov1	cy,a.7
	bc	$enter_main_loop\which
	cmpw	ax, num
	bh	$enter_main_loop\which

	;; den <<= 1
;	movw	ax, den		; already has it from the cmpw above
	shlw	ax, 1
	movw	den, ax

	;; bit <<= 1
	.if \need_result
#ifdef bit
	shlw	bit, 1
#else
	movw	ax, bit
	shlw	ax, 1
	movw	bit, ax
#endif
	.else
	;; if we don't need to compute the quotent, we don't need an
	;; actual bit *mask*, we just need to keep track of which bit
	inc	bitB0
	.endif

	br	$shift_den_bit\which

main_loop\which:

	;; if (num >= den) (cmp den > num)
	movw	ax, den
	cmpw	ax, num
	bh	$next_loop\which

	;; num -= den
	movw	ax, num
	subw	ax, den
	movw	num, ax

	.if \need_result
	;; res |= bit
	mov	a, quotB0
	or	a, bitB0
	mov	quotB0, a
	mov	a, quotB1
	or	a, bitB1
	mov	quotB1, a
	.endif

next_loop\which:	

	;; den >>= 1
	movw	ax, den
	shrw	ax, 1
	movw	den, ax

	.if \need_result
	;; bit >>= 1
	movw	ax, bit
	shrw	ax, 1
	movw	bit, ax
	.else
	dec	bitB0
	.endif

enter_main_loop\which:
	.if \need_result
	movw	ax, bit
	cmpw	ax, #0
	.else
	cmp0	bitB0
	.endif
	bnz	$main_loop\which

main_loop_done\which:	
	ret
	END_FUNC __generic_hidivmod\which
.endm
;----------------------------------------------------------------------

	MAKE_GENERIC _d 1
	MAKE_GENERIC _m 0

;----------------------------------------------------------------------

START_FUNC ___udivhi3
	;; r8 = 4[sp] / 6[sp]
	call	$!__generic_hidiv
	ret
END_FUNC ___udivhi3
	

START_FUNC ___umodhi3
	;; r8 = 4[sp] % 6[sp]
	call	$!__generic_himod
	ret
END_FUNC ___umodhi3

;----------------------------------------------------------------------

.macro NEG_AX
	movw	hl, ax
	movw	ax, #0
	subw	ax, [hl]
	movw	[hl], ax
.endm

;----------------------------------------------------------------------

START_FUNC ___divhi3
	;; r8 = 4[sp] / 6[sp]
	movw	de, #0
	mov	a, [sp+5]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$div_signed_den
	call	$!__generic_hidiv
	ret
	
div_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	NEG_AX
	mov	d, #1
	mov	a, [sp+7]
	mov1	cy, a.7
	bnc	$div_unsigned_den
div_signed_den:	
	;; neg [sp+6]
	movw	ax, sp
	addw	ax, #6
	NEG_AX
	mov	e, #1
div_unsigned_den:	
	call	$!__generic_hidiv

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
	movw	ax, sp
	addw	ax, #6
	NEG_AX
div_skip_restore_den:	
	ret
END_FUNC ___divhi3
	

START_FUNC ___modhi3
	;; r8 = 4[sp] % 6[sp]
	movw	de, #0
	mov	a, [sp+5]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$mod_signed_den
	call	$!__generic_himod
	ret
	
mod_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	NEG_AX
	mov	d, #1
	mov	a, [sp+7]
	mov1	cy, a.7
	bnc	$mod_unsigned_den
mod_signed_den:	
	;; neg [sp+6]
	movw	ax, sp
	addw	ax, #6
	NEG_AX
mod_unsigned_den:	
	call	$!__generic_himod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	movw	ax, #r8
	NEG_AX
	;;  Also restore numerator
	movw 	ax, sp
	addw	ax, #4
	NEG_AX
mod_no_neg:
	mov	a, e
	cmp0	a
	bz	$mod_skip_restore_den
	movw	ax, sp
	addw	ax, #6
	NEG_AX
mod_skip_restore_den:	
	ret
END_FUNC ___modhi3

;----------------------------------------------------------------------

#else

#error "Unknown RL78 hardware multiply/divide support"

#endif
