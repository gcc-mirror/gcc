/* SImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#ifndef __RL78_G10__

#include "vregs.h"

	.macro make_generic  which,need_result

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

generic_div:

	.else

generic_mod:

	.endif

	;; (quot,rem) = 8[sp] /% 12[sp]

	movw	hl, sp
	movw	ax, [hl+14] ; denH
	cmpw	ax, [hl+10] ; numH
	movw	ax, [hl+12] ; denL
	sknz
	cmpw	ax, [hl+8] ; numL
	bh	$num_lt_den\which

	sel	rb2
	push	ax		; denL
;	push	bc		; denH
	push	de		; bitL
	push	hl		; bitH - stored in BC
	sel	rb0

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
	movw	numL, #0
	movw	numH, #0
	ret

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
	sel	rb2
	pop	hl		; bitH - stored in BC
	pop	de		; bitL
;	pop	bc		; denH
	pop	ax		; denL
	sel	rb0

	ret
	.endm

	make_generic _d 1
	make_generic _m 0

;----------------------------------------------------------------------

	.global	___udivsi3
	.type	___udivsi3,@function
___udivsi3:
	;; r8 = 4[sp] / 8[sp]
	call	$!generic_div
	ret
	.size	___udivsi3, . - ___udivsi3
	

	.global	___umodsi3
	.type	___umodsi3,@function
___umodsi3:
	;; r8 = 4[sp] % 8[sp]
	call	$!generic_mod
	ret
	.size	___umodsi3, . - ___umodsi3

;----------------------------------------------------------------------

	.macro neg_ax
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

	.global	___divsi3
	.type	___divsi3,@function
___divsi3:
	;; r8 = 4[sp] / 8[sp]
	movw	de, #0
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+11]
	mov1	cy, a.7
	bc	$div_signed_den
	call	$!generic_div
	ret
	
div_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	neg_ax
	mov	d, #1
	mov	a, [sp+11]
	mov1	cy, a.7
	bnc	$div_unsigned_den
div_signed_den:	
	;; neg [sp+8]
	movw	ax, sp
	addw	ax, #8
	neg_ax
	mov	e, #1
div_unsigned_den:	
	call	$!generic_div

	mov	a, d
	cmp0	a
	bz	$div_skip_restore_num
	;;  We have to restore the numerator [sp+4]
	movw	ax, sp
	addw	ax, #4
	neg_ax
	mov	a, d
div_skip_restore_num:	
	xor	a, e
	bz	$div_no_neg
	movw	ax, #r8
	neg_ax
div_no_neg:
	mov	a, e
	cmp0	a
	bz	$div_skip_restore_den
	;;  We have to restore the denominator [sp+8]
	movw	ax, sp
	addw	ax, #8
	neg_ax
div_skip_restore_den:
	ret
	.size	___divsi3, . - ___divsi3
	

	.global	___modsi3
	.type	___modsi3,@function
___modsi3:
	;; r8 = 4[sp] % 8[sp]
	movw	de, #0
	mov	a, [sp+7]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [sp+11]
	mov1	cy, a.7
	bc	$mod_signed_den
	call	$!generic_mod
	ret
	
mod_signed_num:
	;; neg [sp+4]
	movw	ax, sp
	addw	ax, #4
	neg_ax
	mov	d, #1
	mov	a, [sp+11]
	mov1	cy, a.7
	bnc	$mod_unsigned_den
mod_signed_den:	
	;; neg [sp+8]
	movw	ax, sp
	addw	ax, #8
	neg_ax
	mov	e, #1
mod_unsigned_den:	
	call	$!generic_mod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	movw	ax, #r8
	neg_ax
	;;  We have to restore [sp+4] as well.
	movw	ax, sp
	addw	ax, #4
	neg_ax
mod_no_neg:
 .if 1
	mov	a, e
	cmp0	a
	bz	$mod_skip_restore_den
	movw	ax, sp
	addw	ax, #8
	neg_ax
mod_skip_restore_den:	
 .endif	
	ret
	.size	___modsi3, . - ___modsi3

#endif
