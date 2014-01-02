/* HImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#ifndef __RL78_G10__

#include "vregs.h"

	.macro make_generic  which,need_result

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

#if 1
#define bit	bc
#define bitB0	c
#define bitB1	b
#endif

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

generic_div:

	.else

generic_mod:

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
	movw	num, #0
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
	.endm

	make_generic _d 1
	make_generic _m 0

;----------------------------------------------------------------------

	.global	___udivhi3
	.type	___udivhi3,@function
___udivhi3:
	;; r8 = 4[sp] / 6[sp]
	call	$!generic_div
	ret
	.size	___udivhi3, . - ___udivhi3
	

	.global	___umodhi3
	.type	___umodhi3,@function
___umodhi3:
	;; r8 = 4[sp] % 6[sp]
	call	$!generic_mod
	ret
	.size	___umodhi3, . - ___umodhi3

;----------------------------------------------------------------------

	.macro neg_ax
	movw	hl, ax
	movw	ax, #0
	subw	ax, [hl]
	movw	[hl], ax
	.endm

	.global	___divhi3
	.type	___divhi3,@function
___divhi3:
	;; r8 = 4[sp] / 6[sp]
	movw	de, #0
	mov	a, [sp+5]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+7]
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
	mov	a, [sp+7]
	mov1	cy, a.7
	bnc	$div_unsigned_den
div_signed_den:	
	;; neg [sp+6]
	movw	ax, sp
	addw	ax, #6
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
	movw	ax, sp
	addw	ax, #6
	neg_ax
div_skip_restore_den:	
	ret
	.size	___divhi3, . - ___divhi3
	

	.global	___modhi3
	.type	___modhi3,@function
___modhi3:
	;; r8 = 4[sp] % 6[sp]
	movw	de, #0
	mov	a, [sp+5]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [sp+7]
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
	mov	a, [sp+7]
	mov1	cy, a.7
	bnc	$mod_unsigned_den
mod_signed_den:	
	;; neg [sp+6]
	movw	ax, sp
	addw	ax, #6
	neg_ax
mod_unsigned_den:	
	call	$!generic_mod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	movw	ax, #r8
	neg_ax
	;;  Also restore numerator
	movw 	ax, sp
	addw	ax, #4
	neg_ax
mod_no_neg:
	mov	a, e
	cmp0	a
	bz	$mod_skip_restore_den
	movw	ax, sp
	addw	ax, #6
	neg_ax
mod_skip_restore_den:	
	ret
	.size	___modhi3, . - ___modhi3

#endif
