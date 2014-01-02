/* QImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#if 1
#define bit	b
#define den	c
#define bitden	bc
#endif

num_lt_den\which:
	.if \need_result
	mov	r8, #0
	.else
	mov	a, [hl+4]
	mov	r8, a
	.endif
	ret

num_eq_den\which:
	.if \need_result
	mov	r8, #1
	.else
	mov	r8, #0
	.endif
	ret
	
den_is_zero\which:
	mov	r8, #0xff
	ret

	;; These routines leave DE alone - the signed functions use DE
	;; to store sign information that must remain intact

	.if \need_result

generic_div:

	.else

generic_mod:

	.endif

	;; (quot,rem) = 4[hl] /% 6[hl]

	mov	a, [hl+4] ; num
	cmp	a, [hl+6] ; den
	bz	$num_eq_den\which
	bnh	$num_lt_den\which
	
	;; copy numerator
;	mov	a, [hl+4]	; already there from above
	mov	num, a

	;; copy denomonator
	mov	a, [hl+6]
	mov	den, a

	cmp0	den
	bz	$den_is_zero\which

den_not_zero\which:
	.if \need_result
	;; zero out quot
	mov	quot, #0
	.endif

	;; initialize bit to 1
	mov	bit, #1

; while (den < num && !(den & (1L << BITS_MINUS_1)))

shift_den_bit\which:
	.macro	sdb_one\which
	mov	a, den
	mov1	cy,a.7
	bc	$enter_main_loop\which
	cmp	a, num
	bh	$enter_main_loop\which

	;; den <<= 1
;	mov	a, den		; already has it from the cmpw above
	shl	a, 1
	mov	den, a

	;; bit <<= 1
	shl	bit, 1
	.endm

	sdb_one\which
	sdb_one\which

	br	$shift_den_bit\which

main_loop\which:

	;; if (num >= den) (cmp den > num)
	mov	a, den
	cmp	a, num
	bh	$next_loop\which

	;; num -= den
	mov	a, num
	sub	a, den
	mov	num, a

	.if \need_result
	;; res |= bit
	mov	a, quot
	or	a, bit
	mov	quot, a
	.endif

next_loop\which:	

	;; den, bit >>= 1
	movw	ax, bitden
	shrw	ax, 1
	movw	bitden, ax

enter_main_loop\which:
	cmp0	bit
	bnz	$main_loop\which

main_loop_done\which:	
	ret
	.endm

	make_generic _d 1
	make_generic _m 0

;----------------------------------------------------------------------

	.global	___udivqi3
	.type	___udivqi3,@function
___udivqi3:
	;; r8 = 4[sp] / 6[sp]
	movw	hl, sp
	br	$!generic_div
	.size	___udivqi3, . - ___udivqi3
	

	.global	___umodqi3
	.type	___umodqi3,@function
___umodqi3:
	;; r8 = 4[sp] % 6[sp]
	movw	hl, sp
	br	$!generic_mod
	.size	___umodqi3, . - ___umodqi3

;----------------------------------------------------------------------

	.macro neg_ax
	movw	hl, ax
	mov	a, #0
	sub	a, [hl]
	mov	[hl], a
	.endm

	.global	___divqi3
	.type	___divqi3,@function
___divqi3:
	;; r8 = 4[sp] / 6[sp]
	movw	hl, sp
	movw	de, #0
	mov	a, [sp+4]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+6]
	mov1	cy, a.7
	bc	$div_signed_den
	br	$!generic_div
	
div_signed_num:
	;; neg [sp+4]
	mov	a, #0
	sub	a, [hl+4]
	mov	[hl+4], a
	mov	d, #1
	mov	a, [sp+6]
	mov1	cy, a.6
	bnc	$div_unsigned_den
div_signed_den:	
	;; neg [sp+6]
	mov	a, #0
	sub	a, [hl+6]
	mov	[hl+6], a
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
	.size	___divqi3, . - ___divqi3
	

	.global	___modqi3
	.type	___modqi3,@function
___modqi3:
	;; r8 = 4[sp] % 6[sp]
	movw	hl, sp
	movw	de, #0
	mov	a, [hl+4]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [hl+6]
	mov1	cy, a.7
	bc	$mod_signed_den
	br	$!generic_mod
	
mod_signed_num:
	;; neg [sp+4]
	mov	a, #0
	sub	a, [hl+4]
	mov	[hl+4], a
	mov	d, #1
	mov	a, [hl+6]
	mov1	cy, a.7
	bnc	$mod_unsigned_den
mod_signed_den:	
	;; neg [sp+6]
	mov	a, #0
	sub	a, [hl+6]
	mov	[hl+6], a
	mov	e, #1
mod_unsigned_den:	
	call	$!generic_mod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	mov	a, #0
	sub	a, r8
	mov	r8, a
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
	.size	___modqi3, . - ___modqi3

#endif
