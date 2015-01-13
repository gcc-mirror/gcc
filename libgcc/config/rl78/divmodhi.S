/* HImode div/mod functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
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
