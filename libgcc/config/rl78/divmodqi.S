/* QImode div/mod functions for the GCC support library for the Renesas RL78 processors.
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

#define bit	b
#define den	c
#define bitden	bc

	START_FUNC __generic_qidivmod\which

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
	mov	r8, #0x00
	ret

	;; These routines leave DE alone - the signed functions use DE
	;; to store sign information that must remain intact

	.if \need_result
	.global __generic_qidiv
__generic_qidiv:

	.else

	.global __generic_qimod
__generic_qimod:

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

.macro	SDB_ONE\which
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

	SDB_ONE\which
	SDB_ONE\which

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
	END_FUNC __generic_qidivmod\which
.endm

;----------------------------------------------------------------------

	MAKE_GENERIC _d 1
	MAKE_GENERIC _m 0

;----------------------------------------------------------------------

START_FUNC ___udivqi3
	;; r8 = 4[sp] / 6[sp]
	movw	hl, sp
	br	$!__generic_qidiv
END_FUNC ___udivqi3
	

START_FUNC ___umodqi3
	;; r8 = 4[sp] % 6[sp]
	movw	hl, sp
	br	$!__generic_qimod
END_FUNC ___umodqi3

;----------------------------------------------------------------------

.macro NEG_AX
	movw	hl, ax
	mov	a, #0
	sub	a, [hl]
	mov	[hl], a
.endm

;----------------------------------------------------------------------

START_FUNC	___divqi3
	;; r8 = 4[sp] / 6[sp]
	movw	hl, sp
	movw	de, #0
	mov	a, [sp+4]
	mov1	cy, a.7
	bc	$div_signed_num
	mov	a, [sp+6]
	mov1	cy, a.7
	bc	$div_signed_den
	br	$!__generic_qidiv
	
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
	call	$!__generic_qidiv

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
END_FUNC ___divqi3
	

START_FUNC ___modqi3
	;; r8 = 4[sp] % 6[sp]
	movw	hl, sp
	movw	de, #0
	mov	a, [hl+4]
	mov1	cy, a.7
	bc	$mod_signed_num
	mov	a, [hl+6]
	mov1	cy, a.7
	bc	$mod_signed_den
	br	$!__generic_qimod
	
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
	call	$!__generic_qimod

	mov	a, d
	cmp0	a
	bz	$mod_no_neg
	mov	a, #0
	sub	a, r8
	mov	r8, a
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
END_FUNC ___modqi3
