/* ieee754-df.S double-precision floating point support for ARM

   Copyright (C) 2003-2019 Free Software Foundation, Inc.
   Contributed by Nicolas Pitre (nico@fluxnic.net)

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Notes: 
 * 
 * The goal of this code is to be as fast as possible.  This is
 * not meant to be easy to understand for the casual reader.
 * For slightly simpler code please see the single precision version
 * of this file.
 * 
 * Only the default rounding mode is intended for best performances.
 * Exceptions aren't supported yet, but that can be added quite easily
 * if necessary without impacting performances.
 *
 * In the CFI related comments, 'previousOffset' refers to the previous offset
 * from sp used to compute the CFA.
 */

	.cfi_sections .debug_frame

#ifndef __ARMEB__
#define xl r0
#define xh r1
#define yl r2
#define yh r3
#else
#define xh r0
#define xl r1
#define yh r2
#define yl r3
#endif


#ifdef L_arm_negdf2

ARM_FUNC_START negdf2
ARM_FUNC_ALIAS aeabi_dneg negdf2
	CFI_START_FUNCTION

	@ flip sign bit
	eor	xh, xh, #0x80000000
	RET

	CFI_END_FUNCTION
	FUNC_END aeabi_dneg
	FUNC_END negdf2

#endif

#ifdef L_arm_addsubdf3

ARM_FUNC_START aeabi_drsub
	CFI_START_FUNCTION

	eor	xh, xh, #0x80000000	@ flip sign bit of first arg
	b	1f	

ARM_FUNC_START subdf3
ARM_FUNC_ALIAS aeabi_dsub subdf3

	eor	yh, yh, #0x80000000	@ flip sign bit of second arg
#if defined(__INTERWORKING_STUBS__)
	b	1f			@ Skip Thumb-code prologue
#endif

ARM_FUNC_START adddf3
ARM_FUNC_ALIAS aeabi_dadd adddf3

1:  do_push {r4, r5, lr}        @ sp -= 12
	.cfi_adjust_cfa_offset 12   @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0       @ Registers are saved from sp to sp + 8
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	@ Look for zeroes, equal values, INF, or NAN.
	shift1	lsl, r4, xh, #1
	shift1	lsl, r5, yh, #1
	teq	r4, r5
	do_it	eq
	teqeq	xl, yl
	do_it	ne, ttt
	COND(orr,s,ne)	ip, r4, xl
	COND(orr,s,ne)	ip, r5, yl
	COND(mvn,s,ne)	ip, r4, asr #21
	COND(mvn,s,ne)	ip, r5, asr #21
	beq	LSYM(Lad_s)

	@ Compute exponent difference.  Make largest exponent in r4,
	@ corresponding arg in xh-xl, and positive exponent difference in r5.
	shift1	lsr, r4, r4, #21
	rsbs	r5, r4, r5, lsr #21
	do_it	lt
	rsblt	r5, r5, #0
	ble	1f
	add	r4, r4, r5
	eor	yl, xl, yl
	eor	yh, xh, yh
	eor	xl, yl, xl
	eor	xh, yh, xh
	eor	yl, xl, yl
	eor	yh, xh, yh
1:
	@ If exponent difference is too large, return largest argument
	@ already in xh-xl.  We need up to 54 bit to handle proper rounding
	@ of 0x1p54 - 1.1.
	cmp	r5, #54
	do_it	hi
	RETLDM	"r4, r5" hi

	@ Convert mantissa to signed integer.
	tst	xh, #0x80000000
	mov	xh, xh, lsl #12
	mov	ip, #0x00100000
	orr	xh, ip, xh, lsr #12
	beq	1f
#if defined(__thumb2__)
	negs	xl, xl
	sbc	xh, xh, xh, lsl #1
#else
	rsbs	xl, xl, #0
	rsc	xh, xh, #0
#endif
1:
	tst	yh, #0x80000000
	mov	yh, yh, lsl #12
	orr	yh, ip, yh, lsr #12
	beq	1f
#if defined(__thumb2__)
	negs	yl, yl
	sbc	yh, yh, yh, lsl #1
#else
	rsbs	yl, yl, #0
	rsc	yh, yh, #0
#endif
1:
	@ If exponent == difference, one or both args were denormalized.
	@ Since this is not common case, rescale them off line.
	teq	r4, r5
	beq	LSYM(Lad_d)

@ CFI note: we're lucky that the branches to Lad_* that appear after this
@ function have a CFI state that's exactly the same as the one we're in at this
@ point. Otherwise the CFI would change to a different state after the branch,
@ which would be disastrous for backtracing.
LSYM(Lad_x):

	@ Compensate for the exponent overlapping the mantissa MSB added later
	sub	r4, r4, #1

	@ Shift yh-yl right per r5, add to xh-xl, keep leftover bits into ip.
	rsbs	lr, r5, #32
	blt	1f
	shift1	lsl, ip, yl, lr
	shiftop adds xl xl yl lsr r5 yl
	adc	xh, xh, #0
	shiftop adds xl xl yh lsl lr yl
	shiftop adcs xh xh yh asr r5 yh
	b	2f
1:	sub	r5, r5, #32
	add	lr, lr, #32
	cmp	yl, #1
	shift1	lsl,ip, yh, lr
	do_it	cs
	orrcs	ip, ip, #2		@ 2 not 1, to allow lsr #1 later
	shiftop adds xl xl yh asr r5 yh
	adcs	xh, xh, yh, asr #31
2:
	@ We now have a result in xh-xl-ip.
	@ Keep absolute value in xh-xl-ip, sign in r5 (the n bit was set above)
	and	r5, xh, #0x80000000
	bpl	LSYM(Lad_p)
#if defined(__thumb2__)
	mov	lr, #0
	negs	ip, ip
	sbcs	xl, lr, xl
	sbc	xh, lr, xh
#else
	rsbs	ip, ip, #0
	rscs	xl, xl, #0
	rsc	xh, xh, #0
#endif

	@ Determine how to normalize the result.
LSYM(Lad_p):
	cmp	xh, #0x00100000
	bcc	LSYM(Lad_a)
	cmp	xh, #0x00200000
	bcc	LSYM(Lad_e)

	@ Result needs to be shifted right.
	movs	xh, xh, lsr #1
	movs	xl, xl, rrx
	mov	ip, ip, rrx
	add	r4, r4, #1

	@ Make sure we did not bust our exponent.
	mov	r2, r4, lsl #21
	cmn	r2, #(2 << 21)
	bcs	LSYM(Lad_o)

	@ Our result is now properly aligned into xh-xl, remaining bits in ip.
	@ Round with MSB of ip. If halfway between two numbers, round towards
	@ LSB of xl = 0.
	@ Pack final result together.
LSYM(Lad_e):
	cmp	ip, #0x80000000
	do_it	eq
	COND(mov,s,eq)	ip, xl, lsr #1
	adcs	xl, xl, #0
	adc	xh, xh, r4, lsl #20
	orr	xh, xh, r5
	RETLDM	"r4, r5"

	@ Result must be shifted left and exponent adjusted.
LSYM(Lad_a):
	movs	ip, ip, lsl #1
	adcs	xl, xl, xl
	adc	xh, xh, xh
	subs	r4, r4, #1
	do_it	hs
	cmphs	xh, #0x00100000
	bhs	LSYM(Lad_e)

	@ No rounding necessary since ip will always be 0 at this point.
LSYM(Lad_l):

#if !defined (__ARM_FEATURE_CLZ)

	teq	xh, #0
	movne	r3, #20
	moveq	r3, #52
	moveq	xh, xl
	moveq	xl, #0
	mov	r2, xh
	cmp	r2, #(1 << 16)
	movhs	r2, r2, lsr #16
	subhs	r3, r3, #16
	cmp	r2, #(1 << 8)
	movhs	r2, r2, lsr #8
	subhs	r3, r3, #8
	cmp	r2, #(1 << 4)
	movhs	r2, r2, lsr #4
	subhs	r3, r3, #4
	cmp	r2, #(1 << 2)
	subhs	r3, r3, #2
	sublo	r3, r3, r2, lsr #1
	sub	r3, r3, r2, lsr #3

#else

	teq	xh, #0
	do_it	eq, t
	moveq	xh, xl
	moveq	xl, #0
	clz	r3, xh
	do_it	eq
	addeq	r3, r3, #32
	sub	r3, r3, #11

#endif

	@ determine how to shift the value.
	subs	r2, r3, #32
	bge	2f
	adds	r2, r2, #12
	ble	1f

	@ shift value left 21 to 31 bits, or actually right 11 to 1 bits
	@ since a register switch happened above.
	add	ip, r2, #20
	rsb	r2, r2, #12
	shift1	lsl, xl, xh, ip
	shift1	lsr, xh, xh, r2
	b	3f

	@ actually shift value left 1 to 20 bits, which might also represent
	@ 32 to 52 bits if counting the register switch that happened earlier.
1:	add	r2, r2, #20
2:	do_it	le
	rsble	ip, r2, #32
	shift1	lsl, xh, xh, r2
#if defined(__thumb2__)
	lsr	ip, xl, ip
	itt	le
	orrle	xh, xh, ip
	lslle	xl, xl, r2
#else
	orrle	xh, xh, xl, lsr ip
	movle	xl, xl, lsl r2
#endif

	@ adjust exponent accordingly.
3:	subs	r4, r4, r3
	do_it	ge, tt
	addge	xh, xh, r4, lsl #20
	orrge	xh, xh, r5
	RETLDM	"r4, r5" ge

	@ Exponent too small, denormalize result.
	@ Find out proper shift value.
	mvn	r4, r4
	subs	r4, r4, #31
	bge	2f
	adds	r4, r4, #12
	bgt	1f

	@ shift result right of 1 to 20 bits, sign is in r5.
	add	r4, r4, #20
	rsb	r2, r4, #32
	shift1	lsr, xl, xl, r4
	shiftop orr xl xl xh lsl r2 yh
	shiftop orr xh r5 xh lsr r4 yh
	RETLDM	"r4, r5"

	@ shift result right of 21 to 31 bits, or left 11 to 1 bits after
	@ a register switch from xh to xl.
1:	rsb	r4, r4, #12
	rsb	r2, r4, #32
	shift1	lsr, xl, xl, r2
	shiftop orr xl xl xh lsl r4 yh
	mov	xh, r5
	RETLDM	"r4, r5"

	@ Shift value right of 32 to 64 bits, or 0 to 32 bits after a switch
	@ from xh to xl.
2:	shift1	lsr, xl, xh, r4
	mov	xh, r5
	RETLDM	"r4, r5"

	@ Adjust exponents for denormalized arguments.
	@ Note that r4 must not remain equal to 0.
LSYM(Lad_d):
	teq	r4, #0
	eor	yh, yh, #0x00100000
	do_it	eq, te
	eoreq	xh, xh, #0x00100000
	addeq	r4, r4, #1
	subne	r5, r5, #1
	b	LSYM(Lad_x)


LSYM(Lad_s):
	mvns	ip, r4, asr #21
	do_it	ne
	COND(mvn,s,ne)	ip, r5, asr #21
	beq	LSYM(Lad_i)

	teq	r4, r5
	do_it	eq
	teqeq	xl, yl
	beq	1f

	@ Result is x + 0.0 = x or 0.0 + y = y.
	orrs	ip, r4, xl
	do_it	eq, t
	moveq	xh, yh
	moveq	xl, yl
	RETLDM	"r4, r5"

1:	teq	xh, yh

	@ Result is x - x = 0.
	do_it	ne, tt
	movne	xh, #0
	movne	xl, #0
	RETLDM	"r4, r5" ne

	@ Result is x + x = 2x.
	movs	ip, r4, lsr #21
	bne	2f
	movs	xl, xl, lsl #1
	adcs	xh, xh, xh
	do_it	cs
	orrcs	xh, xh, #0x80000000
	RETLDM	"r4, r5"
2:	adds	r4, r4, #(2 << 21)
	do_it	cc, t
	addcc	xh, xh, #(1 << 20)
	RETLDM	"r4, r5" cc
	and	r5, xh, #0x80000000

	@ Overflow: return INF.
LSYM(Lad_o):
	orr	xh, r5, #0x7f000000
	orr	xh, xh, #0x00f00000
	mov	xl, #0
	RETLDM	"r4, r5"

	@ At least one of x or y is INF/NAN.
	@   if xh-xl != INF/NAN: return yh-yl (which is INF/NAN)
	@   if yh-yl != INF/NAN: return xh-xl (which is INF/NAN)
	@   if either is NAN: return NAN
	@   if opposite sign: return NAN
	@   otherwise return xh-xl (which is INF or -INF)
LSYM(Lad_i):
	mvns	ip, r4, asr #21
	do_it	ne, te
	movne	xh, yh
	movne	xl, yl
	COND(mvn,s,eq)	ip, r5, asr #21
	do_it	ne, t
	movne	yh, xh
	movne	yl, xl
	orrs	r4, xl, xh, lsl #12
	do_it	eq, te
	COND(orr,s,eq)	r5, yl, yh, lsl #12
	teqeq	xh, yh
	orrne	xh, xh, #0x00080000	@ quiet NAN
	RETLDM	"r4, r5"

	CFI_END_FUNCTION
	FUNC_END aeabi_dsub
	FUNC_END subdf3
	FUNC_END aeabi_dadd
	FUNC_END adddf3

ARM_FUNC_START floatunsidf
ARM_FUNC_ALIAS aeabi_ui2d floatunsidf
	CFI_START_FUNCTION

	teq	r0, #0
	do_it	eq, t
	moveq	r1, #0
	RETc(eq)

	do_push {r4, r5, lr}        @ sp -= 12
	.cfi_adjust_cfa_offset 12   @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0       @ Registers are saved from sp + 0 to sp + 8.
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	mov	r4, #0x400		@ initial exponent
	add	r4, r4, #(52-1 - 1)
	mov	r5, #0			@ sign bit is 0
	.ifnc	xl, r0
	mov	xl, r0
	.endif
	mov	xh, #0
	b	LSYM(Lad_l)

	CFI_END_FUNCTION
	FUNC_END aeabi_ui2d
	FUNC_END floatunsidf

ARM_FUNC_START floatsidf
ARM_FUNC_ALIAS aeabi_i2d floatsidf
	CFI_START_FUNCTION

	teq	r0, #0
	do_it	eq, t
	moveq	r1, #0
	RETc(eq)

	do_push {r4, r5, lr}        @ sp -= 12
	.cfi_adjust_cfa_offset 12   @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0       @ Registers are saved from sp + 0 to sp + 8.
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	mov	r4, #0x400		@ initial exponent
	add	r4, r4, #(52-1 - 1)
	ands	r5, r0, #0x80000000	@ sign bit in r5
	do_it	mi
	rsbmi	r0, r0, #0		@ absolute value
	.ifnc	xl, r0
	mov	xl, r0
	.endif
	mov	xh, #0
	b	LSYM(Lad_l)

	CFI_END_FUNCTION
	FUNC_END aeabi_i2d
	FUNC_END floatsidf

ARM_FUNC_START extendsfdf2
ARM_FUNC_ALIAS aeabi_f2d extendsfdf2
	CFI_START_FUNCTION

	movs	r2, r0, lsl #1		@ toss sign bit
	mov	xh, r2, asr #3		@ stretch exponent
	mov	xh, xh, rrx		@ retrieve sign bit
	mov	xl, r2, lsl #28		@ retrieve remaining bits
	do_it	ne, ttt
	COND(and,s,ne)	r3, r2, #0xff000000	@ isolate exponent
	teqne	r3, #0xff000000		@ if not 0, check if INF or NAN
	eorne	xh, xh, #0x38000000	@ fixup exponent otherwise.
	RETc(ne)			@ and return it.

	bics	r2, r2, #0xff000000	@ isolate mantissa
	do_it	eq			@ if 0, that is ZERO or INF,
	RETc(eq)			@ we are done already.

	teq	r3, #0xff000000		@ check for NAN
	do_it	eq, t
	orreq	xh, xh, #0x00080000	@ change to quiet NAN
	RETc(eq)			@ and return it.

	@ value was denormalized.  We can normalize it now.
	do_push	{r4, r5, lr}
	.cfi_adjust_cfa_offset 12   @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0       @ Registers are saved from sp + 0 to sp + 8.
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	mov	r4, #0x380		@ setup corresponding exponent
	and	r5, xh, #0x80000000	@ move sign bit in r5
	bic	xh, xh, #0x80000000
	b	LSYM(Lad_l)

	CFI_END_FUNCTION
	FUNC_END aeabi_f2d
	FUNC_END extendsfdf2

ARM_FUNC_START floatundidf
ARM_FUNC_ALIAS aeabi_ul2d floatundidf
	CFI_START_FUNCTION
	.cfi_remember_state        @ Save the current CFA state.

	orrs	r2, r0, r1
	do_it	eq
	RETc(eq)

	do_push {r4, r5, lr}       @ sp -= 12
	.cfi_adjust_cfa_offset 12  @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0      @ Registers are saved from sp + 0 to sp + 8
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	mov	r5, #0
	b	2f

ARM_FUNC_START floatdidf
ARM_FUNC_ALIAS aeabi_l2d floatdidf
	.cfi_restore_state
	@ Restore the CFI state we saved above. If we didn't do this then the
	@ following instructions would have the CFI state that was set by the
	@ offset adjustments made in floatundidf.

	orrs	r2, r0, r1
	do_it	eq
	RETc(eq)

	do_push {r4, r5, lr}       @ sp -= 12
	.cfi_adjust_cfa_offset 12  @ CFA is now sp + previousOffset + 12
	.cfi_rel_offset r4, 0      @ Registers are saved from sp to sp + 8
	.cfi_rel_offset r5, 4
	.cfi_rel_offset lr, 8

	ands	r5, ah, #0x80000000	@ sign bit in r5
	bpl	2f
#if defined(__thumb2__)
	negs	al, al
	sbc	ah, ah, ah, lsl #1
#else
	rsbs	al, al, #0
	rsc	ah, ah, #0
#endif
2:
	mov	r4, #0x400		@ initial exponent
	add	r4, r4, #(52-1 - 1)

	@ If FP word order does not match integer word order, swap the words.
	.ifnc	xh, ah
	mov	ip, al
	mov	xh, ah
	mov	xl, ip
	.endif

	movs	ip, xh, lsr #22
	beq	LSYM(Lad_p)

	@ The value is too big.  Scale it down a bit...
	mov	r2, #3
	movs	ip, ip, lsr #3
	do_it	ne
	addne	r2, r2, #3
	movs	ip, ip, lsr #3
	do_it	ne
	addne	r2, r2, #3
	add	r2, r2, ip, lsr #3

	rsb	r3, r2, #32
	shift1	lsl, ip, xl, r3
	shift1	lsr, xl, xl, r2
	shiftop orr xl xl xh lsl r3 lr
	shift1	lsr, xh, xh, r2
	add	r4, r4, r2
	b	LSYM(Lad_p)

	CFI_END_FUNCTION
	FUNC_END floatdidf
	FUNC_END aeabi_l2d
	FUNC_END floatundidf
	FUNC_END aeabi_ul2d

#endif /* L_addsubdf3 */

#if defined(L_arm_muldf3) || defined(L_arm_muldivdf3)

@ Define multiplication as weak in _arm_muldf3.o so that it can be overriden
@ by the global definition in _arm_muldivdf3.o.  This allows a program only
@ using multiplication to take the weak definition which does not contain the
@ division code. Programs using only division or both division and
@ multiplication will pull _arm_muldivdf3.o from which both the multiplication
@ and division are taken thanks to the override.
#ifdef L_arm_muldf3
WEAK muldf3
WEAK aeabi_dmul
#endif

ARM_FUNC_START muldf3
ARM_FUNC_ALIAS aeabi_dmul muldf3
	CFI_START_FUNCTION

	do_push {r4, r5, r6, lr}    @ sp -= 16
	.cfi_adjust_cfa_offset 16   @ CFA is now sp + previousOffset + 16
	.cfi_rel_offset r4, 0       @ Registers are saved from sp to sp + 12.
	.cfi_rel_offset r5, 4
	.cfi_rel_offset r6, 8
	.cfi_rel_offset lr, 12

	@ Mask out exponents, trap any zero/denormal/INF/NAN.
	mov	ip, #0xff
	orr	ip, ip, #0x700
	ands	r4, ip, xh, lsr #20
	do_it	ne, tte
	COND(and,s,ne)	r5, ip, yh, lsr #20
	teqne	r4, ip
	teqne	r5, ip
	bleq	LSYM(Lml_s)

	@ Add exponents together
	add	r4, r4, r5

	@ Determine final sign.
	eor	r6, xh, yh

	@ Convert mantissa to unsigned integer.
	@ If power of two, branch to a separate path.
	bic	xh, xh, ip, lsl #21
	bic	yh, yh, ip, lsl #21
	orrs	r5, xl, xh, lsl #12
	do_it	ne
	COND(orr,s,ne)	r5, yl, yh, lsl #12
	orr	xh, xh, #0x00100000
	orr	yh, yh, #0x00100000
	beq	LSYM(Lml_1)

	@ Here is the actual multiplication.
	@ This code works on architecture versions >= 4
	umull	ip, lr, xl, yl
	mov	r5, #0
	umlal	lr, r5, xh, yl
	and	yl, r6, #0x80000000
	umlal	lr, r5, xl, yh
	mov	r6, #0
	umlal	r5, r6, xh, yh

	@ The LSBs in ip are only significant for the final rounding.
	@ Fold them into lr.
	teq	ip, #0
	do_it	ne
	orrne	lr, lr, #1

	@ Adjust result upon the MSB position.
	sub	r4, r4, #0xff
	cmp	r6, #(1 << (20-11))
	sbc	r4, r4, #0x300
	bcs	1f
	movs	lr, lr, lsl #1
	adcs	r5, r5, r5
	adc	r6, r6, r6
1:
	@ Shift to final position, add sign to result.
	orr	xh, yl, r6, lsl #11
	orr	xh, xh, r5, lsr #21
	mov	xl, r5, lsl #11
	orr	xl, xl, lr, lsr #21
	mov	lr, lr, lsl #11

	@ Check exponent range for under/overflow.
	subs	ip, r4, #(254 - 1)
	do_it	hi
	cmphi	ip, #0x700
	bhi	LSYM(Lml_u)

	@ Round the result, merge final exponent.
	cmp	lr, #0x80000000
	do_it	eq
	COND(mov,s,eq)	lr, xl, lsr #1
	adcs	xl, xl, #0
	adc	xh, xh, r4, lsl #20
	RETLDM	"r4, r5, r6"

	@ Multiplication by 0x1p*: let''s shortcut a lot of code.
LSYM(Lml_1):
	and	r6, r6, #0x80000000
	orr	xh, r6, xh
	orr	xl, xl, yl
	eor	xh, xh, yh
	subs	r4, r4, ip, lsr #1
	do_it	gt, tt
	COND(rsb,s,gt)	r5, r4, ip
	orrgt	xh, xh, r4, lsl #20
	RETLDM	"r4, r5, r6" gt

	@ Under/overflow: fix things up for the code below.
	orr	xh, xh, #0x00100000
	mov	lr, #0
	subs	r4, r4, #1
LSYM(Lml_u):
	@ Overflow?
	bgt	LSYM(Lml_o)

	@ Check if denormalized result is possible, otherwise return signed 0.
	cmn	r4, #(53 + 1)
	do_it	le, tt
	movle	xl, #0
	bicle	xh, xh, #0x7fffffff
	RETLDM	"r4, r5, r6" le

	@ Find out proper shift value.
	rsb	r4, r4, #0
	subs	r4, r4, #32
	bge	2f
	adds	r4, r4, #12
	bgt	1f

	@ shift result right of 1 to 20 bits, preserve sign bit, round, etc.
	add	r4, r4, #20
	rsb	r5, r4, #32
	shift1	lsl, r3, xl, r5
	shift1	lsr, xl, xl, r4
	shiftop orr xl xl xh lsl r5 r2
	and	r2, xh, #0x80000000
	bic	xh, xh, #0x80000000
	adds	xl, xl, r3, lsr #31
	shiftop adc xh r2 xh lsr r4 r6
	orrs	lr, lr, r3, lsl #1
	do_it	eq
	biceq	xl, xl, r3, lsr #31
	RETLDM	"r4, r5, r6"

	@ shift result right of 21 to 31 bits, or left 11 to 1 bits after
	@ a register switch from xh to xl. Then round.
1:	rsb	r4, r4, #12
	rsb	r5, r4, #32
	shift1	lsl, r3, xl, r4
	shift1	lsr, xl, xl, r5
	shiftop orr xl xl xh lsl r4 r2
	bic	xh, xh, #0x7fffffff
	adds	xl, xl, r3, lsr #31
	adc	xh, xh, #0
	orrs	lr, lr, r3, lsl #1
	do_it	eq
	biceq	xl, xl, r3, lsr #31
	RETLDM	"r4, r5, r6"

	@ Shift value right of 32 to 64 bits, or 0 to 32 bits after a switch
	@ from xh to xl.  Leftover bits are in r3-r6-lr for rounding.
2:	rsb	r5, r4, #32
	shiftop orr lr lr xl lsl r5 r2
	shift1	lsr, r3, xl, r4
	shiftop orr r3 r3 xh lsl r5 r2
	shift1	lsr, xl, xh, r4
	bic	xh, xh, #0x7fffffff
	shiftop bic xl xl xh lsr r4 r2
	add	xl, xl, r3, lsr #31
	orrs	lr, lr, r3, lsl #1
	do_it	eq
	biceq	xl, xl, r3, lsr #31
	RETLDM	"r4, r5, r6"

	@ One or both arguments are denormalized.
	@ Scale them leftwards and preserve sign bit.
LSYM(Lml_d):
	teq	r4, #0
	bne	2f
	and	r6, xh, #0x80000000
1:	movs	xl, xl, lsl #1
	adc	xh, xh, xh
	tst	xh, #0x00100000
	do_it	eq
	subeq	r4, r4, #1
	beq	1b
	orr	xh, xh, r6
	teq	r5, #0
	do_it	ne
	RETc(ne)
2:	and	r6, yh, #0x80000000
3:	movs	yl, yl, lsl #1
	adc	yh, yh, yh
	tst	yh, #0x00100000
	do_it	eq
	subeq	r5, r5, #1
	beq	3b
	orr	yh, yh, r6
	RET

LSYM(Lml_s):
	@ Isolate the INF and NAN cases away
	teq	r4, ip
	and	r5, ip, yh, lsr #20
	do_it	ne
	teqne	r5, ip
	beq	1f

	@ Here, one or more arguments are either denormalized or zero.
	orrs	r6, xl, xh, lsl #1
	do_it	ne
	COND(orr,s,ne)	r6, yl, yh, lsl #1
	bne	LSYM(Lml_d)

	@ Result is 0, but determine sign anyway.
LSYM(Lml_z):
	eor	xh, xh, yh
	and	xh, xh, #0x80000000
	mov	xl, #0
	RETLDM	"r4, r5, r6"

1:	@ One or both args are INF or NAN.
	orrs	r6, xl, xh, lsl #1
	do_it	eq, te
	moveq	xl, yl
	moveq	xh, yh
	COND(orr,s,ne)	r6, yl, yh, lsl #1
	beq	LSYM(Lml_n)		@ 0 * INF or INF * 0 -> NAN
	teq	r4, ip
	bne	1f
	orrs	r6, xl, xh, lsl #12
	bne	LSYM(Lml_n)		@ NAN * <anything> -> NAN
1:	teq	r5, ip
	bne	LSYM(Lml_i)
	orrs	r6, yl, yh, lsl #12
	do_it	ne, t
	movne	xl, yl
	movne	xh, yh
	bne	LSYM(Lml_n)		@ <anything> * NAN -> NAN

	@ Result is INF, but we need to determine its sign.
LSYM(Lml_i):
	eor	xh, xh, yh

	@ Overflow: return INF (sign already in xh).
LSYM(Lml_o):
	and	xh, xh, #0x80000000
	orr	xh, xh, #0x7f000000
	orr	xh, xh, #0x00f00000
	mov	xl, #0
	RETLDM	"r4, r5, r6"

	@ Return a quiet NAN.
LSYM(Lml_n):
	orr	xh, xh, #0x7f000000
	orr	xh, xh, #0x00f80000
	RETLDM	"r4, r5, r6"

	CFI_END_FUNCTION
	FUNC_END aeabi_dmul
	FUNC_END muldf3

#ifdef L_arm_muldivdf3

ARM_FUNC_START divdf3
ARM_FUNC_ALIAS aeabi_ddiv divdf3
	CFI_START_FUNCTION
	
	do_push	{r4, r5, r6, lr}
	.cfi_adjust_cfa_offset 16
	.cfi_rel_offset r4, 0
	.cfi_rel_offset r5, 4
	.cfi_rel_offset r6, 8
	.cfi_rel_offset lr, 12

	@ Mask out exponents, trap any zero/denormal/INF/NAN.
	mov	ip, #0xff
	orr	ip, ip, #0x700
	ands	r4, ip, xh, lsr #20
	do_it	ne, tte
	COND(and,s,ne)	r5, ip, yh, lsr #20
	teqne	r4, ip
	teqne	r5, ip
	bleq	LSYM(Ldv_s)

	@ Subtract divisor exponent from dividend''s.
	sub	r4, r4, r5

	@ Preserve final sign into lr.
	eor	lr, xh, yh

	@ Convert mantissa to unsigned integer.
	@ Dividend -> r5-r6, divisor -> yh-yl.
	orrs	r5, yl, yh, lsl #12
	mov	xh, xh, lsl #12
	beq	LSYM(Ldv_1)
	mov	yh, yh, lsl #12
	mov	r5, #0x10000000
	orr	yh, r5, yh, lsr #4
	orr	yh, yh, yl, lsr #24
	mov	yl, yl, lsl #8
	orr	r5, r5, xh, lsr #4
	orr	r5, r5, xl, lsr #24
	mov	r6, xl, lsl #8

	@ Initialize xh with final sign bit.
	and	xh, lr, #0x80000000

	@ Ensure result will land to known bit position.
	@ Apply exponent bias accordingly.
	cmp	r5, yh
	do_it	eq
	cmpeq	r6, yl
	adc	r4, r4, #(255 - 2)
	add	r4, r4, #0x300
	bcs	1f
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
1:
	@ Perform first subtraction to align result to a nibble.
	subs	r6, r6, yl
	sbc	r5, r5, yh
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	mov	xl, #0x00100000
	mov	ip, #0x00080000

	@ The actual division loop.
1:	subs	lr, r6, yl
	sbcs	lr, r5, yh
	do_it	cs, tt
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	do_it	cs, tt
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #1
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	do_it	cs, tt
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #2
	movs	yh, yh, lsr #1
	mov	yl, yl, rrx
	subs	lr, r6, yl
	sbcs	lr, r5, yh
	do_it	cs, tt
	subcs	r6, r6, yl
	movcs	r5, lr
	orrcs	xl, xl, ip, lsr #3

	orrs	lr, r5, r6
	beq	2f
	mov	r5, r5, lsl #4
	orr	r5, r5, r6, lsr #28
	mov	r6, r6, lsl #4
	mov	yh, yh, lsl #3
	orr	yh, yh, yl, lsr #29
	mov	yl, yl, lsl #3
	movs	ip, ip, lsr #4
	bne	1b

	@ We are done with a word of the result.
	@ Loop again for the low word if this pass was for the high word.
	tst	xh, #0x00100000
	bne	3f
	orr	xh, xh, xl
	mov	xl, #0
	mov	ip, #0x80000000
	b	1b
2:
	@ Be sure result starts in the high word.
	tst	xh, #0x00100000
	do_it	eq, t
	orreq	xh, xh, xl
	moveq	xl, #0
3:
	@ Check exponent range for under/overflow.
	subs	ip, r4, #(254 - 1)
	do_it	hi
	cmphi	ip, #0x700
	bhi	LSYM(Lml_u)

	@ Round the result, merge final exponent.
	subs	ip, r5, yh
	do_it	eq, t
	COND(sub,s,eq)	ip, r6, yl
	COND(mov,s,eq)	ip, xl, lsr #1
	adcs	xl, xl, #0
	adc	xh, xh, r4, lsl #20
	RETLDM	"r4, r5, r6"

	@ Division by 0x1p*: shortcut a lot of code.
LSYM(Ldv_1):
	and	lr, lr, #0x80000000
	orr	xh, lr, xh, lsr #12
	adds	r4, r4, ip, lsr #1
	do_it	gt, tt
	COND(rsb,s,gt)	r5, r4, ip
	orrgt	xh, xh, r4, lsl #20
	RETLDM	"r4, r5, r6" gt

	orr	xh, xh, #0x00100000
	mov	lr, #0
	subs	r4, r4, #1
	b	LSYM(Lml_u)

	@ Result mightt need to be denormalized: put remainder bits
	@ in lr for rounding considerations.
LSYM(Ldv_u):
	orr	lr, r5, r6
	b	LSYM(Lml_u)

	@ One or both arguments is either INF, NAN or zero.
LSYM(Ldv_s):
	and	r5, ip, yh, lsr #20
	teq	r4, ip
	do_it	eq
	teqeq	r5, ip
	beq	LSYM(Lml_n)		@ INF/NAN / INF/NAN -> NAN
	teq	r4, ip
	bne	1f
	orrs	r4, xl, xh, lsl #12
	bne	LSYM(Lml_n)		@ NAN / <anything> -> NAN
	teq	r5, ip
	bne	LSYM(Lml_i)		@ INF / <anything> -> INF
	mov	xl, yl
	mov	xh, yh
	b	LSYM(Lml_n)		@ INF / (INF or NAN) -> NAN
1:	teq	r5, ip
	bne	2f
	orrs	r5, yl, yh, lsl #12
	beq	LSYM(Lml_z)		@ <anything> / INF -> 0
	mov	xl, yl
	mov	xh, yh
	b	LSYM(Lml_n)		@ <anything> / NAN -> NAN
2:	@ If both are nonzero, we need to normalize and resume above.
	orrs	r6, xl, xh, lsl #1
	do_it	ne
	COND(orr,s,ne)	r6, yl, yh, lsl #1
	bne	LSYM(Lml_d)
	@ One or both arguments are 0.
	orrs	r4, xl, xh, lsl #1
	bne	LSYM(Lml_i)		@ <non_zero> / 0 -> INF
	orrs	r5, yl, yh, lsl #1
	bne	LSYM(Lml_z)		@ 0 / <non_zero> -> 0
	b	LSYM(Lml_n)		@ 0 / 0 -> NAN

	CFI_END_FUNCTION
	FUNC_END aeabi_ddiv
	FUNC_END divdf3

#endif /* L_muldivdf3 */
#endif /* L_arm_muldf3 || L_arm_muldivdf3 */

#ifdef L_arm_cmpdf2

@ Note: only r0 (return value) and ip are clobbered here.

ARM_FUNC_START gtdf2
ARM_FUNC_ALIAS gedf2 gtdf2
	CFI_START_FUNCTION
	mov	ip, #-1
	b	1f

ARM_FUNC_START ltdf2
ARM_FUNC_ALIAS ledf2 ltdf2
	mov	ip, #1
	b	1f

ARM_FUNC_START cmpdf2
ARM_FUNC_ALIAS nedf2 cmpdf2
ARM_FUNC_ALIAS eqdf2 cmpdf2
	mov	ip, #1			@ how should we specify unordered here?

1:	str	ip, [sp, #-4]!
	.cfi_adjust_cfa_offset 4        @ CFA is now sp + previousOffset + 4.
	@ We're not adding CFI for ip as it's pushed into the stack
	@ only because it may be popped off later as a return value
	@ (i.e. we're not preserving it anyways).

	@ Trap any INF/NAN first.
	mov	ip, xh, lsl #1
	mvns	ip, ip, asr #21
	mov	ip, yh, lsl #1
	do_it	ne
	COND(mvn,s,ne)	ip, ip, asr #21
	beq	3f
	.cfi_remember_state
	@ Save the current CFI state.  This is done because the branch
	@ is conditional, and if we don't take it we'll issue a
	@ .cfi_adjust_cfa_offset and return.  If we do take it,
	@ however, the .cfi_adjust_cfa_offset from the non-branch code
	@ will affect the branch code as well.  To avoid this we'll
	@ restore the current state before executing the branch code.

	@ Test for equality.  Note that 0.0 is equal to -0.0.
2:	add	sp, sp, #4
	.cfi_adjust_cfa_offset -4       @ CFA is now sp + previousOffset.

	orrs	ip, xl, xh, lsl #1	@ if x == 0.0 or -0.0
	do_it	eq, e
	COND(orr,s,eq)	ip, yl, yh, lsl #1	@ and y == 0.0 or -0.0
	teqne	xh, yh			@ or xh == yh
	do_it	eq, tt
	teqeq	xl, yl			@ and xl == yl
	moveq	r0, #0			@ then equal.
	RETc(eq)

	@ Clear C flag
	cmn	r0, #0

	@ Compare sign, 
	teq	xh, yh

	@ Compare values if same sign
	do_it	pl
	cmppl	xh, yh
	do_it	eq
	cmpeq	xl, yl

	@ Result:
	do_it	cs, e
	movcs	r0, yh, asr #31
	mvncc	r0, yh, asr #31
	orr	r0, r0, #1
	RET

3:  @ Look for a NAN.

	@ Restore the previous CFI state (i.e. keep the CFI state as it was
	@ before the branch).
	.cfi_restore_state

	mov ip, xh, lsl #1
	mvns	ip, ip, asr #21
	bne	4f
	orrs	ip, xl, xh, lsl #12
	bne	5f			@ x is NAN
4:	mov	ip, yh, lsl #1
	mvns	ip, ip, asr #21
	bne	2b
	orrs	ip, yl, yh, lsl #12
	beq	2b			@ y is not NAN

5:	ldr	r0, [sp], #4		@ unordered return code
	.cfi_adjust_cfa_offset -4       @ CFA is now sp + previousOffset.

	RET

	CFI_END_FUNCTION
	FUNC_END gedf2
	FUNC_END gtdf2
	FUNC_END ledf2
	FUNC_END ltdf2
	FUNC_END nedf2
	FUNC_END eqdf2
	FUNC_END cmpdf2

ARM_FUNC_START aeabi_cdrcmple
	CFI_START_FUNCTION

	mov	ip, r0
	mov	r0, r2
	mov	r2, ip
	mov	ip, r1
	mov	r1, r3
	mov	r3, ip
	b	6f

ARM_FUNC_START aeabi_cdcmpeq
ARM_FUNC_ALIAS aeabi_cdcmple aeabi_cdcmpeq

	@ The status-returning routines are required to preserve all
	@ registers except ip, lr, and cpsr.
6:	do_push	{r0, lr}
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8.
	.cfi_rel_offset r0, 0     @ Previous r0 is saved at sp.
	.cfi_rel_offset lr, 4     @ Previous lr is saved at sp + 4.

	ARM_CALL cmpdf2
	@ Set the Z flag correctly, and the C flag unconditionally.
	cmp	r0, #0
	@ Clear the C flag if the return value was -1, indicating
	@ that the first operand was smaller than the second.
	do_it	mi
	cmnmi	r0, #0

	RETLDM	"r0"

	CFI_END_FUNCTION
	FUNC_END aeabi_cdcmple
	FUNC_END aeabi_cdcmpeq
	FUNC_END aeabi_cdrcmple
	
ARM_FUNC_START	aeabi_dcmpeq
	CFI_START_FUNCTION

	str lr, [sp, #-8]!        @ sp -= 8
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8
	.cfi_rel_offset lr, 0     @ lr is at sp

	ARM_CALL aeabi_cdcmple
	do_it	eq, e
	moveq	r0, #1	@ Equal to.
	movne	r0, #0	@ Less than, greater than, or unordered.

	RETLDM

	CFI_END_FUNCTION
	FUNC_END aeabi_dcmpeq

ARM_FUNC_START	aeabi_dcmplt
	CFI_START_FUNCTION

	str lr, [sp, #-8]!        @ sp -= 8
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8
	.cfi_rel_offset lr, 0     @ lr is at sp

	ARM_CALL aeabi_cdcmple
	do_it	cc, e
	movcc	r0, #1	@ Less than.
	movcs	r0, #0	@ Equal to, greater than, or unordered.
	RETLDM

	CFI_END_FUNCTION
	FUNC_END aeabi_dcmplt

ARM_FUNC_START	aeabi_dcmple
	CFI_START_FUNCTION

	str lr, [sp, #-8]!        @ sp -= 8
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8
	.cfi_rel_offset lr, 0     @ lr is at sp

	ARM_CALL aeabi_cdcmple
	do_it	ls, e
	movls	r0, #1  @ Less than or equal to.
	movhi	r0, #0	@ Greater than or unordered.
	RETLDM

	CFI_END_FUNCTION
	FUNC_END aeabi_dcmple

ARM_FUNC_START	aeabi_dcmpge
	CFI_START_FUNCTION

	str lr, [sp, #-8]!        @ sp -= 8
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8
	.cfi_rel_offset lr, 0     @ lr is at sp

	ARM_CALL aeabi_cdrcmple
	do_it	ls, e
	movls	r0, #1	@ Operand 2 is less than or equal to operand 1.
	movhi	r0, #0	@ Operand 2 greater than operand 1, or unordered.
	RETLDM

	CFI_END_FUNCTION
	FUNC_END aeabi_dcmpge

ARM_FUNC_START	aeabi_dcmpgt
	CFI_START_FUNCTION

	str lr, [sp, #-8]!        @ sp -= 8
	.cfi_adjust_cfa_offset 8  @ CFA is now sp + previousOffset + 8
	.cfi_rel_offset lr, 0     @ lr is at sp

	ARM_CALL aeabi_cdrcmple
	do_it	cc, e
	movcc	r0, #1	@ Operand 2 is less than operand 1.
	movcs	r0, #0  @ Operand 2 is greater than or equal to operand 1,
			@ or they are unordered.
	RETLDM

	CFI_END_FUNCTION
	FUNC_END aeabi_dcmpgt

#endif /* L_cmpdf2 */

#ifdef L_arm_unorddf2

ARM_FUNC_START unorddf2
ARM_FUNC_ALIAS aeabi_dcmpun unorddf2
	.cfi_startproc

	mov	ip, xh, lsl #1
	mvns	ip, ip, asr #21
	bne	1f
	orrs	ip, xl, xh, lsl #12
	bne	3f			@ x is NAN
1:	mov	ip, yh, lsl #1
	mvns	ip, ip, asr #21
	bne	2f
	orrs	ip, yl, yh, lsl #12
	bne	3f			@ y is NAN
2:	mov	r0, #0			@ arguments are ordered.
	RET

3:	mov	r0, #1			@ arguments are unordered.
	RET

	.cfi_endproc
	FUNC_END aeabi_dcmpun
	FUNC_END unorddf2

#endif /* L_unorddf2 */

#ifdef L_arm_fixdfsi

ARM_FUNC_START fixdfsi
ARM_FUNC_ALIAS aeabi_d2iz fixdfsi
	CFI_START_FUNCTION

	@ check exponent range.
	mov	r2, xh, lsl #1
	adds	r2, r2, #(1 << 21)
	bcs	2f			@ value is INF or NAN
	bpl	1f			@ value is too small
	mov	r3, #(0xfffffc00 + 31)
	subs	r2, r3, r2, asr #21
	bls	3f			@ value is too large

	@ scale value
	mov	r3, xh, lsl #11
	orr	r3, r3, #0x80000000
	orr	r3, r3, xl, lsr #21
	tst	xh, #0x80000000		@ the sign bit
	shift1	lsr, r0, r3, r2
	do_it	ne
	rsbne	r0, r0, #0
	RET

1:	mov	r0, #0
	RET

2:	orrs	xl, xl, xh, lsl #12
	bne	4f			@ x is NAN.
3:	ands	r0, xh, #0x80000000	@ the sign bit
	do_it	eq
	moveq	r0, #0x7fffffff		@ maximum signed positive si
	RET

4:	mov	r0, #0			@ How should we convert NAN?
	RET

	CFI_END_FUNCTION
	FUNC_END aeabi_d2iz
	FUNC_END fixdfsi

#endif /* L_fixdfsi */

#ifdef L_arm_fixunsdfsi

ARM_FUNC_START fixunsdfsi
ARM_FUNC_ALIAS aeabi_d2uiz fixunsdfsi
	CFI_START_FUNCTION

	@ check exponent range.
	movs	r2, xh, lsl #1
	bcs	1f			@ value is negative
	adds	r2, r2, #(1 << 21)
	bcs	2f			@ value is INF or NAN
	bpl	1f			@ value is too small
	mov	r3, #(0xfffffc00 + 31)
	subs	r2, r3, r2, asr #21
	bmi	3f			@ value is too large

	@ scale value
	mov	r3, xh, lsl #11
	orr	r3, r3, #0x80000000
	orr	r3, r3, xl, lsr #21
	shift1	lsr, r0, r3, r2
	RET

1:	mov	r0, #0
	RET

2:	orrs	xl, xl, xh, lsl #12
	bne	4f			@ value is NAN.
3:	mov	r0, #0xffffffff		@ maximum unsigned si
	RET

4:	mov	r0, #0			@ How should we convert NAN?
	RET

	CFI_END_FUNCTION
	FUNC_END aeabi_d2uiz
	FUNC_END fixunsdfsi

#endif /* L_fixunsdfsi */

#ifdef L_arm_truncdfsf2

ARM_FUNC_START truncdfsf2
ARM_FUNC_ALIAS aeabi_d2f truncdfsf2
	CFI_START_FUNCTION

	@ check exponent range.
	mov	r2, xh, lsl #1
	subs	r3, r2, #((1023 - 127) << 21)
	do_it	cs, t
	COND(sub,s,cs)	ip, r3, #(1 << 21)
	COND(rsb,s,cs)	ip, ip, #(254 << 21)
	bls	2f			@ value is out of range

1:	@ shift and round mantissa
	and	ip, xh, #0x80000000
	mov	r2, xl, lsl #3
	orr	xl, ip, xl, lsr #29
	cmp	r2, #0x80000000
	adc	r0, xl, r3, lsl #2
	do_it	eq
	biceq	r0, r0, #1
	RET

2:	@ either overflow or underflow
	tst	xh, #0x40000000
	bne	3f			@ overflow

	@ check if denormalized value is possible
	adds	r2, r3, #(23 << 21)
	do_it	lt, t
	andlt	r0, xh, #0x80000000	@ too small, return signed 0.
	RETc(lt)

	@ denormalize value so we can resume with the code above afterwards.
	orr	xh, xh, #0x00100000
	mov	r2, r2, lsr #21
	rsb	r2, r2, #24
	rsb	ip, r2, #32
#if defined(__thumb2__)
	lsls	r3, xl, ip
#else
	movs	r3, xl, lsl ip
#endif
	shift1	lsr, xl, xl, r2
	do_it	ne
	orrne	xl, xl, #1		@ fold r3 for rounding considerations. 
	mov	r3, xh, lsl #11
	mov	r3, r3, lsr #11
	shiftop orr xl xl r3 lsl ip ip
	shift1	lsr, r3, r3, r2
	mov	r3, r3, lsl #1
	b	1b

3:	@ chech for NAN
	mvns	r3, r2, asr #21
	bne	5f			@ simple overflow
	orrs	r3, xl, xh, lsl #12
	do_it	ne, tt
	movne	r0, #0x7f000000
	orrne	r0, r0, #0x00c00000
	RETc(ne)			@ return NAN

5:	@ return INF with sign
	and	r0, xh, #0x80000000
	orr	r0, r0, #0x7f000000
	orr	r0, r0, #0x00800000
	RET

	CFI_END_FUNCTION
	FUNC_END aeabi_d2f
	FUNC_END truncdfsf2

#endif /* L_truncdfsf2 */
