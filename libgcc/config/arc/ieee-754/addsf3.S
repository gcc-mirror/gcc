/* Copyright (C) 2008-2024 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "arc-ieee-754.h"
#if 0 /* DEBUG */
	.global __addsf3
	FUNC(__addsf3)
	.balign 4
__addsf3:
	push_s blink
	push_s r1
	bl.d __addsf3_c
	push_s r0
	ld_s r1,[sp,4]
	st_s r0,[sp,4]
	bl.d __addsf3_asm
	pop_s r0
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__addsf3)
	.global __subsf3
	FUNC(__subsf3)
	.balign 4
__subsf3:
	push_s blink
	push_s r1
	bl.d __subsf3_c
	push_s r0
	ld_s r1,[sp,4]
	st_s r0,[sp,4]
	bl.d __subsf3_asm
	pop_s r0
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__subsf3)
#define __addsf3 __addsf3_asm
#define __subsf3 __subsf3_asm
#endif /* DEBUG */
/* N.B. This is optimized for ARC700.
  ARC600 has very different scheduling / instruction selection criteria.  */

/* inputs: r0, r1
   output: r0
   clobber: r1-r10, r12, flags  */

	.balign 4
	.global __addsf3
	.global __subsf3
	FUNC(__addsf3)
	FUNC(__subsf3)
	.long 0x7f800000 ; exponent mask
__subsf3:
	bxor_l r1,r1,31
__addsf3:
	ld r9,[pcl,-8]
	bmsk r4,r0,30
	xor r10,r0,r1
	and r6,r1,r9
	sub.f r12,r4,r6
	asr_s r12,r12,23
	blo .Ldbl1_gt
	brhs r4,r9,.Linf_nan
	brne r12,0,.Lsmall_shift
	brge r10,0,.Ladd_same_exp ; r12 == 0
/* After subtracting, we need to normalize; when shifting to place the
  leading 1 into position for the implicit 1 and adding that to DBL0,
  we increment the exponent.  Thus, we have to subtract one more than
  the shift count from the exponent beforehand.  Iff the exponent drops thus
  below zero (before adding in the fraction with the leading one), we have
  generated a denormal number.  Denormal handling is basicallly reducing the
  shift count so that we produce a zero exponent instead; FWIW, this way
  the shift count can become zero (if we started out with exponent 1).
  On the plus side, we don't need to check for denorm input, the result
  of subtracing these looks just the same as denormals generated during
  subtraction.  */
	bmsk r7,r1,30
	breq	r4,r7,.Lret0
	sub.f r5,r4,r7
	lsr r12,r4,23
	neg.cs r5,r5
	norm r3,r5
	bmsk r2,r0,22
	sub_s r3,r3,6
	min r12,r12,r3
	bic r1,r0,r2
	sub_s r3,r12,1
	asl_s r12,r12,23
	asl r2,r5,r3
	sub_s r1,r1,r12
	add_s r0,r1,r2
	j_s.d [blink]
	bxor.cs r0,r0,31
	.balign 4
.Linf_nan:
	; If both inputs are inf, but with different signs, the result is NaN.
	asr r12,r10,31
	or_s r1,r1,r12
	j_s.d [blink]
	or.eq r0,r0,r1
	.balign 4
.Ladd_same_exp:
	/* This is a special case because we can't test for need to shift
	   down by checking if bit 23 of DBL0 changes.  OTOH, here we know
	   that we always need to shift down.  */
	; adding the two floating point numbers together makes the sign
	; cancel out and apear as carry; the exponent is doubled, and the
	; fraction also in need of shifting left by one. The two implicit
	; ones of the sources make an implicit 1 of the result, again
	; non-existent in a place shifted by one.
	add.f	r0,r0,r1
	btst_s	r0,1
	breq	r6,0,.Ldenorm_add
	add.ne	r0,r0,1 ; round to even.
	rrc	r0,r0
	bmsk	r1,r9,23
	add	r0,r0,r1 ; increment exponent
	bic.f	0,r9,r0; check for overflow -> infinity.
	jne_l	[blink]
	mov_s	r0,r9
	j_s.d	[blink]
	bset.cs	r0,r0,31

.Ldenorm_add:
	j_s.d [blink]
	add r0,r4,r1

.Lret_dbl0:
        j_s [blink]

	.balign 4
.Lsmall_shift:
	brhi r12,25,.Lret_dbl0
	breq.d r6,0,.Ldenorm_small_shift
	bmsk_s r1,r1,22
	bset_s r1,r1,23
.Lfixed_denorm_small_shift:
	neg r8,r12
	asl r5,r1,r8
	brge.d r10,0,.Ladd
	lsr_l r1,r1,r12
/* subtract, abs(DBL0) > abs(DBL1) */
/* DBL0: original values
   DBL1: fraction with explicit leading 1, shifted into place
   r4:  orig. DBL0 & 0x7fffffff
   r6:  orig. DBL1 & 0x7f800000
   r9:  0x7f800000
   r10: orig. DBL0H ^ DBL1H
   r5 : guard bits */
	.balign 4
.Lsub:
	neg.f r12,r5
	bmsk r3,r0,22
	bset r5,r3,23
	sbc.f r4,r5,r1
	beq.d .Large_cancel_sub
	bic r7,r0,r3
	norm r3,r4
	bmsk r6,r7,30
.Lsub_done:
	sub_s r3,r3,6
	breq r3,1,.Lsub_done_noshift
	asl r5,r3,23
	sub_l r3,r3,1
	brlo r6,r5,.Ldenorm_sub
	sub r0,r7,r5
	neg_s r1,r3
	lsr.f r2,r12,r1
	asl_s r12,r12,r3
	btst_s	r2,0
	bmsk.eq.f r12,r12,30
	asl r5,r4,r3
	add_s r0,r0,r2
	adc.ne r0,r0,0
	j_s.d [blink]
	add_l r0,r0,r5

.Lret0:
	j_s.d	[blink]
	mov_l	r0,0

	.balign 4
.Ldenorm_small_shift:
	brne.d	r12,1,.Lfixed_denorm_small_shift
	sub_s	r12,r12,1
	brlt.d	r10,0,.Lsub
	mov_s	r5,r12 ; zero r5, and align following code
.Ladd: ; Both bit 23 of DBL1 and bit 0 of r5 are clear.
	bmsk	r2,r0,22
	add_s	r2,r2,r1
	bbit0.d	r2,23,.Lno_shiftdown
	add_s	r0,r0,r1
	bic.f	0,r9,r0; check for overflow -> infinity; eq : infinity
	bmsk	r1,r2,22
	lsr.ne.f r2,r2,2; cc: even ; hi: might round down
	lsr.ne	r1,r1,1
	rcmp.hi	r5,1; hi : round down
	bclr.hi	r0,r0,0
	j_l.d	[blink]
	sub_s	r0,r0,r1

/* r4: DBL0H & 0x7fffffff
   r6: DBL1H & 0x7f800000
   r9: 0x7f800000
   r10: sign difference
   r12: shift count (negative) */
	.balign 4
.Ldbl1_gt:
	brhs r6,r9,.Lret_dbl1 ; inf or NaN
	neg r8,r12
	brhi r8,25,.Lret_dbl1
.Lsmall_shift_dbl0:
	breq.d r6,0,.Ldenorm_small_shift_dbl0
	bmsk_s r0,r0,22
	bset_s r0,r0,23
.Lfixed_denorm_small_shift_dbl0:
	asl r5,r0,r12
	brge.d r10,0,.Ladd_dbl1_gt
	lsr r0,r0,r8
/* subtract, abs(DBL0) < abs(DBL1) */
/* DBL0: fraction with explicit leading 1, shifted into place
   DBL1: original value
   r6:  orig. DBL1 & 0x7f800000
   r9:  0x7f800000
   r5: guard bits */
	.balign 4
.Lrsub:
	neg.f r12,r5
	bmsk r5,r1,22
	bic r7,r1,r5
	bset r5,r5,23
	sbc.f r4,r5,r0
	bne.d .Lsub_done ; note: r6 is already set up.
	norm r3,r4
	/* Fall through */

/* r4:r12 : unnormalized result fraction
   r7: result sign and exponent         */
/* When seeing large cancellation, only the topmost guard bit might be set.  */
	.balign 4
.Large_cancel_sub:
	breq_s	r12,0,.Lret0
	sub	r0,r7,24<<23
	xor.f	0,r0,r7 ; test if exponent is negative
	tst.pl	r9,r0  ; test if exponent is zero
	jpnz	[blink] ; return if non-denormal result
	bmsk	r6,r7,30
	lsr	r3,r6,23
	xor	r0,r6,r7
	sub_s	r3,r3,24-22
	j_s.d	[blink]
	bset	r0,r0,r3

	; If a denorm is produced, we have an exact result -
	; no need for rounding.
	.balign 4
.Ldenorm_sub:
	sub r3,r6,1
	lsr.f r3,r3,23
	xor r0,r6,r7
	neg_s r1,r3
	asl.ne r4,r4,r3
	lsr_s r12,r12,r1
	add_s r0,r0,r4
	j_s.d [blink]
	add.ne r0,r0,r12

	.balign 4
.Lsub_done_noshift:
	add.f 0,r12,r12
	btst.eq r4,0
	bclr r4,r4,23
	add r0,r7,r4
	j_s.d [blink]
	adc.ne r0,r0,0

	.balign 4
.Lno_shiftdown:
	add.f 0,r5,r5
	btst.eq r0,0
	cmp.eq r5,r5
	j_s.d [blink]
	add.cs r0,r0,1

.Lret_dbl1:
	j_s.d [blink]
	mov_l r0,r1
	.balign 4
.Ldenorm_small_shift_dbl0:
	sub.f r8,r8,1
	bne.d .Lfixed_denorm_small_shift_dbl0
	add_s r12,r12,1
	brlt.d r10,0,.Lrsub
	mov r5,0
.Ladd_dbl1_gt: ; both bit 23 of DBL0 and bit 0 of r5 are clear.
	bmsk	r2,r1,22
	add_s	r2,r2,r0
	bbit0.d	r2,23,.Lno_shiftdown_dbl1_gt
	add_s	r0,r1,r0
	bic.f	0,r9,r0; check for overflow -> infinity; eq : infinity
	bmsk	r1,r2,22
	lsr.ne.f r2,r2,2; cc: even ; hi: might round down
	lsr.ne	r1,r1,1
	rcmp.hi	r5,1; hi : round down
	bclr.hi	r0,r0,0
	j_l.d	[blink]
	sub_s	r0,r0,r1

	.balign	4
.Lno_shiftdown_dbl1_gt:
	add.f	0,r5,r5
	btst.eq	r0,0
	cmp.eq	r5,r5
	j_s.d	[blink]
	add.cs	r0,r0,1
	ENDFUNC(__addsf3)
	ENDFUNC(__subsf3)
