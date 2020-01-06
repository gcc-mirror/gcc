/* Copyright (C) 2008-2020 Free Software Foundation, Inc.
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
	.global __adddf3
	.balign 4
__adddf3:
	push_s blink
	push_s r2
	push_s r3
	push_s r0
	bl.d __adddf3_c
	push_s r1
	ld_s r2,[sp,12]
	ld_s r3,[sp,8]
	st_s r0,[sp,12]
	st_s r1,[sp,8]
	pop_s r1
	bl.d __adddf3_asm
	pop_s r0
	pop_s r3
	pop_s r2
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	bl abort
	.global __subdf3
	.balign 4
__subdf3:
	push_s blink
	push_s r2
	push_s r3
	push_s r0
	bl.d __subdf3_c
	push_s r1
	ld_s r2,[sp,12]
	ld_s r3,[sp,8]
	st_s r0,[sp,12]
	st_s r1,[sp,8]
	pop_s r1
	bl.d __subdf3_asm
	pop_s r0
	pop_s r3
	pop_s r2
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	bl abort
#define __adddf3 __adddf3_asm
#define __subdf3 __subdf3_asm
#endif /* DEBUG */
/* N.B. This is optimized for ARC700.
  ARC600 has very different scheduling / instruction selection criteria.  */

/* inputs: DBL0, DBL1 (r0-r3)
   output: DBL0 (r0, r1)
   clobber: r2-r10, r12, flags
   All NaN highword bits must be 1.  NaN low word is random.  */

	.balign 4
	.global __adddf3
	.global __subdf3
	.long 0x7ff00000 ; exponent mask
	FUNC(__adddf3)
	FUNC(__subdf3)
__subdf3:
	bxor_l DBL1H,DBL1H,31
__adddf3:
	ld r9,[pcl,-8]
	bmsk r4,DBL0H,30
	xor r10,DBL0H,DBL1H
	and r6,DBL1H,r9
	sub.f r12,r4,r6
	asr_s r12,r12,20
	blo .Ldbl1_gt
	brhs r4,r9,.Linf_nan
	brhs r12,32,.Large_shift
	brne r12,0,.Lsmall_shift
	brge r10,0,.Ladd_same_exp ; r12 == 0

/* After subtracting, we need to normalize; when shifting to place the
  leading 1 into position for the implicit 1 and adding that to DBL0H,
  we increment the exponent.  Thus, we have to subtract one more than
  the shift count from the exponent beforehand.  Iff the exponent drops thus
  below zero (before adding in the fraction with the leading one), we have
  generated a denormal number.  Denormal handling is basicallly reducing the
  shift count so that we produce a zero exponent instead; however, this way
  the shift count can become zero (if we started out with exponent 1).
  Therefore, a simple min operation is not good enough, since we don't
  want to handle a zero normalizing shift in the main path.
  On the plus side, we don't need to check for denorm input, the result
  of subtracing these looks just the same as denormals generated during
  subtraction.  */
	bmsk r7,DBL1H,30
	cmp r4,r7
	cmp.eq DBL0L,DBL1L
	blo .L_rsub_same_exp
	sub.f DBL0L,DBL0L,DBL1L
	bmsk r12,DBL0H,19
	bic DBL1H,DBL0H,r12
	sbc.f r4,r4,r7
	beq_l .Large_cancel
	norm DBL1L,r4
	b.d .Lsub_done_same_exp
	sub r12,DBL1L,9

	.balign 4
.Linf_nan:
	; If both inputs are inf, but with different signs, the result is NaN.
	asr r12,r10,31
	or_s DBL1H,DBL1H,r12
	j_s.d [blink]
	or.eq DBL0H,DBL0H,DBL1H

	.balign 4
.L_rsub_same_exp:
	rsub.f DBL0L,DBL0L,DBL1L
	bmsk r12,DBL1H,19
	bic_s DBL1H,DBL1H,r12
	sbc.f r4,r7,r4
	beq_l .Large_cancel
	norm DBL1L,r4

	sub r12,DBL1L,9
.Lsub_done_same_exp:
	asl_s r12,r12,20
	sub_s DBL1L,DBL1L,10
	sub DBL0H,DBL1H,r12
	xor.f 0,DBL0H,DBL1H
	bmi .Ldenorm
.Lpast_denorm:
	neg_s r12,DBL1L
	lsr r7,DBL0L,r12
	asl r12,r4,DBL1L
	asl_s DBL0L,DBL0L,DBL1L
	add_s r12,r12,r7
	j_s.d [blink]
	add_l DBL0H,DBL0H,r12
	.balign 4
.Ladd_same_exp:
	/* This is a special case because we can't test for need to shift
	   down by checking if bit 20 of DBL0H changes.  OTOH, here we know
	   that we always need to shift down.  */
	; The implicit 1 of DBL0 is not shifted together with the
	;  fraction, thus effectively doubled, compensating for not setting
	;  implicit1 for DBL1
	add_s r12,DBL0L,DBL1L
	lsr.f 0,r12,2 ; round to even
	breq r6,0,.Ldenorm_add
	adc.f DBL0L,DBL0L,DBL1L
	sub r7,DBL1H,DBL0H
	sub1 r7,r7,r9 ; boost exponent by 2/2
	rrc DBL0L,DBL0L
	asr.f r7,r7 ; DBL1.fraction/2 - DBL0.fraction/2 ; exp++
	add.cs.f DBL0L,DBL0L,0x80000000
	add_l DBL0H,DBL0H,r7 ; DBL0.implicit1 not shifted for DBL1.implicit1
	add.cs DBL0H,DBL0H,1
	bic.f 0,r9,DBL0H ; check for overflow -> infinity.
	jne_l [blink]
	and DBL0H,DBL0H,0xfff00000
	j_s.d [blink]
	mov_s DBL0L,0
	.balign 4
.Large_shift:
	brhs r12,55,.Lret_dbl0
	bmsk_s DBL1H,DBL1H,19
	brne r6,0,.Lno_denorm_large_shift
	brhi.d r12,33,.Lfixed_denorm_large_shift
	sub_s r12,r12,1
	breq r12,31, .Lfixed_denorm_small_shift
.Lshift32:
	mov_s r12,DBL1L
	mov_s DBL1L,DBL1H
	brlt.d r10,0,.Lsub
	mov_s DBL1H,0
	b_s .Ladd
.Ldenorm_add:
	cmp_s r12,DBL1L
	mov_s DBL0L,r12
	j_s.d [blink]
	adc DBL0H,r4,DBL1H

.Lret_dbl0:
	j_s [blink]
	.balign 4
.Lsmall_shift:
	breq.d r6,0,.Ldenorm_small_shift
	bmsk_s DBL1H,DBL1H,19
	bset_s DBL1H,DBL1H,20
.Lfixed_denorm_small_shift:
	neg r8,r12
	asl r4,DBL1H,r8
	lsr_l DBL1H,DBL1H,r12
	lsr r5,DBL1L,r12
	asl r12,DBL1L,r8
	brge.d r10,0,.Ladd
	or DBL1L,r4,r5
/* subtract, abs(DBL0) > abs(DBL1) */
/* DBL0H, DBL0L: original values
   DBL1H, DBL1L: fraction with explicit leading 1, shifted into place
   r4:  orig. DBL0H & 0x7fffffff
   r6:  orig. DBL1H & 0x7ff00000
   r9:  0x7ff00000
   r10: orig. DBL0H ^ DBL1H
   r12: guard bits */
	.balign 4
.Lsub:
	neg.f r12,r12
	mov_s r7,DBL1H
	bmsk r5,DBL0H,19
	sbc.f DBL0L,DBL0L,DBL1L
	bic DBL1H,DBL0H,r5
	bset r5,r5,20
	sbc.f r4,r5,r7
	beq_l .Large_cancel_sub
	norm DBL1L,r4
	bmsk r6,DBL1H,30
.Lsub_done:
	sub_s DBL1L,DBL1L,9
	breq DBL1L,1,.Lsub_done_noshift
	asl r5,DBL1L,20
	sub_s DBL1L,DBL1L,1
	brlo r6,r5,.Ldenorm_sub
	sub DBL0H,DBL1H,r5
.Lpast_denorm_sub:
	neg_s DBL1H,DBL1L
	lsr r6,r12,DBL1H
	asl_s r12,r12,DBL1L
	and r8,r6,1
	add1.f 0,r8,r12
	add.ne.f r12,r12,r12
	asl r8,DBL0L,DBL1L
	lsr r12,DBL0L,DBL1H
	adc.f DBL0L,r8,r6
	asl r5,r4,DBL1L
	add_s DBL0H,DBL0H,r12
	j_s.d [blink]
	adc DBL0H,DBL0H,r5

	.balign 4
.Lno_denorm_large_shift:
	breq.d r12,32,.Lshift32
	bset_l DBL1H,DBL1H,20
.Lfixed_denorm_large_shift:
	neg r8,r12
	asl r4,DBL1H,r8
	lsr r5,DBL1L,r12
	asl.f 0,DBL1L,r8
	lsr DBL1L,DBL1H,r12
	or r12,r4,r5
	tst.eq r12,1
	or.ne r12,r12,2
	brlt.d r10,0,.Lsub
	mov_s DBL1H,0
	b_l .Ladd

	; If a denorm is produced without shifting, we have an exact result -
	; no need for rounding.
	.balign 4
.Ldenorm_sub:
	lsr DBL1L,r6,20
	xor DBL0H,r6,DBL1H
	brne.d DBL1L,1,.Lpast_denorm_sub
	sub_s DBL1L,DBL1L,1
.Lsub_done_noshift:
	add.f 0,r12,r12
	btst.eq DBL0L,0
	cmp.eq r12,r12
	add.cs.f DBL0L,DBL0L,1
	bclr r4,r4,20
	j_s.d [blink]
	adc DBL0H,DBL1H,r4

	.balign 4
.Ldenorm_small_shift:
	brne.d r12,1,.Lfixed_denorm_small_shift
	sub_l r12,r12,1
	brlt r10,0,.Lsub
.Ladd: ; bit 20 of DBL1H is clear and bit 0 of r12 does not matter
	add.f DBL0L,DBL0L,DBL1L
	add_s DBL1H,DBL1H,DBL0H
	add.cs DBL1H,DBL1H,1
	xor_l DBL0H,DBL0H,DBL1H
	bbit0 DBL0H,20,.Lno_shiftdown
	lsr.f DBL0H,DBL1H
	and r4,DBL0L,2
	bmsk DBL0H,DBL0H,18
	sbc DBL0H,DBL1H,DBL0H
	rrc.f DBL0L,DBL0L
	or.f r12,r12,r4
	cmp.eq r12,r12
	add.cs.f DBL0L,DBL0L,1
	bic.f 0,r9,DBL0H ; check for generating infinity with possible ...
	jne.d [blink]    ; ... non-zero fraction
	add.cs DBL0H,DBL0H,1
	mov_s DBL0L,0
	bmsk DBL1H,DBL0H,19
	j_s.d [blink]
	bic_s DBL0H,DBL0H,DBL1H
.Lno_shiftdown:
	mov_s DBL0H,DBL1H
	add.f 0,r12,r12
	btst.eq DBL0L,0
	cmp.eq r12,r12
	add.cs.f DBL0L,DBL0L,1
	j_s.d [blink]
	add.cs DBL0H,DBL0H,1
	.balign 4
.Ldenorm:
	bmsk DBL0H,DBL1H,30
	lsr r12,DBL0H,20
	xor_s DBL0H,DBL0H,DBL1H
	sub_l DBL1L,r12,1
	bgt .Lpast_denorm
	j_s.d [blink]
	add_l DBL0H,DBL0H,r4

	.balign 4
.Large_cancel:
	;DBL0L: mantissa DBL1H: sign & exponent
	norm.f DBL1L,DBL0L
	bmsk DBL0H,DBL1H,30
	add_s DBL1L,DBL1L,22
	mov.mi DBL1L,21
	add_s r12,DBL1L,1
	asl_s r12,r12,20
	beq_s .Lret0
	brhs.d DBL0H,r12,.Lpast_denorm_large_cancel
	sub DBL0H,DBL1H,r12
	bmsk DBL0H,DBL1H,30
	lsr r12,DBL0H,20
	xor_s DBL0H,DBL0H,DBL1H
	sub.f DBL1L,r12,1
	jle [blink]
.Lpast_denorm_large_cancel:
	rsub.f r7,DBL1L,32
	lsr r7,DBL0L,r7
	asl_s DBL0L,DBL0L,DBL1L
	mov.ls r7,DBL0L
	add_s DBL0H,DBL0H,r7
	j_s.d [blink]
	mov.ls DBL0L,0
.Lret0:
	j_s.d	[blink]
	mov_l	DBL0H,0

/* r4:DBL0L:r12 : unnormalized result fraction
   DBL1H: result sign and exponent         */
/* When seeing large cancellation, only the topmost guard bit might be set.  */
	.balign 4
.Large_cancel_sub:
	norm.f DBL1L,DBL0L
	bpnz.d 0f
	bmsk DBL0H,DBL1H,30
	mov r5,22<<20
	bne.d 1f
	mov_s DBL1L,21
	bset r5,r5,5+20
	add_s DBL1L,DBL1L,32
	brne r12,0,1f
	j_s.d	[blink]
	mov_l	DBL0H,0
	.balign 4
0:	add r5,DBL1L,23
	asl r5,r5,20
	add_s DBL1L,DBL1L,22
1:	brlo DBL0H,r5,.Ldenorm_large_cancel_sub
	sub DBL0H,DBL1H,r5
.Lpast_denorm_large_cancel_sub:
	rsub.f r7,DBL1L,32
	lsr r12,r12,r7
	lsr r7,DBL0L,r7
	asl_s DBL0L,DBL0L,DBL1L
	add.ge DBL0H,DBL0H,r7
	add_s DBL0L,DBL0L,r12
	add.lt DBL0H,DBL0H,DBL0L
	mov.eq DBL0L,r12
	j_s.d [blink]
	mov.lt DBL0L,0
	.balign 4
.Ldenorm_large_cancel_sub:
	lsr r5,DBL0H,20
	xor_s DBL0H,DBL0H,DBL1H
	brgt.d r5,1,.Lpast_denorm_large_cancel_sub
	sub DBL1L,r5,1
	j_l [blink] ; denorm, no shift -> no rounding needed.

/* r4: DBL0H & 0x7fffffff
   r6: DBL1H & 0x7ff00000
   r9: 0x7ff00000
   r10: sign difference
   r12: shift count (negative) */
	.balign 4
.Ldbl1_gt:
	brhs r6,r9,.Lret_dbl1 ; inf or NaN
	neg r8,r12
	brhs r8,32,.Large_shift_dbl0
.Lsmall_shift_dbl0:
	breq.d r6,0,.Ldenorm_small_shift_dbl0
	bmsk_s DBL0H,DBL0H,19
	bset_s DBL0H,DBL0H,20
.Lfixed_denorm_small_shift_dbl0:
	asl r4,DBL0H,r12
	lsr DBL0H,DBL0H,r8
	lsr r5,DBL0L,r8
	asl r12,DBL0L,r12
	brge.d r10,0,.Ladd_dbl1_gt
	or DBL0L,r4,r5
/* subtract, abs(DBL0) < abs(DBL1) */
/* DBL0H, DBL0L: fraction with explicit leading 1, shifted into place
   DBL1H, DBL1L: original values
   r6:  orig. DBL1H & 0x7ff00000
   r9:  0x7ff00000
   r12: guard bits */
	.balign 4
.Lrsub:
	neg.f r12,r12
	bmsk r7,DBL1H,19
	mov_s r5,DBL0H
	sbc.f DBL0L,DBL1L,DBL0L
	bic DBL1H,DBL1H,r7
	bset r7,r7,20
	sbc.f r4,r7,r5
	beq_l .Large_cancel_sub
	norm DBL1L,r4
	b_l .Lsub_done ; note: r6 is already set up.

.Lret_dbl1:
	mov_s DBL0H,DBL1H
	j_s.d [blink]
	mov_l DBL0L,DBL1L
	.balign 4
.Ldenorm_small_shift_dbl0:
	sub.f r8,r8,1
	bne.d .Lfixed_denorm_small_shift_dbl0
	add_s r12,r12,1
	brlt r10,0,.Lrsub
.Ladd_dbl1_gt: ; bit 20 of DBL0H is clear and bit 0 of r12 does not matter
	add.f DBL0L,DBL0L,DBL1L
	add_s DBL0H,DBL0H,DBL1H
	add.cs DBL0H,DBL0H,1
	xor DBL1H,DBL0H,DBL1H
	bbit0 DBL1H,20,.Lno_shiftdown_dbl1_gt
	lsr.f DBL1H,DBL0H
	and r4,DBL0L,2
	bmsk DBL1H,DBL1H,18
	sbc DBL0H,DBL0H,DBL1H
	rrc.f DBL0L,DBL0L
	or.f r12,r12,r4
	cmp.eq r12,r12
	add.cs.f DBL0L,DBL0L,1
	bic.f 0,r9,DBL0H ; check for generating infinity with possible ...
	jne.d [blink]    ; ... non-zero fraction
	add.cs DBL0H,DBL0H,1
	mov_s DBL0L,0
	bmsk DBL1H,DBL0H,19
	j_s.d [blink]
	bic_s DBL0H,DBL0H,DBL1H
.Lno_shiftdown_dbl1_gt:
	add.f 0,r12,r12
	btst.eq DBL0L,0
	cmp.eq r12,r12
	add.cs.f DBL0L,DBL0L,1
	j_s.d [blink]
	add.cs DBL0H,DBL0H,1

	.balign 4
.Large_shift_dbl0:
	brhs r8,55,.Lret_dbl1
	bmsk_s DBL0H,DBL0H,19
	brne r6,0,.Lno_denorm_large_shift_dbl0
	add_s r12,r12,1
	brne.d r8,33,.Lfixed_denorm_large_shift_dbl0
	sub r8,r8,1
	bset_s DBL0H,DBL0H,20
.Lshift32_dbl0:
	mov_s r12,DBL0L
	mov_s DBL0L,DBL0H
	brlt.d r10,0,.Lrsub
	mov_s DBL0H,0
	b_s .Ladd_dbl1_gt

	.balign 4
.Lno_denorm_large_shift_dbl0:
	breq.d r8,32,.Lshift32_dbl0
	bset_l DBL0H,DBL0H,20
.Lfixed_denorm_large_shift_dbl0:
	asl r4,DBL0H,r12
	lsr r5,DBL0L,r8
	asl.f 0,DBL0L,r12
	lsr DBL0L,DBL0H,r8
	or r12,r4,r5
	tst.eq r12,1
	or.ne r12,r12,2
	brlt.d r10,0,.Lrsub
	mov_s DBL0H,0
	b_l .Ladd_dbl1_gt
	ENDFUNC(__adddf3)
	ENDFUNC(__subdf3)
